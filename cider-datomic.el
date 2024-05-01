;;; cider-datomic.el --- Cider Datomic Mode -*- lexical-binding: t -*-

;; Copyright © 2023-2024 Bozhidar Batsov and CIDER contributors

;; Author: r0man <roman@burningswell.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Log inspection functionality for Clojure.
;;
;; Please, refer to the online documentation for more details
;; https://docs.cider.mx/cider/debugging/logging.html.

;;; Code:

(require 'cider-client)
(require 'cl-lib)
(require 'eieio)
(require 'ht)
(require 'nrepl-dict)
(require 'seq)
(require 'transient)

(defcustom cider-datomic-client-server-type "datomic-local"
  "The Datomic client server type."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-storage-dir "mem"
  "The Datomic client storage directory."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-system "default"
  "The Datomic client system."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-region nil
  "The Datomic client AWS region."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-endpoint nil
  "The Datomic client IP address of the system or query group."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-access-key nil
  "The Datomic client access key from peer server launch."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defcustom cider-datomic-client-validate-hostnames nil
  "Whether to validate Datomic client hostnames."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'stringp
  :type 'string)

(defvar cider-datomic-clients
  (list (cider-datomic-client))
  "The list of Datomic clients.")

(defvar cider-datomic-client-server-types
  '("cloud" "datomic-local" "peer-server")
  "The list of Datomic query content types.")

;; Classes

(defclass cider-datomic-client ()
  ((server-type
    :documentation "The coding system to use for the media type."
    :initarg :server-type
    :initform "datomic-local"
    :type string)
   (storage-dir
    :documentation "The coding system to use for the media type."
    :initarg :storage-dir
    :initform "mem"
    :type string)
   (system
    :documentation "The system to use for the media type."
    :initarg :system
    :initform "default"
    :type (or null string)))
  "A class representing a Datomic client.")

;; Attribute

(defclass cider-datomic-ident-object ()
  ((ident
    :accessor cider-datomic-ident
    :documentation "The Datomic identifier."
    :initarg :ident
    :type symbol)))

(defclass cider-datomic-attribute (cider-datomic-ident-object)
  ((cardinality
    :accessor cider-datomic-attribute-cardinality
    :documentation "The cardinality of the attribute."
    :initarg :cardinality
    :type symbol)
   (doc
    :accessor cider-datomic-attribute-doc
    :documentation "The documentation of the attribute."
    :initarg :doc
    :type string)
   (type
    :accessor cider-datomic-attribute-type
    :documentation "The data type of the attribute."
    :initarg :type
    :type symbol)
   (unique
    :accessor cider-datomic-attribute-unique
    :documentation "The data unique of the attribute."
    :initarg :unique
    :unique symbol)))

(defun cider-datomic-attribute-name (attribute)
  "Return the name of the ATTRIBUTE."
  (with-slots (ident) attribute
    (format "%s" ident)))

(defun cider-datomic-attribute-namespace (attribute)
  "Return the namespace of the ATTRIBUTE."
  (with-slots (ident) attribute
    (seq-first (cider-datomic--split-symbol ident))))

;; Schema

(defclass cider-datomic-schema ()
  ((attributes
    :accessor cider-datomic-schema-attributes
    :documentation "The attributes of the schema."
    :initarg :attributes)
   (namespaces
    :accessor cider-datomic-schema-namespaces
    :documentation "The namespaces of the schema."
    :initarg :namespaces)))

(defun cider-datomic-schema--result-to-attribute (result)
  "Convert the a Datomic query RESULT into a schema."
  (seq-let [ident type cardinality doc unique] result
    (cider-datomic-attribute
     :cardinality cardinality
     :doc doc
     :ident ident
     :type type
     :unique (when (hash-table-p unique)
               (ht-get* unique :db/unique :db/ident)))))

(defun cider-datomic-schema--results-to-schema (results)
  "Convert the a Datomic query RESULTS into a schema."
  (let ((attributes (seq-map #'cider-datomic-schema--result-to-attribute (car results))))
    (cider-datomic-schema :attributes attributes)))

(defvar cider-datomic-schema--load-query
  (vector :find '\?ident '\?type '\?cardinality '\?doc
          `(pull \?a ,(vector (ht (:db/unique [:db/ident]))))
          :where
          [_ :db.install/attribute \?a]
          [\?a :db/valueType \?t]
          [\?a :db/cardinality \?c]
          [\?a :db/ident \?ident]
          [\?t :db/ident \?type]
          [\?c :db/ident \?cardinality]
          [\?ident :db/doc \?doc])
  "The Datomic query to load the schema.")

;; Helper

(defun cider-datomic--split-symbol (symbol)
  "Split the SYMBOL into a list with NAMESPACE and NAME, or NIL."
  (seq-let [namespace name] (split-string (format "%s" symbol) "/" t)
    (cond ((and namespace (null name))
           (list nil (string-trim-left namespace ":")))
          ((and namespace name (equal ":" (substring namespace 0 1)))
           (list (string-trim-left namespace ":") name))
          ((and namespace name)
           (list namespace name)))))

;; NREPL

(defun cider-request:datomic-create-database (client db-name &optional callback)
  "Create the Datomic database DB-NAME using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/create-database")
  (thread-first `("op" "cider.datomic/create-database"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client)
                  "cider.datomic/db-name" ,db-name)
                (cider-nrepl-send-request callback)))

(defun cider-request:datomic-delete-database (client db-name &optional callback)
  "Delete the Datomic database DB-NAME using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/delete-database")
  (thread-first `("op" "cider.datomic/delete-database"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client)
                  "cider.datomic/db-name" ,db-name)
                (cider-nrepl-send-request callback)))

(defun cider-request:datomic-list-databases (client &optional callback)
  "List the Datomic database using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/list-databases")
  (thread-first `("op" "cider.datomic/list-databases"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client))
                (cider-nrepl-send-request callback)))

(defun cider-request:datomic-query (client db-name query &optional callback)
  "Evaluate the Datomic QUERY with DB-NAME using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/query")
  (thread-first `("op" "cider.datomic/query"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client)
                  "cider.datomic/db-name" ,db-name
                  "cider.datomic/query" ,(if (stringp query)
                                             query
                                           (parseedn-print-str query)))
                (cider-nrepl-send-request callback)))

(defun cider-request:datomic-transact (client db-name tx-data &optional callback)
  "Submit TX-DATA to DB-NAME using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/transact")
  (thread-first `("op" "cider.datomic/transact"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client)
                  "cider.datomic/db-name" ,db-name
                  "cider.datomic/transact" ,(if (stringp tx-data)
                                                tx-data
                                              (parseedn-print-str tx-data)))
                (cider-nrepl-send-request callback)))

(defun cider-sync-request:datomic-list-databases (client)
  "List the Datomic database using CLIENT and invoke CALLBACK."
  (cider-ensure-op-supported "cider.datomic/list-databases")
  (thread-first `("op" "cider.datomic/list-databases"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "cider.datomic/list-databases")))

(defvar cider-datomic-client--current
  (cider-datomic-client)
  "The current Datomic client.")

(defun cider-datomic-client-current ()
  "Return the current Datomic client."
  cider-datomic-client--current)

(defun cider-datomic--database-under-point ()
  "Return the Datomic database under point."
  (let ((entry (tabulated-list-get-id)))
    (when (stringp entry)
      entry)))

(defun cider-datomic-create-database (client name)
  "Create a database for CLIENT with NAME."
  (interactive (list (cider-datomic-client-current) nil))
  (unless name (setq name (cider-datomic-read-database client)))
  (cider-request:datomic-create-database
   client name (lambda (response)
                 (nrepl-dbind-response response (status)
                   (cond ((member "done" status)
                          (cider-datomic-list-databases client)
                          (message "Database %s created."
                                   (propertize name 'face 'bold))))))))

(defun cider-datomic-delete-database (client name)
  "Delete the database NAME using CLIENT."
  (interactive (list (cider-datomic-client-current)
                     (cider-datomic--database-under-point)))
  (unless name (setq name (cider-datomic-read-database client)))
  (when (yes-or-no-p (format "Delete database %s? " name))
    (cider-request:datomic-delete-database
     client name (lambda (response)
                   (nrepl-dbind-response response (status)
                     (cond ((member "done" status)
                            (cider-datomic-list-databases client)
                            (message "Database %s deleted."
                                     (propertize name 'face 'bold)))))))))

(defun cider-datomic-read-client-storage-directory ()
  "Read the storage directory of a Datomic client."
  (read-string "Storage directory: " cider-datomic-client-storage-dir
               'cider-datomic-client-storage-directory-history))

(defun cider-datomic-read-client-server-type ()
  "Read the server type of a Datomic client."
  (completing-read "Server type: " cider-datomic-client-server-types nil t
                   cider-datomic-client-server-type
                   'cider-datomic-client-server-type-history))

(defun cider-datomic-read-client-system ()
  "Read the system of a Datomic client."
  (read-string "System: " cider-datomic-client-system
               'cider-datomic-client-system-history))

(defun cider-datomic-read-client ()
  "Read a Datomic client."
  (cider-datomic-client
   :server-type (cider-datomic-read-client-server-type)
   :storage-dir (cider-datomic-read-client-storage-directory)
   :system (cider-datomic-read-client-system)))

(defun cider-datomic-read-database (client)
  "Read a Datomic database with CLIENT."
  (let ((databases (cider-sync-request:datomic-list-databases client)))
    (completing-read "Database name: " databases)))

;; List clients

(defvar cider-datomic-client-tabulated-list-format
  [("System" 12 t)
   ("Server Type" 16 t)
   ("Storage Directory" 20 nil)]
  "The tabulated list format of a Datomic client.")

(defun cider-datomic-client-tabulated-list-entry (client)
  "Return a tabulated list entry for CLIENT."
  (with-slots (server-type storage-dir system) client
    (list client (vector (format "%s" system)
                         (format "%s" server-type)
                         (format "%s" storage-dir)))))

(defun cider-datomic-client-enter ()
  "List the databases of the seletcted Datomic client."
  (interactive)
  (let ((client (tabulated-list-get-id)))
    (setq cider-datomic-client--current client)
    (cider-datomic-list-databases client)))

(defun cider-datomic-create-client ()
  "Create a new Datomic client."
  (interactive)
  (push (cider-datomic-read-client) cider-datomic-clients)
  (cider-datomic-list-clients))

(defun cider-datomic-delete-client ()
  "Create a new Datomic client."
  (interactive)
  (let ((client (tabulated-list-get-id)))
    (when (and (cl-typep client 'cider-datomic-client)
               (yes-or-no-p "Delete client? "))
      (setq cider-datomic-clients (seq-remove (lambda (c) (eq c client))
                                              cider-datomic-clients))
      (cider-datomic-list-clients))))

(defvar cider-datomic-clients-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'cider-datomic-client-enter)
    (define-key map (kbd "c") #'cider-datomic-create-client)
    (define-key map (kbd "d") #'cider-datomic-delete-client)
    (define-key map (kbd "g") #'cider-datomic-list-clients)
    map)
  "The Cider Datomic clients mode key map.")

(define-derived-mode cider-datomic-clients-mode
  tabulated-list-mode "Datomic Clients"
  "Major mode to list Datomic clients."
  (setq tabulated-list-format cider-datomic-client-tabulated-list-format)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun cider-datomic-list-clients ()
  "List Datomic clients."
  (interactive)
  (pop-to-buffer-same-window "*Datomic Clients*")
  (cider-datomic-clients-mode)
  (setq tabulated-list-entries
        (seq-map #'cider-datomic-client-tabulated-list-entry
                 cider-datomic-clients))
  (tabulated-list-print t))

;; List databases

(defvar cider-datomic-database-tabulated-list-format
  [("Name" 12 t)
   ("System" 12 t)
   ("Server Type" 16 t)
   ("Storage Directory" 20 nil)]
  "The tabulated list format of a Datomic database.")

(defun cider-datomic-database-tabulated-list-entry (client database)
  "Return a tabulated list entry for CLIENT and DATABASE."
  (with-slots (server-type storage-dir system) client
    (list database (vector (format "%s" database)
                           (format "%s" system)
                           (format "%s" server-type)
                           (format "%s" storage-dir)))))

(defvar cider-datomic-databases-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") #'cider-datomic-create-database)
    (define-key map (kbd "d") #'cider-datomic-delete-database)
    (define-key map (kbd "g") #'cider-datomic-list-databases)
    (define-key map (kbd "l") #'cider-datomic-list-clients)
    map)
  "The Cider Datomic databases mode key map.")

(define-derived-mode cider-datomic-databases-mode
  tabulated-list-mode "Datomic Databases"
  "Major mode to list Datomic databases."
  (setq tabulated-list-format cider-datomic-database-tabulated-list-format)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun cider-datomic-list-databases (client)
  "List databases for CLIENT."
  (interactive (list (cider-datomic-client-current)))
  (cider-request:datomic-list-databases
   client (lambda (response)
            (nrepl-dbind-response response (status)
              (cond ((member "done" status)
                     (pop-to-buffer-same-window "*Datomic Databases*")
                     (cider-datomic-databases-mode)
                     (setq tabulated-list-entries
                           (seq-map (lambda (database)
                                      (cider-datomic-database-tabulated-list-entry client database))
                                    (nrepl-dict-get response "cider.datomic/list-databases")))
                     (tabulated-list-print t)))))))

;; Transient

(transient-define-argument cider-datomic--client-server-type-option ()
  :argument "--server-type="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Server type"
  :key "=t")

(transient-define-argument cider-datomic--client-storage-dir-option ()
  :argument "--storage-dir="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Storage directory"
  :key "=d")

(transient-define-argument cider-datomic--client-system-option ()
  :argument "--system="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Database System"
  :key "=s")

(transient-define-argument cider-datomic--client-region-option ()
  :argument "--region="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Database Region"
  :key "=r")

(transient-define-argument cider-datomic--client-endpoint-option ()
  :argument "--endpoint="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Database Endpoint"
  :key "=e")

(transient-define-argument cider-datomic--client-access-key-option ()
  :argument "--access-key="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Database Access Key"
  :key "=a")

(transient-define-argument cider-datomic--client-validate-hostnames-option ()
  :argument "--validate-hostnames"
  :class 'transient-switch
  :description "Database Validate Hostnames"
  :key "=v")

;;;###autoload (autoload 'cider-datomic "cider-datomic" "Manage the Datomic client." t)
(transient-define-prefix cider-datomic ()
  "Show the Cider log event menu."
  :history-key 'cider-datomic-client
  ["Cider Datomic\n\nClient:"
   (cider-datomic--client-server-type-option)
   (cider-datomic--client-storage-dir-option)
   (cider-datomic--client-system-option)
   (cider-datomic--client-region-option)
   (cider-datomic--client-endpoint-option)
   (cider-datomic--client-access-key-option)
   (cider-datomic--client-validate-hostnames-option)]
  ["Actions"
   ("n" "Create new database" cider-datomic-create-database)
   ("k" "Delete database" cider-datomic-delete-database)
   ("c" "List clients" cider-datomic-list-clients)
   ("l" "List databases" cider-datomic-list-databases)])

;; Major mode

(defvar cider-datomic-mode-map
  (let ((map (make-sparse-keymap))
        (parent special-mode-map))
    (set-keymap-parent map parent)
    (define-key map (kbd "C-c M-d l") #'cider-datomic-list-databases)
    map)
  "The Cider log stream mode key map.")

(defun cider-datomic--setup-mode ()
  "Setup CIDER log mode."
  (use-local-map cider-datomic-mode-map)
  ;; (setq-local electric-indent-chars nil)
  ;; (setq-local logview-show-ellipses nil)
  (setq-local sesman-system 'CIDER)
  ;; (setq-local truncate-lines t)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'cider-datomic-mode 'emacs)))

(defvar cider-datomic--mode-doc
  "Major mode for inspecting Clojure log events.

CIDER Log Mode allows you to capture, debug, inspect and view log events
emitted by Java logging frameworks.  The captured log events can be
searched, streamed to the client, pretty printed and are integrated with
the CIDER Inspector and the CIDER stacktrace mode.

\\{cider-datomic-mode-map}")

(define-derived-mode cider-datomic-mode fundamental-mode "Cider Datomic" cider-datomic--mode-doc
  (cider-datomic--setup-mode))

(defun cider-datomic-client-transform-value (client)
  "Transform CLIENT into a list of properties."
  (with-slots (server-type storage-dir system) client
    (let ((value (nrepl-dict)))
      (when server-type
        (nrepl-dict-put value  "server-type" server-type))
      (when storage-dir
        (nrepl-dict-put value "storage-dir" storage-dir))
      (when system
        (nrepl-dict-put value "system" system))
      value)))

(provide 'cider-datomic)

;;; cider-datomic.el ends here

;; (cider-request:datomic-query
;;  (cider-datomic-client-current)
;;  "foo" cider-datomic-schema--load-query
;;  (lambda (result)
;;    (setq my-result result)))

;; (cider-sync-request:datomic-list-databases (cider-datomic-client-current))

;; (cider-datomic-read-database (cider-datomic-client-current))
