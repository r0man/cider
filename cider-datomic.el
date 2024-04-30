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
(require 'nrepl-dict)
(require 'seq)
(require 'transient)

(defcustom cider-datomic-client-server-type :datomic-local
  "The Datomic client server type."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'symbolp
  :type 'symbol)

(defcustom cider-datomic-client-storage-directory :mem
  "The Datomic client storage directory."
  :group 'cider-datomic
  :package-version '(cider . "1.13.2")
  :safe #'symbolp
  :type 'symbol)

(defcustom cider-datomic-client-system nil
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

(defvar cider-datomic-client-server-types
  '(:cloud :datomic-local :peer-server)
  "The list of Datomic query content types.")

;; Classes

(defclass cider-datomic-client ()
  ((server-type
    :documentation "The coding system to use for the media type."
    :initarg :server-type
    :initform :datomic-local
    :type symbol)
   (storage-directory
    :documentation "The coding system to use for the media type."
    :initarg :storage-directory
    :initform :mem
    :type symbol)
   (system
    :documentation "The system to use for the media type."
    :initarg :system
    :initform nil
    :type (or null string)))
  "A class representing a Datomic client.")

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
    (datomic-attribute :cardinality cardinality
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

;; NREPL

(defun cider-request:datomic-create-database (client db-name &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "cider.datomic/create-databases")
  (thread-first `("op" "cider.datomic/list-databases"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client)
                  "cider.datomic/db-name" ,db-name)
                (cider-nrepl-send-request callback)))

(defun cider-request:datomic-list-databases (client &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "cider.datomic/list-databases")
  (thread-first `("op" "cider.datomic/list-databases"
                  "cider.datomic/client" ,(cider-datomic-client-transform-value client))
                (cider-nrepl-send-request callback)))

(defun cider-datomic-client-apply ()
  (interactive))

(defun cider-datomic-current-client ()
  (cider-datomic-client :system "foo"))

(defun cider-datomic--do-create-database (client name)
  "Create a database for CLIENT with NAME."
  (interactive (list (cider-datomic-current-client)
                     (read-string "Database name: ")))
  (cider-request:datomic-create-database client name))

(define-derived-mode cider-datomic-databases-mode tabulated-list-mode "Cider Datomic Databases" "Major mode to list Datomic databases"
  (setq tabulated-list-format [("Name" 12 t)
                               ("Server Type" 16 t)
                               ("Storage Directory" 20 nil)])
  (setq tabulated-list-padding 2)
  ;; (setq tabulated-list-sort-key (cons "Date" t))
  ;; (use-local-map blog-mode-map)
  (tabulated-list-init-header))

(defun cider-datomic--list-databases-handler (client)
  "List databases for CLIENT."
  (lambda (response)
    (nrepl-dbind-response response (status)
      (cond ((member "done" status)
             (pop-to-buffer "*Cider Datomic Databases*" nil)
             (cider-datomic-databases-mode)
             (setq tabulated-list-entries
                   (seq-map (lambda (entry)
                              (with-slots (server-type storage-directory) client
                                (list (format "%s" entry)
                                      (vector
                                       (format "%s" entry)
                                       (format "%s" server-type)
                                       (format "%s" storage-directory)))))
                            (nrepl-dict-get response "cider.datomic/list-databases")))
             (tabulated-list-print t))))))

(defun cider-datomic-list-databases (client)
  "List databases for CLIENT."
  (interactive (list (cider-datomic-current-client)))
  (cider-request:datomic-list-databases client (cider-datomic--list-databases-handler client)))

;; Transient

(transient-define-argument cider-datomic--client-server-type-option ()
  :argument "--server-type="
  :choices cider-datomic-client-server-types
  :class 'transient-option
  :description "Server type"
  :key "=t")

(transient-define-argument cider-datomic--client-storage-directory-option ()
  :argument "--storage-directory="
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

;;;###autoload (autoload 'cider-datomic-client "cider-datomic" "Manage the Datomic client." t)
(transient-define-prefix cider-datomic-client-menu ()
  "Show the Cider log event menu."
  :history-key 'cider-datomic-client
  ["Cider Datomic\n\nClient:"
   (cider-datomic--client-server-type-option)
   (cider-datomic--client-storage-directory-option)
   (cider-datomic--client-system-option)
   (cider-datomic--client-region-option)
   (cider-datomic--client-endpoint-option)
   (cider-datomic--client-access-key-option)
   (cider-datomic--client-validate-hostnames-option)]
  ["Actions"
   ("c" "Create database" cider-datomic--do-create-database)
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
  (with-slots (server-type storage-directory system) client
    (let ((value (nrepl-dict)))
      (when server-type
        (nrepl-dict-put value  "server-type" (symbol-name server-type)))
      (when storage-directory
        (nrepl-dict-put value "storage-directory" (symbol-name storage-directory)))
      (when system
        (nrepl-dict-put value "system" system))
      value)))

(provide 'cider-datomic)

;;; cider-datomic.el ends here
