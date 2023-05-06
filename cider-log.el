;;; cider-log.el --- Log inspection functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2023 Nubank, Bozhidar Batsov and CIDER contributors

;; Author: Roman Scherer <roman.scherer@nubank.com.br>

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

;;; Code:

(require 'cider)
(require 'cider-inspector)
(require 'cider-stacktrace)
(require 'cl-lib)
(require 'logview)
(require 'org)
(require 'transient)

(defcustom cider-log-appender-id "cider-log"
  "The name of the default log appender."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-buffer "*cider-log*"
  "The name of the log buffer."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-event-buffer "*cider-log-event*"
  "The name of the log event buffer."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-logview-p t
  "Whether or not to use Logview."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'booleanp
  :type 'boolean)

(defvar cider-log-framework nil
  "The current log framework to use.")

(defvar cider-log-appender nil
  "The current log appender.")

(defvar cider-log-appender-size 100000
  "The size of the log appender.")

(defvar cider-log-appender-threshold 10
  "The threshold in percent of the log appender.")

(defvar-local cider-log-consumer nil
  "The current log consumer.")

(defun cider-log--bold (s)
  "Return S with a bold face."
  (when s (propertize (format "%s" s) 'face 'bold)))

(defun cider-log-buffer-clear-p (&optional buffer)
  "Return non-nil if BUFFER is not empty, otherwise nil."
  (when-let (buffer (get-buffer (or buffer cider-log-buffer)))
    (> (buffer-size buffer) 0)))

(defun cider-log--format-time (time)
  "Format TIME in ISO8601 format."
  (format-time-string "%FT%T%z" time))

(defun cider-log--strip-whitespace (s)
  "Replace multiple white space characters in S with a single one."
  (replace-regexp-in-string "[\n ]+" " " s))

(defun cider-log-appender-attached-p ()
  "Return non-nil if the log appender is attached, otherwise nil."
  (when (and cider-log-framework cider-log-appender)
    (cider-log-appender-reload cider-log-framework cider-log-appender)))

(defun cider-log-consumer-attached-p ()
  "Return non-nil if the log consumer is attached, otherwise nil."
  (when (and cider-log-framework cider-log-appender cider-log-consumer)
    (cider-log-consumer-reload cider-log-framework cider-log-appender cider-log-consumer)))

;; NREPL

(defun cider-request:log-add-consumer (framework appender consumer &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "log-add-consumer")
  (thread-first `("op" "log-add-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-consumer-filters consumer))
                (cider-nrepl-send-request callback)))

(defun cider-request:log-analyze-stacktrace (framework appender event &optional callback)
  "Analyze the EVENT stacktrace of the APPENDER of FRAMEWORK and call CALLBACK."
  (cider-ensure-op-supported "log-analyze-stacktrace")
  (thread-first `("op" "log-analyze-stacktrace"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "event-id" ,(nrepl-dict-get event "id"))
                (cider-nrepl-send-request callback)))

(defun cider-sync-request:log-update-consumer (framework appender consumer)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "log-update-consumer")
  (thread-first `("op" "log-update-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "consumer" ,(cider-log-consumer-id consumer)
                  "filters" ,(cider-log-consumer-filters consumer))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-update-consumer")))

(defun cider-sync-request:log-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-ensure-op-supported "log-add-appender")
  (thread-first `("op" "log-add-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-appender-filters appender)
                  "size" ,(cider-log-appender-size appender)
                  "threshold" ,(cider-log-appender-threshold appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-add-appender")))

(defun cider-sync-request:log-update-appender (framework appender)
  "Update the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "log-update-appender")
  (thread-first `("op" "log-update-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,(cider-log-appender-filters appender)
                  "size" ,(cider-log-appender-size appender)
                  "threshold" ,(cider-log-appender-threshold appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-update-appender")))

(defun cider-sync-request:log-clear (framework appender)
  "Clear the log events for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "log-clear-appender")
  (thread-first `("op" "log-clear-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-clear-appender")))

(defun cider-sync-request:log-inspect-event (framework appender event-id)
  "Inspect the log event with the ID in the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "log-inspect-event")
  (thread-first `("op" "log-inspect-event"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "event-id" ,event-id)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:log-format-event (framework appender event)
  "Format the log EVENT from the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "log-format-event")
  (thread-first
    (seq-mapcat #'identity
                (map-merge 'list
                           (cider--nrepl-print-request-map fill-column)
                           `(("op" "log-format-event")
                             ("framework" ,(cider-log-framework-id framework))
                             ("appender" ,(cider-log-appender-id appender))
                             ("event-id" ,(nrepl-dict-get event "id")))))
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "log-format-event")))

(defun cider-sync-request:log-frameworks ()
  "Return the available log frameworks."
  (cider-ensure-op-supported "log-frameworks")
  (thread-first `("op" "log-frameworks")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-frameworks")))

(cl-defun cider-sync-request:log-search (framework appender &key limit filters)
  "Search log events of FRAMEWORK and APPENDER using LIMIT and FILTERS."
  (cider-ensure-op-supported "log-search")
  (thread-first `("op" "log-search"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "filters" ,filters
                  "limit" ,limit)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-search")))

(defun cider-sync-request:log-exceptions (framework appender)
  "Return the Cider log exceptions for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "log-exceptions")
  (thread-first `("op" "log-exceptions"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-exceptions")))

(defun cider-sync-request:log-levels (framework appender)
  "Return the Cider log levels for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "log-levels")
  (thread-first `("op" "log-levels"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-levels")))

(defun cider-sync-request:log-loggers (framework appender)
  "Return the Cider loggers for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "log-loggers")
  (thread-first `("op" "log-loggers"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-loggers")))

(defun cider-sync-request:log-remove-appender (framework appender)
  "Remove the APPENDER from the log FRAMEWORK."
  (cider-ensure-op-supported "log-remove-appender")
  (thread-first `("op" "log-remove-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-remove-appender")))

(defun cider-sync-request:log-remove-consumer (framework appender consumer)
  "Remove the CONSUMER from the APPENDER of the log FRAMEWORK."
  (cider-ensure-op-supported "log-remove-consumer")
  (thread-first `("op" "log-remove-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender)
                  "consumer" ,(cider-log-consumer-id consumer))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-remove-consumer")))

(defun cider-sync-request:log-threads (framework appender)
  "Return the threads for FRAMEWORK and APPENDER."
  (cider-ensure-op-supported "log-threads")
  (thread-first `("op" "log-threads"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-id appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "log-threads")))

(defun cider-log--completion-extra-properties (keys &optional separator)
  "Return the `completion-extra-properties` for NREPL completions."
  `(:annotation-function
    ,(lambda (identifier)
       (when-let (dict (cadr (assoc identifier minibuffer-completion-table)))
         (let ((annotation (string-join (seq-map (lambda (key) (nrepl-dict-get dict key)) keys) (or separator " "))))
           (unless (string-blank-p annotation)
             (propertize (format " - %s" (cider-log--strip-whitespace annotation)) 'face 'font-lock-comment-face)))))))

(defun cider-log--read-appender (prompt initial-input history)
  "Read a appender from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (nrepl-dict "id" (read-string (or prompt "Log appender: ")
                                (or initial-input cider-log-appender-id)
                                history cider-log-appender-id)))

(defun cider-log--read-buffer (&optional prompt initial-input history)
  "Read the log buffer from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (read-string (or prompt "Buffer: ") (or initial-input cider-log-buffer) history cider-log-buffer))

(defun cider-log--read-exceptions (&optional prompt initial-input history)
  "Read a list of exceptions from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-exceptions
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or prompt "Exceptions: ") table nil nil initial-input history)))

(defun cider-log--read-framework (&optional prompt initial-input history)
  "Read a framework from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((completion-extra-properties (cider-log--completion-extra-properties '("name")))
        (frameworks (cider-sync-request:log-frameworks)))
    (thread-last (completing-read
                  (or prompt "Log framework: ")
                  (seq-map (lambda (framework)
                             (list (cider-log-framework-id framework) framework))
                           frameworks)
                  nil nil initial-input history)
                 (cider-log-framework-by-id frameworks))))

(defun cider-log--read-levels (&optional prompt initial-input history)
  "Read a list of levels from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-levels
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or prompt "Levels: ") table nil nil initial-input history)))

(defun cider-log--read-loggers (&optional prompt initial-input history)
  "Read a list of loggers from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-loggers
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or "Loggers: " prompt) table nil nil initial-input history)))

(defun cider-log--read-threads (&optional prompt initial-input history)
  "Read a list of threads from the minibuffer using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((table (when (cider-log-appender-attached-p)
                 (nrepl-dict-keys (cider-sync-request:log-threads
                                   cider-log-framework cider-log-appender)))))
    (completing-read-multiple (or prompt "Threads: ") table nil nil initial-input history)))

(defun cider-log--read-time (&optional prompt initial-input _)
  "Read a time from the minibuffer using PROMPT and INITIAL-INPUT."
  (cider-log--format-time (org-read-date t 'to-time nil prompt nil initial-input)))

;; Log Framework

(defun cider-log-framework-appenders (framework)
  "Return the appenders of the log FRAMEWORK."
  (nrepl-dict-get framework "appenders"))

(defun cider-log-framework-id (framework)
  "Return the id of the log FRAMEWORK."
  (nrepl-dict-get framework "id"))

(defun cider-log-framework-javadoc-url (framework)
  "Return the Javadoc URL of the log FRAMEWORK."
  (nrepl-dict-get framework "javadoc-url"))

(defun cider-log-framework-name (framework)
  "Return the name of the log FRAMEWORK."
  (nrepl-dict-get framework "name"))

(defun cider-log-framework-website-url (framework)
  "Return the website URL of the log FRAMEWORK."
  (nrepl-dict-get framework "website-url"))

(defun cider-log-framework-display-name (framework)
  "Return the display name of the log FRAMEWORK."
  (cider-log--bold (cider-log-framework-name framework)))

(defun cider-log-framework-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-sync-request:log-add-appender framework appender))

(defun cider-log-framework-appender-by-id (framework id)
  "Lookup the appender of the log FRAMEWORK by ID."
  (seq-find (lambda (appender) (equal id (cider-log-appender-id appender)))
            (cider-log-framework-appenders framework)))

(defun cider-log-framework-by-id (frameworks id)
  "Find the log framework in FRAMEWORKS by ID."
  (seq-find (lambda (framework) (equal id (cider-log-framework-id framework))) frameworks))

(defun cider-log-framework-reload (framework)
  "Reload the log FRAMEWORK."
  (cider-log-framework-by-id
   (cider-sync-request:log-frameworks)
   (cider-log-framework-id framework)))

;; Log Appender

(defun cider-log-appender-consumers (appender)
  "Return the consumers of the log APPENDER."
  (nrepl-dict-get appender "consumers"))

(defun cider-log-appender-id (appender)
  "Return the id of the log APPENDER."
  (nrepl-dict-get appender "id"))

(defun cider-log-appender-size (appender)
  "Return the size of the log APPENDER."
  (nrepl-dict-get appender "size"))

(defun cider-log-appender-threshold (appender)
  "Return the threshold of the log APPENDER."
  (nrepl-dict-get appender "threshold"))

(defun cider-log-appender-filters (appender)
  "Return the filters of the log APPENDER."
  (nrepl-dict-get appender "filters"))

(defun cider-log-appender-display-name (appender)
  "Return the display name of the log APPENDER."
  (cider-log--bold (cider-log-appender-id appender)))

(defun cider-log-appender-consumer (appender consumer)
  "Find the consumer in the log APPENDER by the id slot of CONSUMER."
  (let ((id (cider-log-consumer-id consumer)))
    (seq-find (lambda (consumer) (equal id (cider-log-consumer-id consumer)))
              (cider-log-appender-consumers appender))))

(defun cider-log-appender-reload (framework appender)
  "Reload the APPENDER of the log FRAMEWORK."
  (when-let (framework (cider-log-framework-reload framework))
    (cider-log-framework-appender-by-id framework (cider-log-appender-id appender))))

;; Log Consumer

(defun cider-log-consumer-id (consumer)
  "Return the id of the log CONSUMER."
  (nrepl-dict-get consumer "id"))

(defun cider-log-consumer-filters (consumer)
  "Return the filters of the log CONSUMER."
  (nrepl-dict-get consumer "filters"))

(defun cider-log-consumer-buffers (consumer)
  "Find all buffers in which `cider-log-consumer' is bound to CONSUMER."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (nrepl-dict-p cider-log-consumer)
                       (equal (cider-log-consumer-id consumer)
                              (cider-log-consumer-id cider-log-consumer)))))
              (buffer-list)))

(defun cider-log-consumer-display-name (consumer)
  "Return the display name of the log CONSUMER."
  (cider-log--bold (cider-log-consumer-id consumer)))

(defun cider-log-consumer-reload (framework appender consumer)
  "Reload the CONSUMER attached to APPENDER of the log FRAMEWORK."
  (when-let (appender (cider-log-appender-reload framework appender))
    (cider-log-appender-consumer appender consumer)))

;; Event

(defun cider-log-event--format-logback (event)
  "Format the log EVENT in logview's Logback format."
  (nrepl-dbind-response event (_exception level logger message thread timestamp)
    (propertize (format "%s [%s] %s %s - %s\n"
                        (if (numberp timestamp)
                            (format-time-string "%F %T.%3N" (/ timestamp 1000))
                          (format "%s" timestamp))
                        thread
                        (upcase level)
                        logger message)
                :cider-log-event event)))

;; Major Mode

(defun cider-log-select-framework ()
  "Select the log framework."
  (let ((frameworks (cider-sync-request:log-frameworks)))
    (cond ((= 1 (length frameworks))
           (car frameworks))
          (t (cider-log--read-framework)))))

;;;###autoload
(defun cider-log-set-framework (framework)
  "Set the Cider log framework to FRAMEWORK."
  (interactive (list (cider-log--read-framework)))
  (setq cider-log-framework framework))

;;;###autoload
(defun cider-log-set-buffer (buffer)
  "Set the Cider log buffer to BUFFER."
  (interactive (list (cider-log--read-buffer)))
  (setq cider-log-buffer buffer))

(defun cider-log--framework ()
  "Return the current log framework, or select it."
  (or cider-log-framework (cider-log-select-framework)))

(defun cider-log--appender ()
  "Return the current log appender, or select it."
  (or cider-log-appender (cider-log--read-appender nil nil nil)))

(defun cider-log--consumer ()
  "Return the current log consumer."
  cider-log-consumer)

(defun cider-log-event-at-point ()
  "Return the log event at point."
  (get-text-property (point) :cider-log-event))

(defun cider-log--search (framework appender filters)
  "Search the events of the log FRAMEWORK APPENDER matching FILTERS."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((events (nreverse (cider-sync-request:log-search framework appender :filters filters))))
      (seq-doseq (event events)
        (insert (cider-log-event--format-logback event)))
      events)))

(defun cider-log--insert-events (buffer events)
  "Insert the log EVENTS into BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((windows (seq-filter (lambda (window) (= (window-point window) (point-max)))
                               (get-buffer-window-list buffer))))
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (seq-doseq (event events)
            (insert (cider-log-event--format-logback event)))))
      (seq-doseq (window windows)
        (set-window-point window (point-max))))))

(defun cider-log-event-stacktrace (event)
  "Show the stacktrace of the given log EVENT or the one at point when called interactively"
  (interactive (list (cider-log-event-at-point)))
  (when (and event (nrepl-dict-get event "exception"))
    (let ((auto-select-buffer cider-auto-select-error-buffer)
          (causes nil))
      (cider-request:log-analyze-stacktrace
       cider-log-framework cider-log-appender event
       (lambda (response)
         (nrepl-dbind-response response (class status)
           (cond (class  (setq causes (cons response causes)))
                 (status (when causes
                           (cider-stacktrace-render
                            (cider-popup-buffer cider-error-buffer
                                                auto-select-buffer
                                                #'cider-stacktrace-mode
                                                'ancillary)
                            (reverse causes)))))))))))

(defun cider-log-format-event (event)
  "Format the given log EVENT or the one at point when called interactively."
  (interactive (list (cider-log-event-at-point)))
  (if event
      (when-let (event (cider-sync-request:log-format-event cider-log-framework cider-log-appender event))
        (cider-popup-buffer cider-log-event-buffer cider-auto-select-error-buffer 'clojure-mode 'ancillary)
        (with-current-buffer cider-log-event-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert event)
            (goto-char (point-min)))))
    (user-error "No log event found at point")))

(defun cider-log-inspect-event (event)
  "Inspect the given log EVENT or the one at point when called interactively."
  (interactive (list (cider-log-event-at-point)))
  (when event
    (cider-inspector--render-value
     (cider-sync-request:log-inspect-event
      cider-log-framework cider-log-appender
      (nrepl-dict-get event "id")))))

(defun cider-log-next-line (&optional n)
  "Move N lines forward."
  (interactive "p")
  (let ((n (or n 1)))
    ;; TODO: When logview did not guess the mode logview-next-entry complains
    (forward-line n)
    ;; (if cider-log-logview-p
    ;;     (logview-next-entry n)
    ;;   (forward-line n))
    (beginning-of-line)
    (when-let (event (cider-log-event-at-point))
      (when (get-buffer-window cider-error-buffer)
        (save-window-excursion
          (let ((cider-auto-select-error-buffer nil))
            (cider-log-event-stacktrace event))))
      (when (get-buffer-window cider-inspector-buffer)
        (save-window-excursion (cider-log-inspect-event event)))
      (when (get-buffer-window cider-log-event-buffer)
        (save-window-excursion (cider-log-format-event event))))))

(defun cider-log-previous-line (&optional n)
  "Move N lines backward."
  (interactive "p")
  (cider-log-next-line (- (or n 1))))

(defun cider-log--remove-current-buffer-consumer ()
  "Cleanup the log consumer of the current buffer."
  (when-let ((framework cider-log-framework)
             (appender cider-log-appender)
             (consumer cider-log-consumer))
    (setq-local cider-log-consumer nil)
    (when-let (consumer (cider-log-consumer-reload framework appender consumer))
      (cider-sync-request:log-remove-consumer framework appender consumer)
      consumer)))

(defvar cider-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map logview-mode-map)
    (define-key map (kbd "RET") 'cider-log-inspect-event)
    (define-key map (kbd "l a") 'cider-log-appender)
    (define-key map (kbd "l c") 'cider-log-consumer)
    (define-key map (kbd "l e") 'cider-log-event)
    (define-key map (kbd "l l") 'cider-log)
    (define-key map (kbd "C") 'cider-log)
    (define-key map (kbd "E") 'cider-log-event-stacktrace)
    (define-key map (kbd "F") 'cider-log-format-event)
    (define-key map (kbd "I") 'cider-log-inspect-event)
    (define-key map (kbd "n") 'cider-log-next-line)
    (define-key map (kbd "p") 'cider-log-previous-line)
    map)
  "The Cider log stream mode key map.")

(define-derived-mode cider-log-mode logview-mode "Cider Log"
  "Major mode for inspecting Clojure log events.

\\{cider-log-mode-map}"
  (use-local-map cider-log-mode-map)
  (setq-local electric-indent-chars nil)
  (setq-local logview-show-ellipses nil)
  (setq-local sesman-system 'CIDER)
  (setq-local truncate-lines t)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'cider-log-mode 'emacs)))

;;;###autoload
(defun cider-log-framework-browse-javadoc (framework)
  "Browse the Javadoc of the log FRAMEWORK."
  (interactive (list (cider-log--framework)))
  (browse-url (or (cider-log-framework-javadoc-url framework)
                  (user-error (format "The %s framework does not have Javadocs."
                                      (cider-log-framework-name framework))))))

;;;###autoload
(defun cider-log-framework-browse-website (framework)
  "Browse the website of the log FRAMEWORK."
  (interactive (list (cider-log--framework)))
  (browse-url (or (cider-log-framework-website-url framework)
                  (user-error (format "The %s framework does not have a website."
                                      (cider-log-framework-name framework))))))

;;;###autoload
(defun cider-log-clear-appender (framework appender)
  "Clear the log events of the APPENDER of FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-sync-request:log-clear framework appender)
  (message "Cleared the %s log appender of the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

;;;###autoload
(defun cider-log-kill-appender (framework appender)
  "Remove the log APPENDER from FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-sync-request:log-remove-appender framework appender)
  (setq-local cider-log-consumer nil)
  (message "Log appender %s removed from the %s framework."
           (cider-log-framework-display-name framework)
           (cider-log-appender-display-name appender)))

(defun cider-log--do-add-appender (framework appender)
  "Add the log APPENDER to FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (cider-sync-request:log-add-appender framework appender)
  (message "Log appender %s added to the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

(defun cider-log--do-update-appender (framework appender)
  "Update the log APPENDER of FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender)))
  ;; TODO: Get values from appender
  (thread-last (nrepl-dict
                "filters" (cider-log--filters)
                "id" (cider-log-appender-id appender)
                "size" cider-log-appender-size
                "threshold" cider-log-appender-threshold)
               (cider-sync-request:log-update-appender framework)
               (setq cider-log-appender))
  (message "Updated log appender %s of the %s framework."
           (cider-log-appender-display-name appender)
           (cider-log-framework-display-name framework)))

(defun cider-log-kill-buffer ()
  "Called from `kill-buffer-hook' to remove the consumer."
  (when (eq 'cider-log-mode major-mode)
    (when-let ((framework cider-log-framework)
               (appender cider-log-appender)
               (consumer cider-log-consumer))
      (cider-log--remove-current-buffer-consumer)
      (message "Removed %s event consumer %s from appender %s."
               (cider-log-framework-display-name framework)
               (cider-log-consumer-display-name consumer)
               (cider-log-appender-display-name appender)))))

;; Consumer commands

(defun cider-log--do-add-consumer (framework appender buffer)
  "Add CONSUMER to the APPENDER of the log FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender) (current-buffer)))
  (cider-request:log-add-consumer
   framework appender (nrepl-dict "filters" (cider-log--filters))
   (lambda (msg)
     (nrepl-dbind-response msg (log-add-consumer log-consumer log-event status)
       (cond ((member "done" status)
              (with-current-buffer (get-buffer-create buffer)
                (setq-local cider-log-framework framework)
                (setq-local cider-log-appender appender)
                (setq cider-log-consumer log-add-consumer)
                (switch-to-buffer buffer)))
             ((member "log-event" status)
              (let* ((consumer (nrepl-dict "id" log-consumer))
                     (buffers (cider-log-consumer-buffers consumer)))
                (when (seq-empty-p buffers)
                  (message "WARNING: No buffers found for %s log consumer %s of appender %s."
                           (cider-log-framework-display-name framework)
                           (cider-log-consumer-display-name consumer)
                           (cider-log-appender-display-name appender))
                  (cider-sync-request:log-remove-consumer framework appender consumer))
                (seq-doseq (buffer buffers)
                  (with-current-buffer buffer
                    (cider-log--insert-events buffer (list log-event))
                    (when (and cider-log-logview-p
                               (not (logview-initialized-p)))
                      (let ((framework cider-log-framework)
                            (appender cider-log-appender)
                            (consumer cider-log-consumer))
                        (logview--guess-submode)
                        (cider-log-mode)
                        ;; Restore buffer local vars reset by calling major mode.
                        (setq-local cider-log-framework framework
                                    cider-log-appender appender
                                    cider-log-consumer consumer))))))))))))

(defun cider-log--do-kill-consumer (framework appender consumer)
  "Remove CONSUMER from the APPENDER of the log FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log--consumer)))
  (cider-sync-request:log-remove-consumer framework appender consumer)
  (setq-local cider-log-consumer nil)
  (message "Removed %s log consumer %s for appender %s."
           (cider-log-framework-display-name framework)
           (cider-log-consumer-display-name consumer)
           (cider-log-appender-display-name appender)))

(defun cider-log--do-update-consumer (framework appender consumer)
  "Update CONSUMER of the APPENDER of the log FRAMEWORK."
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log--consumer)))
  (let ((consumer (nrepl-dict "id" (cider-log-consumer-id consumer)
                              "filters" (cider-log--filters)) ))
    (setq cider-log-consumer (cider-sync-request:log-update-consumer framework appender consumer))
    (message "Updated %s log consumer %s for appender %s."
             (cider-log-framework-display-name framework)
             (cider-log-consumer-display-name consumer)
             (cider-log-appender-display-name appender))))

(defun cider-log--do-search-events (framework appender filters)
  "Search the log events of FRAMEWORK and APPENDER which match FILTERS."
  (interactive (list (cider-log--framework) (cider-log--appender) (cider-log--filters)))
  (let ((buffer (get-buffer-create cider-log-buffer)))
    (with-current-buffer buffer
      (cider-log--remove-current-buffer-consumer)
      (let ((events (cider-log--search framework appender filters)))
        (cider-log-mode)
        (setq-local cider-log-framework framework)
        (setq-local cider-log-appender appender)
        (when (seq-empty-p events)
          (message "No log events found matching your search criteria."))
        (cider-log--do-add-consumer framework appender buffer)))))

;; Event commands

;;;###autoload
(defun cider-log-clear-events-buffer (buffer)
  "Clear the Cider log events in BUFFER."
  (interactive (list cider-log-buffer))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;; Transient

(defclass cider-log-variable (transient-lisp-variable) ())

(cl-defmethod transient-format-value ((obj cider-log-variable))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (format "%s" value) 'face 'transient-value)
    ""))

(cl-defmethod transient-prompt ((obj cider-log-variable))
  "Return the prompt to be used to read infix object OBJ's value."
  (with-slots (prompt) obj
    (let ((prompt (if (functionp prompt)
                      (funcall prompt obj)
                    prompt)))
      (if (stringp prompt)
          prompt
        (format "Set %s: " (oref obj variable))))))

(defclass cider-log-transient-framework (cider-log-variable) ())

(cl-defmethod transient-format-value ((obj cider-log-transient-framework))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (cider-log-framework-name value) 'face 'transient-value)
    ""))

(defclass cider-log-transient-appender (cider-log-variable) ())

(cl-defmethod transient-format-value ((obj cider-log-transient-appender))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (cider-log-appender-id value) 'face 'transient-value)
    ""))

(defun cider-log--transient-value (argument suffixes)
  "Return the value of the transient object matching ARGUMENT in SUFFIXES."
  (when-let (option (thread-last
                      suffixes
                      (seq-filter (lambda (obj)
                                    (and (cl-typep obj 'transient-option)
                                         (equal argument (oref obj argument)))))
                      (seq-first)))
    (oref option value)))

(defun cider-log--filters ()
  "Return the log event filters."
  (let ((suffixes (transient-suffixes transient-current-command)))
    (nrepl-dict
     "end-time" (when-let (time (cider-log--transient-value "--end-time=" suffixes))
                  (* 1000 (time-convert (parse-iso8601-time-string time) 'integer)))
     "exceptions" (cider-log--transient-value "--exceptions=" suffixes)
     "levels" (cider-log--transient-value "--levels=" suffixes)
     "loggers" (cider-log--transient-value "--loggers=" suffixes)
     "pattern" (cider-log--transient-value "--pattern=" suffixes)
     "start-time" (when-let (time (cider-log--transient-value "--start-time=" suffixes))
                    (* 1000 (time-convert (parse-iso8601-time-string time) 'integer)))
     "threads" (cider-log--transient-value "--threads=" suffixes))))

(transient-define-infix cider-log--framework-option ()
  :always-read t
  :class 'cider-log-transient-framework
  :description "Framework"
  :key "=f"
  :prompt "Log framework: "
  :reader #'cider-log--read-framework
  :variable 'cider-log-framework)

(transient-define-infix cider-log--appender-option ()
  :always-read t
  :class 'cider-log-transient-appender
  :description "Appender"
  :key "=a"
  :prompt "Log appender: "
  :reader #'cider-log--read-appender
  :variable 'cider-log-appender)

(transient-define-infix cider-log--buffer-option ()
  :always-read t
  :class 'cider-log-variable
  :description "Buffer"
  :key "=b"
  :prompt "Log buffer: "
  :reader #'cider-log--read-buffer
  :variable 'cider-log-buffer)

(transient-define-infix cider-log--appender-size-setting ()
  :always-read t
  :argument "--size="
  :class 'transient-option
  :description "Appender size"
  :key "=s"
  :prompt "Size: "
  :reader (lambda (prompt initial-input history)
            (transient-read-number-N+ prompt initial-input history)))

(transient-define-infix cider-log--appender-threshold-setting ()
  :always-read t
  :argument "--threshold="
  :class 'transient-option
  :description "Appender threshold"
  :key "=t"
  :prompt "Threshold: "
  :reader (lambda (prompt initial-input history)
            (transient-read-number-N+ prompt initial-input history)))

(transient-define-infix cider-log--exception-option ()
  :argument "--exceptions="
  :class 'transient-option
  :description "Filter by exceptions"
  :key "-E"
  :multi-value t
  :prompt "Exceptions: "
  :reader #'cider-log--read-exceptions)

(transient-define-infix cider-log--level-option ()
  :argument "--levels="
  :class 'transient-option
  :description "Filter by levels"
  :key "-l"
  :multi-value t
  :prompt "Log Levels: "
  :reader #'cider-log--read-levels)

(transient-define-infix cider-log--logger-option ()
  :argument "--loggers="
  :class 'transient-option
  :description "Filter by loggers"
  :key "-L"
  :multi-value t
  :prompt "Loggers: "
  :reader #'cider-log--read-loggers)

(transient-define-infix cider-log--thread-option ()
  :argument "--threads="
  :class 'transient-option
  :description "Filter by threads"
  :key "-t"
  :multi-value t
  :prompt "Threads: "
  :reader #'cider-log--read-threads)

(transient-define-infix cider-log--start-time-option ()
  :argument "--start-time="
  :class 'transient-option
  :description "Filter by start time"
  :key "-s"
  :prompt "Start time: "
  :reader #'cider-log--read-time)

(transient-define-infix cider-log--end-time-option ()
  :argument "--end-time="
  :class 'transient-option
  :description "Filter by end time"
  :key "-e"
  :prompt "End time: "
  :reader #'cider-log--read-time)

(transient-define-infix cider-log--pattern-option ()
  :argument "--pattern="
  :class 'transient-option
  :description "Filter by regex pattern"
  :key "-r"
  :prompt "Regex pattern: "
  :reader #'read-string)

;; Log Appender Transient

;;;###autoload (autoload 'cider-log-add-appender "cider-log" "Show the menu to add a Cider log appender." t)
(transient-define-prefix cider-log-add-appender ()
  "Show the menu to add a Cider log appender."
  ["Cider Log Appender\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("a" "Add log appender" cider-log--do-add-appender
    :inapt-if cider-log-appender-attached-p)])

;;;###autoload (autoload 'cider-log-update-appender "cider-log" "Show the menu to update a Cider log appender." t)
(transient-define-prefix cider-log-update-appender ()
  "Show the menu to update a Cider log appender."
  ["Cider Log Appender\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("u" "Update log appender" cider-log--do-update-appender
    :inapt-if-not cider-log-appender-attached-p)])

;;;###autoload (autoload 'cider-log-appender "cider-log" "Show the Cider log appender menu." t)
(transient-define-prefix cider-log-appender ()
  "Show the Cider log appender menu."
  ["Cider Log Appender\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Settings:"
   (cider-log--appender-size-setting)
   (cider-log--appender-threshold-setting)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("a" "Add log appender" cider-log--do-add-appender
    :inapt-if cider-log-appender-attached-p)
   ("c" "Clear log appender" cider-log-clear-appender
    :inapt-if-not cider-log-appender-attached-p)
   ("k" "Kill log appender" cider-log-kill-appender
    :inapt-if-not cider-log-appender-attached-p)
   ("u" "Update log appender" cider-log--do-update-appender
    :inapt-if-not cider-log-appender-attached-p)])

;; Log Consumer Transient

;;;###autoload (autoload 'cider-log-add-consumer "cider-log" "Show the menu to add a Cider log consumer." t)
(transient-define-prefix cider-log-add-consumer ()
  "Show the menu to add a Cider log consumer."
  ["Cider Log Consumer\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("a" "Add log consumer" cider-log--do-add-consumer
    :inapt-if cider-log-consumer-attached-p)])

;;;###autoload (autoload 'cider-log-update-consumer "cider-log" "Show the menu to update a Cider log consumer." t)
(transient-define-prefix cider-log-update-consumer ()
  "Show the menu to update a Cider log consumer."
  ["Cider Log Consumer\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("u" "Update log consumer" cider-log--do-update-consumer
    :inapt-if-not cider-log-consumer-attached-p)])

;;;###autoload (autoload 'cider-log-consumer "cider-log" "Show the Cider log consumer menu." t)
(transient-define-prefix cider-log-consumer ()
  "Show the Cider log consumer menu."
  ["Cider Log Consumer\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("a" "Add log consumer" cider-log--do-add-consumer
    :inapt-if cider-log-consumer-attached-p)
   ("k" "Kill log consumer" cider-log--do-kill-consumer
    :inapt-if-not cider-log-consumer-attached-p)
   ("u" "Update log consumer" cider-log--do-update-consumer
    :inapt-if-not cider-log-consumer-attached-p)])

;; Log Event Transient

;;;###autoload (autoload 'cider-log-search-events "cider-log" "Search the search log events menu." t)
(transient-define-prefix cider-log-search-events ()
  "Search the search log events menu."
  ["Cider Log Event\n"
   (cider-log--framework-option)
   (cider-log--appender-option)
   (cider-log--buffer-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("s" "Search log events" cider-log--do-search-events
    :inapt-if-not cider-log-appender-attached-p)])

;;;###autoload (autoload 'cider-log-event "cider-log" "Show the Cider log events menu." t)
(transient-define-prefix cider-log-event ()
  "Show the Cider log events menu."
  ["Cider Log Event\n"
   (cider-log--framework-option)
   (cider-log--appender-option)
   (cider-log--buffer-option)]
  ["Filters:"
   (cider-log--end-time-option)
   (cider-log--exception-option)
   (cider-log--level-option)
   (cider-log--logger-option)
   (cider-log--pattern-option)
   (cider-log--start-time-option)
   (cider-log--thread-option)]
  ["Actions"
   ("c" "Clear log event buffer" cider-log-clear-events-buffer
    :inapt-if-not cider-log-buffer-clear-p)
   ("s" "Search log events" cider-log--do-search-events
    :inapt-if-not cider-log-appender-attached-p)])

;; Log Transient

 ;;;###autoload (autoload 'cider-log "cider-log" "Show the Cider log menu." t)
(transient-define-prefix cider-log (framework appender)
  "Show the Cider log menu."
  [["Framework Actions"
    ("fs" "Select log framework" cider-log-set-framework :transient t)
    ("fb" "Select log buffer" cider-log-set-buffer :transient t)
    ("fj" "Browse Java documentation" cider-log-framework-browse-javadoc)
    ("fw" "Browse website" cider-log-framework-browse-website)]
   ["Appender Actions"
    ("aa" "Add log appender" cider-log-add-appender
     :inapt-if cider-log-appender-attached-p)
    ("ac" "Clear log appender" cider-log-clear-appender
     :inapt-if-not cider-log-appender-attached-p)
    ("ak" "Kill log appender" cider-log-kill-appender
     :inapt-if-not cider-log-appender-attached-p)
    ("au" "Update log appender" cider-log-update-appender
     :inapt-if-not cider-log-appender-attached-p)]
   ["Consumer Actions"
    ("ca" "Add log consumer" cider-log-add-consumer
     :inapt-if cider-log-consumer-attached-p)
    ("ck" "Kill log consumer" cider-log--do-kill-consumer
     :inapt-if-not cider-log-consumer-attached-p)
    ("cu" "Update log consumer" cider-log-update-consumer
     :inapt-if-not cider-log-consumer-attached-p)]
   ["Event Actions"
    ("ec" "Clear log events buffer" cider-log-clear-events-buffer
     :inapt-if-not cider-log-buffer-clear-p)
    ("es" "Search log events" cider-log-search-events
     :inapt-if-not cider-log-appender-attached-p)]]
  (interactive (list (cider-log--framework) (cider-log--appender)))
  (setq cider-log-framework framework)
  (setq cider-log-appender appender)
  ;; (unless (cider-log-appender-reload framework appender)
  ;;   (cider-log--do-add-appender framework appender))
  (transient-setup 'cider-log))

(add-hook 'kill-buffer-hook #'cider-log-kill-buffer)

(provide 'cider-log)

;;; cider-log.el ends here
