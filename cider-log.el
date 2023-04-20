;;; cider-log.el --- Log inspection functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2023 CIDER contributors
;;
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

;;; Code:

(require 'autorevert)
(require 'cider)
(require 'cl-lib)
(require 'logview)
(require 'org)
(require 'transient)

(defcustom cider-log-appender-id "cider"
  "The name of the default log appender."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-buffer-name "*cider-log*"
  "The name of the log buffer."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-log-search-buffer-name "*cider-log-search*"
  "The name of the log search buffer."
  :group 'cider
  :package-version '(cider . "1.7.0")
  :safe #'stringp
  :type 'string)

(defvar cider-log-framework nil
  "The current log framework to use.")

(defvar cider-log-appender nil
  "The current log appender.")

(defvar cider-log-exceptions nil
  "The list of exception names used to filter records.")

(defvar cider-log-levels nil
  "The list of log level names used to filter records.")

(defvar cider-log-loggers nil
  "The list of logger names used to filter records.")

(defvar cider-log-threads nil
  "The list of thread names used to filter records.")

(defvar cider-log-start-time nil
  "The start time used to filter records.")

(defvar cider-log-end-time nil
  "The end time used to filter records.")

(defvar cider-log-pattern nil
  "The regular expression pattern used to filter records.")

(defvar-local cider-log-pending-events nil
  "The pending log event that need to be inserted into the buffer.")

(defun cider-log--bold (s)
  "Return S with a bold face."
  (when s (propertize (format "%s" s) 'face 'bold)))

(defclass cider-transient-lisp-variable (transient-lisp-variable) ())

(cl-defmethod transient-format-value ((obj cider-transient-lisp-variable))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (prin1-to-string value) 'face 'transient-value)
    ""))

(cl-defmethod transient-prompt ((obj cider-transient-lisp-variable))
  "Return the prompt to be used to read infix object OBJ's value."
  (with-slots (prompt) obj
    (let ((prompt (if (functionp prompt)
                      (funcall prompt obj)
                    prompt)))
      (if (stringp prompt)
          prompt
        (format "Set %s: " (oref obj variable))))))

(defclass cider-transient-time (cider-transient-lisp-variable) ())

(cl-defmethod transient-format-value ((obj cider-transient-time))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (format-time-string "%a, %b %d %T %Y" value) 'face 'transient-value)
    ""))

(defun cider-log-framework-id (framework)
  "Return the id of the log FRAMEWORK."
  (nrepl-dict-get framework "id"))

(defun cider-log-framework-name (framework)
  "Return the name of the log FRAMEWORK."
  (nrepl-dict-get framework "name"))

(defun cider-log-framework-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-sync-request:add-appender (cider-log-framework-id framework)
                                   (cider-log-appender-id appender)))

(defun cider-log-framework-appenders (framework)
  "Return the appenders of the log FRAMEWORK."
  (nrepl-dict-get framework "appenders"))

(defun cider-log-framework-appender-by-id (framework id)
  "Lookup the appender of the log FRAMEWORK by ID."
  (seq-find (lambda (appender)
              (equal id (cider-log-appender-id appender)))
            (cider-log-framework-appenders framework)))

(defun cider-log-framework-by-name (frameworks name)
  "Find the log framework in FRAMEWORKS by NAME."
  (seq-find (lambda (framework) (equal name (cider-log-framework-name framework))) frameworks))

(defun cider-log-framework-remove-appender (framework appender)
  "Remove the APPENDER from the log FRAMEWORK."
  (cider-sync-request:remove-appender (cider-log-framework-id framework)
                                      (cider-log-appender-id appender)))

(defun cider-log-appender-id (appender)
  "Return the id of the log APPENDER."
  (nrepl-dict-get appender "id"))

(defun cider-log--format-logview-line (event)
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

(defun cider-log--reset-filters ()
  "Reset the filters."
  (setq cider-log-end-time nil
        cider-log-exceptions nil
        cider-log-levels nil
        cider-log-loggers nil
        cider-log-start-time nil
        cider-log-threads nil))

(defun cider-log-completing-read-framework (frameworks &optional prompt initial-input history)
  "Read a log framework in the minibuffer with completion.

  FRAMEWORKS is a list of NREPL dictionaries containing log frameworks.

  PROMPT is a string to prompt with; normally it ends in a colon
  and a space.

  INITIAL-INPUT if non-nil, insert it in the minibuffer
  initially.

  HISTORY if non-nil, specifies a history list and optionally the initial
  position in the list."
  (let* ((completion-extra-properties
          `(:annotation-function
            (lambda (identifier)
              (when-let (entry (cadr (assoc identifier minibuffer-completion-table)))
                (let ((annotation (nrepl-dict-get entry "description")))
                  (propertize (format " - %s" (or annotation "No documentation"))
                              'face 'font-lock-comment-face))))))
         (selected (completing-read (or prompt "Log framework: ")
                                    (seq-map (lambda (framework)
                                               (list (cider-log-framework-name framework) framework))
                                             frameworks)
                                    nil nil initial-input history)))
    (cider-log-framework-by-name frameworks selected)))

(defun cider-sync-request:inspect-event (framework appender id)
  "Inspect the log event with the ID in the APPENDER of the log FRAMEWORK."
  (thread-first `("op" "log-inspect"
                  "framework" ,framework
                  "appender" ,appender
                  "event-id" ,id)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:add-appender (framework appender)
  "Add the APPENDER to the logging FRAMEWORK."
  (thread-first `("op" "log-add-appender"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)))

(defun cider-nrepl-add-consumer (framework appender consumer &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (thread-first `("op" "log-add-consumer"
                  "framework" ,framework
                  "appender" ,appender
                  "consumer" ,consumer)
                (cider-nrepl-send-request callback)))

(defun cider-sync-request:clear (framework appender)
  "Clear the log events for FRAMEWORK and APPENDER."
  (thread-first `(list "op" "log-clear-appender"
                       "framework" ,framework
                       "appender" ,appender)
                (cider-nrepl-send-sync-request)))

(defun cider-sync-request:log-frameworks ()
  "Return the available log frameworks."
  (thread-first `("op" "log-frameworks")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "frameworks")
                (nrepl-dict-vals)))

(cl-defun cider-sync-request:search (framework appender &key end-time exceptions levels loggers pattern start-time threads)
  "Search the Cider log events for FRAMEWORK and APPENDER.

  END-TIME - filter events before the end time timestamp
  EXCEPTIONS - filter events by the exceptions
  LEVELS - filter events with the log levels
  LOGGERS - filter events by the loggers
  PATTERN - filter events whose message matches pattern
  START-TIME - filter events after the start time timestamp
  THREADS  - filter events after the start time timestamp"
  (thread-first `("op" "log-search"
                  "framework" ,framework
                  "appender" ,appender
                  "end-time" ,end-time
                  "exceptions" ,exceptions
                  "levels" ,levels
                  "loggers" ,loggers
                  "pattern" ,pattern
                  "start-time" ,start-time
                  "threads" ,threads)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "search")))

(defun cider-sync-request:exceptions (framework appender)
  "Return the Cider log exceptions for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-exceptions"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "exceptions")))

(defun cider-sync-request:levels (framework appender)
  "Return the Cider log levels for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-levels"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "levels")))

(defun cider-sync-request:loggers (framework appender)
  "Return the Cider loggers for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-loggers"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "loggers")))

(defun cider-sync-request:remove-appender (framework appender)
  "Remove the APPENDER from the logging FRAMEWORK."
  (thread-first `("op" "log-remove-appender"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)))

(defun cider-sync-request:remove-consumer (framework appender consumer)
  "Remove the CONSUMER from the APPENDER of the logging FRAMEWORK."
  (thread-first `("op" "log-remove-consumer"
                  "framework" ,framework
                  "appender" ,appender
                  "consumer" ,consumer)
                (cider-nrepl-send-sync-request)))

(defun cider-sync-request:threads (framework appender)
  "Return the log exceptions for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-threads"
                  "framework" ,framework
                  "appender" ,appender)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "threads")))

(defun cider-log--search ()
  "Search for log events matching the current filters."
  (cider-sync-request:search
   (cider-log-framework-id cider-log-framework)
   (cider-log-appender-id cider-log-appender)
   :end-time (when cider-log-end-time
               (* 1000 (time-convert cider-log-end-time 'integer)))
   :exceptions cider-log-exceptions
   :levels cider-log-levels
   :loggers cider-log-loggers
   :pattern cider-log-pattern
   :start-time (when cider-log-start-time
                 (* 1000 (time-convert cider-log-start-time 'integer)))
   :threads cider-log-threads))

;;;###autoload
(defun cider-log-attach ()
  "Attach the Cider log."
  (interactive)
  (let ((response (cider-sync-request:add-appender cider-log-framework cider-log-appender)))
    (nrepl-dbind-response response (add-appender status)
      (cond ((member "done" status)
             (message "Appender %s added to the %s logging framework."
                      (cider-log--bold cider-log-appender)
                      (cider-log--bold cider-log-framework)))
            (t (message "Failed to add appender %s to the %s logging framework."
                        (cider-log--bold cider-log-appender)
                        (cider-log--bold cider-log-framework)))))))

;;;###autoload
(defun cider-log-add-consumer ()
  "Attach the Cider log."
  (interactive)
  (message "Adding log consumer ...")
  (cider-sync-request:add-appender
   cider-log-framework cider-log-appender "*cider-log*"
   (lambda (response)
     (nrepl-dbind-response response (status event)
       (setq my-response response)
       (cond
        ((member "done" status)
         (with-current-buffer (get-buffer-create "*cider-log*")
           (cider-log-mode)
           (pop-to-buffer (current-buffer))))
        ((member "log-event" status)
         (with-current-buffer (get-buffer-create "*cider-log*")
           (add-to-list 'cider-log-pending-events event)))))))
  (message "Cider log attached."))

;;;###autoload
(defun cider-log-clear ()
  "Clear the Cider log."
  (interactive)
  (let ((response (cider-sync-request:clear cider-log-framework cider-log-appender)))
    (nrepl-dbind-response response (status)
      (cond ((member "done" status)
             (message "Cleared log events of the %s appender %s."
                      (cider-log--bold cider-log-framework)
                      (cider-log--bold cider-log-appender)))
            (t (message "Failed to clear log events of the %s appender %s."
                        (cider-log--bold cider-log-framework)
                        (cider-log--bold cider-log-appender)))))))

;;;###autoload
(defun cider-log-detach ()
  "Detach the Cider log."
  (interactive)
  (let ((response (cider-sync-request:remove-appender cider-log-framework cider-log-appender)))
    (nrepl-dbind-response response (status)
      (cond ((member "done" status)
             (message "Appender %s removed from the %s logging framework."
                      (cider-log--bold cider-log-appender)
                      (cider-log--bold cider-log-framework)))
            (t (message "Failed to remove appender %s from the %s logging framework."
                        (cider-log--bold cider-log-appender)
                        (cider-log--bold cider-log-framework)))))))

(defun cider-log-event-at-point ()
  "Return the log event at point."
  (get-text-property (point) :cider-log-event))

;;;###autoload
(defun cider-log-inspect ()
  "Inspect the log event."
  (interactive)
  (if-let (event (cider-log-event-at-point))
      (cider-inspector--render-value
       (cider-sync-request:inspect-event
        (cider-log-framework-id cider-log-framework)
        (cider-log-appender-id cider-log-appender)
        (nrepl-dict-get event "id")))
    (user-error "Sorry, no log event id found at point")))

(defun cider-log-refresh ()
  "Return the log events."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (seq-doseq (event (cider-log--search))
      (insert (cider-log--format-logview-line event)))
    (goto-char (point-min))))

;;;###autoload
(defun cider-log-reset ()
  "Reset the filters."
  (interactive)
  (cider-log--reset-filters)
  (cider-log-show))

;;;###autoload
(defun cider-log-show ()
  "Show the Cider log data."
  (interactive)
  (with-current-buffer (get-buffer-create "*cider-log*")
    (cider-log-ensure-appender)
    (cider-log-refresh)
    (cider-log-mode)
    (switch-to-buffer (current-buffer))))

;; (defun cider-log-revert (&optional _ignore-auto _noconfirm)
;;   "Revert the Cider log buffer."
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*cider-log*")
;;     (goto-char (point-min))
;;     (when (looking-at "\\.\\.\\.")
;;       (forward-line 1))
;;     (when-let (last-event (cider-log-event-at-point))
;;       (let ((inhibit-read-only t)
;;             (cider-log-start-time (time-convert (/ (nrepl-dict-get last-event "timestamp") 1000) t))
;;             (cider-log-end-time nil))
;;         (seq-doseq (event (cdr (cider-log--search)))
;;           (unless (= (nrepl-dict-get last-event "id")
;;                      (nrepl-dict-get event "id"))
;;             (insert (cider-log--format-logview-line event))))
;;         (goto-char (point-min))))))

(defun cider-log--insert-events (buffer events)
  "Insert the log EVENTS into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t)
            ;; TODO: Prevent buffer flickering?
            (inhibit-double-buffering t))
        (goto-char (point-max))
        (seq-doseq (event events)
          (insert (cider-log--format-logview-line event)))))))

(defun cider-log-revert (&optional _ignore-auto _noconfirm)
  "Revert the Cider log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*cider-log*")
    (let ((events cider-log-pending-events)
          (windows (seq-filter (lambda (window) (= (window-point window) (point-max)))
                               (get-buffer-window-list (current-buffer))))
          (current-point (point)))
      (setq cider-log-pending-events nil)
      (cider-log--insert-events (current-buffer) (reverse events))
      (seq-doseq (window windows)
        (set-window-point window (point-max))))))

(defun cider-log--buffer-stale-p (&optional _noconfirm)
  "Return non-nil if the Cider log buffer is stale."
  (car cider-log-pending-events))

(defun cider-log--revert-buffer (_ignore-auto _noconfirm)
  "Revert the Cider log buffer."
  (cider-log-refresh))

(defvar cider-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map logview-mode-map)
    (define-key map (kbd "RET") 'cider-log-inspect)
    (define-key map (kbd "g") 'cider-log-revert)
    (define-key map (kbd "l l") 'cider-log)
    map)
  "The Cider log stream mode key map.")

(define-derived-mode cider-log-mode logview-mode "Cider Log"
  "Special mode for streaming Cider log events."
  (use-local-map cider-log-mode-map)
  (setq-local auto-revert-interval 1)
  (setq-local auto-revert-verbose nil)
  (setq-local buffer-stale-function #'cider-log--buffer-stale-p)
  (setq-local electric-indent-chars nil)
  (setq-local revert-buffer-function #'cider-log-revert)
  (setq-local sesman-sycider 'CIDER)
  (auto-revert-mode 1)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'cider-log-mode 'emacs)))

(transient-define-infix cider-log--exception-filter ()
  :always-read t
  :class 'cider-transient-lisp-variable
  :description "Filter by exceptions"
  :key "-E"
  :prompt "Exceptions: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:exceptions
               (cider-log-framework-id cider-log-framework)
               (cider-log-appender-id cider-log-appender)))
             nil nil initial-input history))
  :variable 'cider-log-exceptions)

(transient-define-infix cider-log--level-filter ()
  :always-read t
  :class 'cider-transient-lisp-variable
  :description "Filter by levels"
  :key "-l"
  :prompt "Log Levels: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:levels
               (cider-log-framework-id cider-log-framework)
               (cider-log-appender-id cider-log-appender)))
             nil nil initial-input history))
  :variable 'cider-log-levels)

(transient-define-infix cider-log--logger-filter ()
  :always-read t
  :class 'cider-transient-lisp-variable
  :description "Filter by loggers"
  :key "-L"
  :variable 'cider-log-loggers
  :prompt "Loggers: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:loggers
               (cider-log-framework-id cider-log-framework)
               (cider-log-appender-id cider-log-appender)))
             nil nil initial-input history)))

(transient-define-infix cider-log--thread-filter ()
  :always-read t
  :class 'cider-transient-lisp-variable
  :description "Filter by threads"
  :key "-t"
  :prompt "Threads: "
  :variable 'cider-log-threads
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:threads
               (cider-log-framework-id cider-log-framework)
               (cider-log-appender-id cider-log-appender)))
             nil nil initial-input history)))

(transient-define-infix cider-log--start-time-filter ()
  :always-read nil
  :class 'cider-transient-time
  :description "Filter by start time"
  :key "-s"
  :prompt "Start time: "
  :reader (lambda (prompt initial-input _)
            (org-read-date t 'to-time nil prompt nil (or initial-input "-30m")))
  :variable 'cider-log-start-time)

(transient-define-infix cider-log--end-time-filter ()
  :always-read nil
  :class 'cider-transient-time
  :description "Filter by end time"
  :key "-e"
  :prompt "End time: "
  :reader (lambda (prompt initial-input _)
            (org-read-date t 'to-time nil prompt nil initial-input))
  :variable 'cider-log-end-time)

(transient-define-infix cider-log--pattern-filter ()
  :always-read nil
  :class 'cider-transient-lisp-variable
  :description "Filter by regex pattern"
  :key "-r"
  :prompt "Regex pattern: "
  :variable 'cider-log-pattern
  :reader #'read-string)

;;;###autoload (autoload 'cider-log "cider-log" "Show the Cider log menu." t)
(transient-define-prefix cider-log ()
  "Show the Cider log menu."
  ["Cider Log\n\nOptions:"
   (cider-log--end-time-filter)
   (cider-log--exception-filter)
   (cider-log--level-filter)
   (cider-log--logger-filter)
   (cider-log--pattern-filter)
   (cider-log--start-time-filter)
   (cider-log--thread-filter)]
  ["Actions"
   ("a" "Attach" cider-log-attach)
   ("c" "Clear" cider-log-clear)
   ("d" "Detach" cider-log-detach)
   ("l" "Show" cider-log-show)
   ("r" "Reset filters" cider-log-reset)])

(defun cider-log-framework-select ()
  "Select the log framework."
  (let ((frameworks (cider-sync-request:log-frameworks)))
    (cond ((= 1 (length frameworks))
           (car frameworks))
          (t (cider-log-completing-read-framework frameworks)))))

(defun cider-log-appender-select ()
  "Select the log appender."
  (nrepl-dict "id" (or cider-log-appender-id (read-string "Log appender: "))))

(defun cider-log-add-appender ()
  "Add the log appender to the current framework."
  (interactive)
  (let ((framework (setq cider-log-framework (cider-log-framework-select)))
        (appender (setq cider-log-appender (cider-log-appender-select))))
    (cider-log-framework-add-appender framework appender)))

(defun cider-log-ensure-appender ()
  "Ensure the current log framework and appender are initialized."
  (unless cider-log-framework
    (setq cider-log-framework (cider-log-framework-select)))
  (unless cider-log-appender
    (setq cider-log-appender (cider-log-appender-select))))

(defun cider-log-remove-appender ()
  "Remove the log appender from the current framework."
  (interactive)
  (cider-log-framework-remove-appender cider-log-framework cider-log-appender)
  (setq cider-log-appender nil))

(provide 'cider-log)

;;; cider-log.el ends here
