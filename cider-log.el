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

(require 'autorevert)
(require 'cider)
(require 'cider-inspector)
(require 'cl-lib)
(require 'logview)
(require 'org)
(require 'transient)

(defcustom cider-log-appender-name "cider"
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

(defvar cider-log-framework nil
  "The current log framework to use.")

(defvar cider-log-appender nil
  "The current log appender.")

(defvar-local cider-log-consumer nil
  "The current log consumer.")

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

(defun cider-log--strip-whitespace (s)
  "Replace multiple white space characters in S with a single one."
  (replace-regexp-in-string "[\n ]+" " " s))

(defun cider-log-filter-selected-p ()
  "Return non-nil if any filter is selected, otherwise nil."
  (or cider-log-exceptions
      cider-log-end-time
      cider-log-levels
      cider-log-loggers
      cider-log-pattern
      cider-log-start-time
      cider-log-threads))

(defun cider-log-appender-attached-p ()
  "Return non-nil if the log appender is attached, otherwise nil."
  (when (and cider-log-framework cider-log-appender)
    (let ((framework (cider-log-framework-reload cider-log-framework)))
      (cider-log-framework-appender-by-id framework (cider-log-appender-name cider-log-appender)))))

;; NREPL

(defun cider-request:log-add-consumer (framework appender consumer &optional callback)
  "Add CONSUMER to the APPENDER of FRAMEWORK and call CALLBACK on log events."
  (cider-ensure-op-supported "log-add-consumer")
  (thread-first `("op" "log-add-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender)
                  "consumer" ,consumer)
                (cider-nrepl-send-request callback)))

(defun cider-sync-request:log-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (thread-first `("op" "log-add-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)))

(defun cider-sync-request:log-clear (framework appender)
  "Clear the log events for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-clear-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)))

(defun cider-sync-request:log-inspect-event (framework appender id)
  "Inspect the log event with the ID in the APPENDER of the log FRAMEWORK."
  (thread-first `("op" "log-inspect-event"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender)
                  "event-id" ,id)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:log-frameworks ()
  "Return the available log frameworks."
  (thread-first `("op" "log-frameworks")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "frameworks")
                (nrepl-dict-vals)))

(cl-defun cider-sync-request:log-search (framework appender &key end-time exceptions levels loggers pattern start-time threads)
  "Search the Cider log events for FRAMEWORK and APPENDER.

  END-TIME - filter events before the end time timestamp
  EXCEPTIONS - filter events by the exceptions
  LEVELS - filter events with the log levels
  LOGGERS - filter events by the loggers
  PATTERN - filter events whose message matches pattern
  START-TIME - filter events after the start time timestamp
  THREADS  - filter events after the start time timestamp"
  (thread-first `("op" "log-search"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender)
                  "end-time" ,end-time
                  "exceptions" ,exceptions
                  "levels" ,levels
                  "loggers" ,loggers
                  "pattern" ,pattern
                  "start-time" ,start-time
                  "threads" ,threads)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "search")))

(defun cider-sync-request:log-exceptions (framework appender)
  "Return the Cider log exceptions for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-exceptions"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "exceptions")))

(defun cider-sync-request:log-levels (framework appender)
  "Return the Cider log levels for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-levels"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "levels")))

(defun cider-sync-request:log-loggers (framework appender)
  "Return the Cider loggers for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-loggers"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "loggers")))

(defun cider-sync-request:log-remove-appender (framework appender)
  "Remove the APPENDER from the log FRAMEWORK."
  (thread-first `("op" "log-remove-appender"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "remove-appender")))

(defun cider-sync-request:log-remove-consumer (framework appender consumer)
  "Remove the CONSUMER from the APPENDER of the log FRAMEWORK."
  (thread-first `("op" "log-remove-consumer"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender)
                  "consumer" ,consumer)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "remove-consumer")))

(defun cider-sync-request:log-threads (framework appender)
  "Return the log exceptions for FRAMEWORK and APPENDER."
  (thread-first `("op" "log-threads"
                  "framework" ,(cider-log-framework-id framework)
                  "appender" ,(cider-log-appender-name appender))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "threads")))

(defun cider-log--completion-extra-properties (keys &optional separator)
  "Return the `completion-extra-properties` for NREPL completions."
  `(:annotation-function
    ,(lambda (identifier)
       (when-let (dict (cadr (assoc identifier minibuffer-completion-table)))
         (let ((annotation (string-join (seq-map (lambda (key) (nrepl-dict-get dict key)) keys) (or separator " "))))
           (unless (string-blank-p annotation)
             (propertize (format " - %s" (cider-log--strip-whitespace annotation)) 'face 'font-lock-comment-face)))))))

(defun cider-log-completing-read-framework (frameworks &optional prompt initial-input history)
  "Read a log framework in the minibuffer with completion.

  FRAMEWORKS is a list of log frameworks.

  PROMPT is a string to prompt with; normally it ends in a colon
  and a space.

  INITIAL-INPUT if non-nil, insert it in the minibuffer
  initially.

  HISTORY if non-nil, specifies a history list and optionally the initial
  position in the list."
  (let ((completion-extra-properties (cider-log--completion-extra-properties '("description"))))
    (cider-log-framework-by-name
     frameworks
     (completing-read
      (or prompt "Log framework: ")
      (seq-map (lambda (framework)
                 (list (cider-log-framework-name framework) framework))
               frameworks)
      nil nil initial-input history))))

;; Framework

(defun cider-log-framework-id (framework)
  "Return the id of the log FRAMEWORK."
  (nrepl-dict-get framework "id"))

(defun cider-log-framework-name (framework)
  "Return the name of the log FRAMEWORK."
  (nrepl-dict-get framework "name"))

(defun cider-log-framework-javadoc-url (framework)
  "Return the Javadoc URL of the log FRAMEWORK."
  (nrepl-dict-get framework "javadoc-url"))

(defun cider-log-framework-website-url (framework)
  "Return the website URL of the log FRAMEWORK."
  (nrepl-dict-get framework "website-url"))

(defun cider-log-framework-add-appender (framework appender)
  "Add the APPENDER to the log FRAMEWORK."
  (cider-sync-request:log-add-appender framework appender))

(defun cider-log-framework-appenders (framework)
  "Return the appenders of the log FRAMEWORK."
  (nrepl-dict-get framework "appenders"))

(defun cider-log-framework-appender-by-id (framework id)
  "Lookup the appender of the log FRAMEWORK by ID."
  (seq-find (lambda (appender)
              (equal id (cider-log-appender-name appender)))
            (cider-log-framework-appenders framework)))

(defun cider-log-framework-by-name (frameworks name)
  "Find the log framework in FRAMEWORKS by NAME."
  (seq-find (lambda (framework) (equal name (cider-log-framework-name framework))) frameworks))

(defun cider-log-framework-remove-appender (framework appender)
  "Remove the APPENDER from the log FRAMEWORK."
  (cider-sync-request:log-remove-appender framework appender))

(defun cider-log-framework-reload (framework)
  "Reload the log FRAMEWORK."
  (cider-log-framework-by-name (cider-sync-request:log-frameworks)
                               (cider-log-framework-name framework)))

;; Appender

(defun cider-log-appender-name (appender)
  "Return the id of the log APPENDER."
  (nrepl-dict-get appender "name"))

;; Consumer

(defun cider-log-add-consumer ()
  "Attach the Cider log."
  (interactive)
  (message "Adding log consumer ...")
  ;; (cider-sync-request:log-add-appender
  ;;  cider-log-framework cider-log-appender cider-log-buffer-name
  ;;  (lambda (response)
  ;;    (nrepl-dbind-response response (status event)
  ;;      (cond ((member "done" status)
  ;;             (with-current-buffer (get-buffer-create cider-log-buffer-name)
  ;;               (cider-log-mode)
  ;;               (pop-to-buffer (current-buffer))))
  ;;            ((member "log-event" status)
  ;;             (with-current-buffer (get-buffer-create cider-log-buffer-name)
  ;;               (add-to-list 'cider-log-pending-events event)))))))
  (message "Cider log attached."))

;; Event

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

;; Major Mode

(defun cider-log--reset-filters ()
  "Reset the filters."
  (setq cider-log-end-time nil
        cider-log-exceptions nil
        cider-log-levels nil
        cider-log-loggers nil
        cider-log-start-time nil
        cider-log-threads nil))

(defun cider-log-select-framework ()
  "Select the log framework."
  (let ((frameworks (cider-sync-request:log-frameworks)))
    (cond ((= 1 (length frameworks))
           (car frameworks))
          (t (cider-log-completing-read-framework frameworks)))))

(defun cider-log-select-appender ()
  "Select the log appender."
  (nrepl-dict "name" (read-string "Log appender: " nil nil cider-log-appender-name)))

(defun cider-log-add-appender ()
  "Add the log appender to the current framework."
  (interactive)
  (let ((framework (setq cider-log-framework (cider-log-select-framework)))
        (appender (setq cider-log-appender (cider-log-select-appender))))
    (cider-log-framework-add-appender framework appender)))

(defun cider-log-ensure-appender ()
  "Ensure the current log framework and appender are initialized."
  (unless cider-log-framework
    (setq cider-log-framework (cider-log-select-framework)))
  (unless cider-log-appender
    (setq cider-log-appender (nrepl-dict "name" cider-log-appender-name))))

(defun cider-log-remove-appender ()
  "Remove the log appender from the current framework."
  (interactive)
  (cider-log-framework-remove-appender cider-log-framework cider-log-appender)
  (setq cider-log-appender nil))

(defun cider-log--search ()
  "Search for log events matching the current filters."
  (cider-sync-request:log-search
   cider-log-framework cider-log-appender
   :end-time (when cider-log-end-time
               (* 1000 (time-convert cider-log-end-time 'integer)))
   :exceptions cider-log-exceptions
   :levels cider-log-levels
   :loggers cider-log-loggers
   :pattern cider-log-pattern
   :start-time (when cider-log-start-time
                 (* 1000 (time-convert cider-log-start-time 'integer)))
   :threads cider-log-threads))

(defun cider-log-event-at-point ()
  "Return the log event at point."
  (get-text-property (point) :cider-log-event))

(defun cider-log-refresh ()
  "Return the log events."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (seq-doseq (event (cider-log--search))
      (insert (cider-log--format-logview-line event)))
    (goto-char (point-min))))

(defun cider-log--insert-events (buffer events)
  "Insert the log EVENTS into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (seq-doseq (event events)
          (insert (cider-log--format-logview-line event)))))))

(defun cider-log-revert (&optional _ignore-auto _noconfirm)
  "Revert the Cider log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create cider-log-buffer-name)
    (let ((events cider-log-pending-events)
          (windows (seq-filter (lambda (window) (= (window-point window) (point-max)))
                               (get-buffer-window-list (current-buffer)))))
      (setq cider-log-pending-events nil)
      (cider-log--insert-events (current-buffer) (reverse events))
      (seq-doseq (window windows)
        (set-window-point window (point-max))))))

(defun cider-log--buffer-stale-p (&optional _noconfirm)
  "Return non-nil if the Cider log buffer is stale."
  (car cider-log-pending-events))

(defun cider-log-inspect-event-at-point ()
  "Inspect the log event at point."
  (interactive)
  (when-let (event (cider-log-event-at-point))
    (cider-inspector--render-value
     (cider-sync-request:log-inspect-event
      cider-log-framework cider-log-appender
      (nrepl-dict-get event "id")))))

(defun cider-log--revert-buffer (_ignore-auto _noconfirm)
  "Revert the Cider log buffer."
  (cider-log-refresh))

(defun cider-log-next-line (&optional n)
  "Move N lines forward."
  (interactive)
  (forward-line (or n 1))
  (when (get-buffer-window cider-inspector-buffer)
    (save-window-excursion (cider-log-inspect-event-at-point))))

(defun cider-log-previous-line (&optional n)
  "Move N lines backward."
  (interactive)
  (cider-log-next-line (- (or n 1))))

(defun cider-log-resume ()
  "Resume streaming log events to the client."
  (interactive)
  (with-current-buffer (get-buffer-create cider-log-buffer-name)
    (when (and cider-log-framework cider-log-appender)
      (setq cider-log-consumer cider-log-buffer-name)
      (cider-request:log-add-consumer
       cider-log-framework cider-log-appender cider-log-consumer
       (lambda (msg)
         (when-let (event (nrepl-dict-get msg "event"))
           ;; TODO: Make sure we add to the right buffer local var
           (with-current-buffer (get-buffer-create cider-log-buffer-name)
             (add-to-list 'cider-log-pending-events event)))))
      (message "Log consumer added."))))

(defun cider-log-pause ()
  "Pause streaming log events to the client."
  (interactive)
  (when (and cider-log-framework cider-log-appender cider-log-consumer)
    (with-current-buffer (get-buffer-create cider-log-buffer-name)
      (cider-sync-request:log-remove-consumer cider-log-framework cider-log-appender cider-log-consumer)
      (setq cider-log-consumer nil)
      (message "Log consumer removed."))))

(defvar cider-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map logview-mode-map)
    (define-key map (kbd "RET") 'cider-log-inspect-event-at-point)
    (define-key map (kbd "g") 'cider-log-revert)
    (define-key map (kbd "l l") 'cider-log)
    (define-key map (kbd "n") 'cider-log-next-line)
    (define-key map (kbd "p") 'cider-log-previous-line)
    (define-key map (kbd "P") 'cider-log-pause)
    (define-key map (kbd "R") 'cider-log-resume)
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

;; Transient

(defclass cider-log-variable (transient-lisp-variable) ())

(cl-defmethod transient-format-value ((obj cider-log-variable))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (prin1-to-string value) 'face 'transient-value)
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

(defclass cider-transient-time (cider-log-variable) ())

(cl-defmethod transient-format-value ((obj cider-transient-time))
  "Format OBJ's value for display and return the result."
  (if-let (value (oref obj value))
      (propertize (format-time-string "%a, %b %d %T %Y" value) 'face 'transient-value)
    ""))

(transient-define-infix cider-log--framework-option ()
  :always-read t
  :class 'cider-log-variable
  :description "Log framework"
  :key "F"
  :prompt "Log framework: "
  :reader (lambda (prompt initial-input history)
            (cider-log-completing-read-framework
             (cider-sync-request:log-frameworks)
             prompt initial-input history))
  :variable 'cider-log-framework)

(transient-define-infix cider-log--appender-option ()
  :always-read t
  :class 'cider-log-variable
  :description "Log appender"
  :key "A"
  :prompt "Log appender: "
  :reader (lambda (_prompt _initial-input _history)
            (cider-log-select-appender))
  :variable 'cider-log-appender)

(transient-define-infix cider-log--exception-filter ()
  :always-read t
  :class 'cider-log-variable
  :description "Filter by exceptions"
  :key "-E"
  :prompt "Exceptions: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:log-exceptions
               cider-log-framework cider-log-appender))
             nil nil initial-input history))
  :variable 'cider-log-exceptions)

(transient-define-infix cider-log--level-filter ()
  :always-read t
  :class 'cider-log-variable
  :description "Filter by levels"
  :key "-l"
  :prompt "Log Levels: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:log-levels
               cider-log-framework cider-log-appender))
             nil nil initial-input history))
  :variable 'cider-log-levels)

(transient-define-infix cider-log--logger-filter ()
  :always-read t
  :class 'cider-log-variable
  :description "Filter by loggers"
  :key "-L"
  :variable 'cider-log-loggers
  :prompt "Loggers: "
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:log-loggers
               cider-log-framework cider-log-appender))
             nil nil initial-input history)))

(transient-define-infix cider-log--thread-filter ()
  :always-read t
  :class 'cider-log-variable
  :description "Filter by threads"
  :key "-t"
  :prompt "Threads: "
  :variable 'cider-log-threads
  :reader (lambda (prompt initial-input history)
            (completing-read-multiple
             prompt
             (nrepl-dict-keys
              (cider-sync-request:log-threads
               cider-log-framework cider-log-appender))
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
  :class 'cider-log-variable
  :description "Filter by regex pattern"
  :key "-r"
  :prompt "Regex pattern: "
  :variable 'cider-log-pattern
  :reader #'read-string)

;;;###autoload
(defun cider-log-framework-browse-javadoc (framework)
  "Browse the Javadoc of the log FRAMEWORK."
  (interactive (list (cider-log-select-framework)))
  (browse-url (or (cider-log-framework-javadoc-url framework)
                  (user-error (format "The %s framework does not have Javadocs."
                                      (cider-log-framework-name framework))))))

;;;###autoload
(defun cider-log-framework-browse-website (framework)
  "Browse the website of the log FRAMEWORK."
  (interactive (list (cider-log-select-framework)))
  (browse-url (or (cider-log-framework-website-url framework)
                  (user-error (format "The %s framework does not have a website."
                                      (cider-log-framework-name framework))))))

;;;###autoload
(defun cider-log-appender-add (framework appender)
  "Add the log APPENDER from FRAMEWORK."
  (interactive (list cider-log-framework cider-log-appender))
  (cider-log-ensure-appender)
  (let ((response (cider-sync-request:log-add-appender framework appender)))
    (nrepl-dbind-response response (status)
      (let ((framework-name (cider-log-framework-name framework))
            (appender-name (cider-log-appender-name appender)))
        (cond ((member "done" status)
               (message "Log appender %s added to the %s framework."
                        (cider-log--bold appender-name)
                        (cider-log--bold framework-name)))
              (t (message "Failed to add log appender %s to the %s framework."
                          (cider-log--bold appender-name)
                          (cider-log--bold framework-name))))))))

;;;###autoload
(defun cider-log-appender-clear (framework appender)
  "Clear the log events of the APPENDER of FRAMEWORK."
  (interactive)
  (let ((response (cider-sync-request:log-clear framework appender)))
    (nrepl-dbind-response response (status)
      (let ((framework-name (cider-log-framework-name framework))
            (appender-name (cider-log-appender-name appender)))
        (cond ((member "done" status)
               (cider-log-show-events)
               (message "Cleared the %s log appender of the %s framework."
                        (cider-log--bold appender-name)
                        (cider-log--bold framework-name)))
              (t (message "Failed to clear the %s log appender of the %s framework."
                          (cider-log--bold appender-name)
                          (cider-log--bold framework-name))))))))

;;;###autoload
(defun cider-log-appender-remove (framework appender)
  "Remove the log APPENDER from FRAMEWORK."
  (interactive (list cider-log-framework cider-log-appender))
  (let ((response (cider-sync-request:log-remove-appender framework appender)))
    (nrepl-dbind-response response (status)
      (let ((framework-name (cider-log-framework-name framework))
            (appender-name (cider-log-appender-name appender)))
        (cond ((member "done" status)
               (message "Log appender %s removed from the %s framework."
                        (cider-log--bold appender-name)
                        (cider-log--bold framework-name)))
              (t (message "Failed to remove log appender %s from the %s framework."
                          (cider-log--bold appender-name)
                          (cider-log--bold framework-name))))))))

;;;###autoload
(defun cider-log-show-events ()
  "Show the Cider log data."
  (interactive)
  (with-current-buffer (get-buffer-create cider-log-buffer-name)
    (cider-log-ensure-appender)
    (cider-log-refresh)
    (cider-log-mode)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun cider-log-reset ()
  "Reset the filters and show the log events."
  (interactive)
  (cider-log--reset-filters)
  (cider-log))

;;;###autoload (autoload 'cider-log "cider-log" "Show the Cider log menu." t)
(transient-define-prefix cider-log ()
  "Show the Cider log menu."
  ["Cider Log\n"
   (cider-log--framework-option)
   (cider-log--appender-option)]
  ["Filters:"
   (cider-log--end-time-filter)
   (cider-log--exception-filter)
   (cider-log--level-filter)
   (cider-log--logger-filter)
   (cider-log--pattern-filter)
   (cider-log--start-time-filter)
   (cider-log--thread-filter)]
  ["Actions"
   ("a" "Add log appender" cider-log-appender-add
    :inapt-if cider-log-appender-attached-p)
   ("c" "Clear log appender" cider-log-appender-clear
    :inapt-if-not cider-log-appender-attached-p)
   ("d" "Remove log appender" cider-log-appender-remove
    :inapt-if-not cider-log-appender-attached-p)
   ("l" "Show log events" cider-log-show-events
    :inapt-if-not cider-log-appender-attached-p)
   ("r" "Reset filters" cider-log-reset
    :inapt-if cider-log-filter-selected-p)])

(provide 'cider-log)

;;; cider-log.el ends here