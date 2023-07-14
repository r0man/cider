;;; cider-stateful-check.el --- Stateful Check Debugger -*- lexical-binding: t -*-

;; Copyright © 2023 r0man, Bozhidar Batsov and CIDER contributors

;; Author: r0man <roman@burningswell.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Stateful Check Debugger

;;; Commentary:

;; This major mode provides a debugger for Stateful Check specifications.  The
;; failing test cases of a Stateful Check run are rendered in an interactive
;; buffer.  Objects in that buffer, such as the arguments a command was invoked
;; with, the result of invoking the command and the execution state can be
;; viewed in the Cider Inspector.  The debugger also provides functionality to
;; step through the commands of a failing test case.

;;; Usage:

;; Run M-x stateful-check to open the Stateful Check transient menu.

;;; Code:

(require 'ansi-color)
(require 'button)
(require 'cider-client)
(require 'cider-common)
(require 'cider-inspector)
(require 'cider-mode)
(require 'cider-overlays)
(require 'cider-popup)
(require 'cider-stacktrace)
(require 'cider-test)
(require 'cl-lib)
(require 'easymenu)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defcustom cider-stateful-check-buffer "*stateful-check*"
  "The name of the Stateful Check buffer."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-stateful-check-auto-select-buffer t
  "Determines if the debugger buffer should be auto selected."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom cider-stateful-check-render-options t
  "Whether to render options in the debugger buffer."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom cider-stateful-check-thread-name-index "abcdefghijklmnopqrstuvwxzy"
  "The index used to pick thread names."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'stringp
  :type 'string)

(defcustom cider-stateful-check-gen-max-length 5
  "Specifies a max length for command sequences."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-gen-max-size 200
  "Specifies a maximum size for generated values."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-gen-threads 0
  "Specifies how many parallel threads to execute."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-run-assume-immutable-results-p nil
  "Specifies whether the runner should assume that the results of running commands are immutable."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom cider-stateful-check-run-max-tries 1
  "Specifies how attempts to make to fail a test."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-run-num-tests 200
  "Specifies how many tests to run."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-run-seed nil
  "Specifies the initial seed to use for generation."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-run-timeout-ms 0
  "Specifies the maximum number of milliseconds that a test is permitted to run for."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom cider-stateful-check-report-first-case-p t
  "Specifies whether to print the first failure."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom cider-stateful-check-report-command-frequency-p nil
  "Specifies whether to print information about how often each command was run."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom cider-stateful-check-render-execution 'cider-stateful-check--render-execution-long
  "Specifies a function that renders a command execution."
  :group 'cider-stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'symbolp
  :type 'symbol)

(defvar-local cider-stateful-check--current-run nil
  "The Stateful Check run of the current buffer.")

;; Stateful Check options

(defun cider-stateful-check-gen-options ()
  "Return the Stateful Check generation options."
  (nrepl-dict "max-length" cider-stateful-check-gen-max-length
              "max-size" cider-stateful-check-gen-max-size
              "threads" cider-stateful-check-gen-threads))

(defun cider-stateful-check-run-options ()
  "Return the Stateful Check run options."
  (nrepl-dict "assume-immutable-results"
              (if cider-stateful-check-run-assume-immutable-results-p "true" "false")
              "max-tries" cider-stateful-check-run-max-tries
              "num-tests" cider-stateful-check-run-num-tests
              "seed" cider-stateful-check-run-seed
              "timeout-ms" cider-stateful-check-run-timeout-ms))

(defun cider-stateful-check-report-options ()
  "Return the Stateful Check report options."
  (nrepl-dict "first-case?"
              (if cider-stateful-check-report-first-case-p "true" "false")
              "command-frequency?"
              (if cider-stateful-check-report-command-frequency-p "true" "false")))

(defun cider-stateful-check-options ()
  "Return the Stateful Check options."
  (nrepl-dict "gen" (cider-stateful-check-gen-options)
              "report" (cider-stateful-check-report-options)
              "run" (cider-stateful-check-run-options)))

;; NREPL operations

(defun cider-sync-request:stateful-check-analysis (id)
  "Return Stateful Check analysis by ID."
  (cider-ensure-op-supported "stateful-check/analysis")
  (thread-first `("op" "stateful-check/analysis"
                  "id" ,id)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/analysis")))

(defun cider-sync-request:stateful-check-analyze-test (test)
  "Analyze Stateful Check TEST report."
  (cider-ensure-op-supported "stateful-check/analyze-test")
  (thread-first `("op" "stateful-check/analyze-test"
                  "test" ,test)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/analyze-test")))

(defun cider-sync-request:stateful-check-inspect (query)
  "Inspect the Stateful Check test run object for the QUERY."
  (cider-ensure-op-supported "stateful-check/inspect")
  (thread-first `("op" "stateful-check/inspect"
                  "query" ,query)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:stateful-check-print (index)
  "Print the Stateful Check test run object at INDEX."
  (cider-ensure-op-supported "stateful-check/print")
  (thread-first `("op" "stateful-check/print"
                  "index" ,index)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/print")))

(defun cider-sync-request:stateful-check-scan ()
  "Scan public vars and test reports for Stateful Check specifications."
  (cider-ensure-op-supported "stateful-check/scan")
  (thread-first `("op" "stateful-check/scan")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/scan")))

(defun cider-sync-request:stateful-check-specifications ()
  "List all known Stateful Check specifications."
  (cider-ensure-op-supported "stateful-check/specifications")
  (thread-first `("op" "stateful-check/specifications")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/specifications")))

(defun cider-request:stateful-check-eval-step (run case callback)
  "Evaluate the current command for the failing CASE of RUN."
  (cider-ensure-op-supported "stateful-check/eval-step")
  (thread-first `("op" "stateful-check/eval-step"
                  "run" ,run
                  "case", (or case "smallest"))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-eval-stop (run case callback)
  "Stop the evaluation of the failing CASE of RUN."
  (cider-ensure-op-supported "stateful-check/eval-stop")
  (thread-first `("op" "stateful-check/eval-stop"
                  "run" ,run
                  "case", (or case "smallest"))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-stacktrace (query callback)
  "Request the stacktrace of Stateful Check run matching QUERY and invoke CALLBACK."
  (cider-ensure-op-supported "stateful-check/stacktrace")
  (thread-first `("op" "stateful-check/stacktrace"
                  "query" ,query)
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-test (test-report callback)
  "Run Stateful Check TEST-REPORT and invoke CALLBACK."
  (thread-first `("op" "test-var-query"
                  "ns" ,(cider-stateful-check--run-ns test-report)
                  "tests" ,(list (cider-stateful-check--run-var test-report)))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-run (specification options callback)
  "Run the Stateful Check SPECIFICATION using OPTIONS and invoke CALLBACK."
  (cider-ensure-op-supported "stateful-check/run")
  (thread-first `("op" "stateful-check/run"
                  "specification" ,(cider-stateful-check--specification-id specification)
                  "options" ,options)
                (cider-nrepl-send-request callback)))

;; Misc

(defun cider-stateful-check--delete-property-region (property)
  "Delete region of at which PROPERTY changes, relative to the current point."
  (let ((start (previous-single-property-change (point) property))
        (end (next-single-property-change (point) property)))
    (when (and start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (list start end)))))

(defun cider-stateful-check--strip-handle (handle)
  "Strip the Stateful Check HANDLE of its angle brackets."
  (when handle (string-trim (string-trim handle "#") "<" ">")))

(defun cider-stateful-check--thread-name (index)
  "Return the thread name from `cider-stateful-check-thread-name-index' for INDEX."
  (char-to-string (elt cider-stateful-check-thread-name-index index)))

(defun cider-stateful-check--run-id-at-point ()
  "Return the Stateful Check test run id at point, or nil."
  (when-let ((ns (get-text-property (point) 'ns))
             (var (get-text-property (point) 'var)))
    (format "%s/%s" ns var)))

;; Specification

(defun cider-stateful-check--specification-id (specification)
  "Return the id of the SPECIFICATION."
  (nrepl-dict-get specification "id"))

;; Run

(defun cider-stateful-check--run-specification (run)
  "Return the specification of the Stateful Check RUN."
  (nrepl-dict-get-in run '("specification")))

(defun cider-stateful-check--run-options (run)
  "Return the options used for the Stateful Check RUN."
  (nrepl-dict-get-in run '("options")))

(defun cider-stateful-check--run-pass-p (run)
  "Return non-nil if Stateful Check RUN passed, otherwise nil."
  (equal "true" (nrepl-dict-get run "pass?")))

(defun cider-stateful-check--run-replace (run)
  "Replace the Stateful Check run at point with RUN."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (column (current-column)))
    (when (cider-stateful-check--delete-property-region 'cider-stateful-check-run)
      ;; Render into a temp buffer first, to avoid issues with `insert-rectangle'
      ;; messing up text after the replaced region.
      (insert (with-temp-buffer
                (cider-stateful-check--insert-run run)
                (buffer-string)))
      (goto-char (point-min))
      (forward-line (- line 1))
      (move-to-column column))))

;; Run

(defun cider-stateful-check--run-ns (run)
  "Return the namespace of the RUN."
  (nrepl-dict-get-in run '("specification" "ns")))

(defun cider-stateful-check--run-var (run)
  "Return the namespace of the RUN."
  (nrepl-dict-get-in run '("specification" "var")))

(defun cider-stateful-check--run-name (run)
  "Return the namespace of the RUN."
  (when-let ((ns (cider-stateful-check--run-ns run))
             (var (cider-stateful-check--run-var run)))
    (format "%s/%s" ns var)))

(defun cider-stateful-check--run-first-case (run)
  "Return the first failing case from the Stateful Check RUN."
  (nrepl-dict-get-in run '("result-data")))

(defun cider-stateful-check--run-smallest-case (run)
  "Return the smallest failing case from the Stateful Check RUN."
  (nrepl-dict-get-in run '("shrunk" "result-data")))

(defun cider-stateful-check--run-sort (runs)
  "Sort the Stateful Check RUNS by namespace and var."
  (seq-sort-by (lambda (run)
                 (format "%s/%s"
                         (nrepl-dict-get run "ns")
                         (nrepl-dict-get run "var")))
               #'string< runs))

(defun cider-stateful-check--read-specification-id (specifications)
  "Read one of the Stateful Check specification name from SPECIFICATIONS, with completion."
  (completing-read "Stateful Check Specification: "
                   (seq-map #'cider-stateful-check--specification-id specifications)))

(defun cider-stateful-check--read-specification (specifications)
  "Read one of the Stateful Check SPECIFICATIONS, with completion."
  (let ((id (cider-stateful-check--read-specification-id specifications)))
    (seq-find (lambda (specification)
                (equal id (cider-stateful-check--specification-id specification)))
              specifications)))

;; Render

(defun cider-stateful-check--render-handle (failing-case execution)
  "Render the Stateful Check EXECUTION handle of the FAILING-CASE."
  (nrepl-dbind-response execution (handle result)
    (nrepl-dbind-response result (evaluation)
      (let ((eval-p (cider-stateful-check--failing-case-eval-p failing-case))
            (states (cider-stateful-check--eval-states failing-case))
            (state (cider-stateful-check--strip-handle handle)))
        (cond ((and eval-p (member state states))
               (cider-insert handle 'bold))
              ((and eval-p evaluation)
               (insert handle))
              ((and eval-p (not evaluation))
               (cider-insert handle 'font-lock-comment-face))
              (t (insert handle)))))))

(defun cider-stateful-check--render-argument (argument)
  "Render the Stateful Check command ARGUMENT."
  (when (nrepl-dict-p argument)
    (nrepl-dbind-response argument (index symbolic)
      (cider-propertize-region
          (list 'cider-stateful-check-argument argument
                'cider-value-idx index
                'mouse-face 'highlight)
        (insert (cider-font-lock-as-clojure symbolic))))))

(defun cider-stateful-check--render-failure-event (failing-case event)
  "Render the test assertion EVENT of FAILING-CASE into the current buffer."
  (nrepl-dbind-response event (context message expected actual diffs error gen-input)
    (cl-flet ((insert-label (s)
                (cider-insert (format "%14s: " s) 'font-lock-comment-face))
              (insert-align-label (s)
                (insert (format "%18s" s)))
              (insert-rect (s)
                (cl-letf (((symbol-function 'message) (lambda (_ &rest _))))
                  (let ((start (point)))
                    (insert-rectangle (thread-first
                                        s
                                        cider-font-lock-as-clojure
                                        (split-string "\n")))
                    (ansi-color-apply-on-region start (point)))
                  (beginning-of-line))))
      (cider-propertize-region (list 'cider-stateful-check-test-event event)
        (let ((beg (point))
              (bg `(:background ,cider-test-items-background-color :extend t)))
          (when context  (cider-insert context 'font-lock-doc-face t))
          (when message  (cider-insert message 'font-lock-string-face t))
          (when expected
            (insert-label "expected")
            (insert-rect expected))
          (if diffs
              (dolist (d diffs)
                (cl-destructuring-bind (actual (removed added)) d
                  (insert-label "actual")
                  (insert-rect actual)
                  (insert-label "diff")
                  (insert "- ")
                  (insert-rect removed)
                  (insert-align-label "+ ")
                  (insert-rect added)))
            (when actual
              (insert-label "actual")
              (insert-rect actual)))
          (when error
            (insert-label "error")
            (cider-propertize-region
                (list 'cider-value-idx error
                      'mouse-face 'highlight)
              (cider-stateful-check--render-error
               failing-case (nrepl-dict "error" (nrepl-dict "real" error))))
            (insert "\n"))
          (when gen-input
            (insert-label "input")
            (insert (cider-font-lock-as-clojure gen-input)))
          (overlay-put (make-overlay beg (point)) 'font-lock-face bg))))))

(defun cider-stateful-check--render-failure (failing-case failure)
  "Render the Stateful Check post-condition FAILURE."
  (when (nrepl-dict-p failure)
    (nrepl-dbind-response failure (events message)
      (cider-propertize-region (list 'cider-stateful-check-failure failure)
        (if (zerop (length events))
            (when message
              (insert (format "      %s\n" message)))
          (seq-doseq (event events)
            (cider-stateful-check--render-failure-event failing-case event)))))))

(defun cider-stateful-check--render-failures (failing-case failures)
  "Render the evaluation FAILURES of the FAILING-CASE."
  (nrepl-dbind-response failures (evaluation real)
    (let ((eval-p (cider-stateful-check--failing-case-eval-p failing-case))
          (start (point)))
      (cond
       ((and evaluation)
        (seq-doseq (failure evaluation)
          (cider-stateful-check--render-failure failing-case failure)))
       ((and eval-p real)
        (seq-doseq (failure real)
          (cider-stateful-check--render-failure failing-case failure))
        (add-text-properties start (point) (list 'face 'font-lock-comment-face)))
       ((and (not eval-p) real)
        (seq-doseq (failure real)
          (cider-stateful-check--render-failure failing-case failure)))))))

(defun cider-stateful-check--show-error ()
  "Show the error at point in the stacktrace navigator."
  (when-let (query (cider-stateful-check--query-at-point))
    (let (causes)
      (cider-request:stateful-check-stacktrace
       query (lambda (response)
               (nrepl-dbind-response response (class status)
                 (cond (class  (setq causes (cons response causes)))
                       (status (when causes
                                 (cider-stacktrace-render
                                  (cider-popup-buffer cider-error-buffer
                                                      cider-auto-select-error-buffer
                                                      #'cider-stacktrace-mode
                                                      'ancillary)
                                  (reverse causes)))))))))))

(defun cider-stateful-check--failing-case-assume-immutable-results-p (failing-case)
  "Return non-nil if FAILING-CASE was run with the assume-immutable-results option."
  (equal "true" (nrepl-dict-get-in failing-case '("options" "run" "assume-immutable-results"))))

(defun cider-stateful-check--render-result (failing-case execution)
  "Render the EXECUTION result of a Stateful Check FAILING-CASE."
  (let ((eval-p (cider-stateful-check--failing-case-eval-p failing-case))
        (immutable-results-p (cider-stateful-check--failing-case-assume-immutable-results-p failing-case)))
    (nrepl-dbind-response execution (result)
      (nrepl-dbind-response result (evaluation real real-str real-mutated? real-mutated)
        (cider-propertize-region
            (list 'cider-stateful-check-result result
                  'cider-value-idx result
                  'mouse-face 'highlight)
          (cond
           ((and (not immutable-results-p) real-mutated)
            (insert (cider-font-lock-as-clojure real-str))
            (insert "\n")
            (cider-insert
             (format "      >> object may have been mutated later into %s <<"
                     real-mutated)
             'font-lock-type-face))
           ((and real-mutated? real-str)
            (insert (cider-font-lock-as-clojure real-str)))
           ((and eval-p evaluation)
            (insert (cider-font-lock-as-clojure evaluation)))
           ((and eval-p real)
            (insert (cider-propertize real 'font-lock-comment-face)))
           (real (insert (cider-font-lock-as-clojure real)))))))))

(defun cider-stateful-check--render-error-button (exception)
  "Render the EXCEPTION as a text button."
  (insert-text-button exception
                      'follow-link t
                      'action '(lambda (_button) (cider-stateful-check--show-error))
                      'help-echo "View causes and stacktrace"))

(defun cider-stateful-check--render-error (failing-case execution)
  "Render the EXECUTION error of a Stateful Check FAILING-CASE."
  (nrepl-dbind-response execution (error result)
    (nrepl-dbind-response error (evaluation real)
      (let ((eval-p (cider-stateful-check--failing-case-eval-p failing-case))
            (eval-result-p (and result (nrepl-dict-contains result "evaluation"))))
        (cider-propertize-region
            (list 'cider-stateful-check-error error
                  'cider-value-idx error
                  'mouse-face 'highlight)
          (cond ((and eval-p evaluation)
                 (cider-stateful-check--render-error-button evaluation))
                ((and eval-p (not eval-result-p) real)
                 (cider-stateful-check--render-error-button
                  (cider-propertize real 'font-lock-comment-face)))
                ((and (not eval-result-p) real)
                 (cider-stateful-check--render-error-button real))))))))

(defun cider-stateful-check--render-execution-short (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE in short form."
  (nrepl-dbind-response execution (arguments command failures)
    (cider-propertize-region (list 'cider-stateful-check-execution execution)
      (insert "    ")
      (cider-stateful-check--render-handle failing-case execution)
      (insert (cider-propertize " = " 'font-lock-comment-face)
              (cider-propertize "(" 'paren-face))
      (nrepl-dbind-response command (name)
        (insert (cider-propertize name 'var)))
      (seq-doseq (argument arguments)
        (insert " ")
        (cider-stateful-check--render-argument argument))
      (insert (cider-propertize ")" 'paren-face)
              (cider-propertize " = " 'font-lock-comment-face))
      (cider-stateful-check--render-result failing-case execution)
      (cider-stateful-check--render-error failing-case execution)
      (insert "\n")
      (cider-stateful-check--render-failures failing-case failures))))

(defun cider-stateful-check--render-execution-long (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE in long form."
  (nrepl-dbind-response execution (arguments command failures)
    (cider-propertize-region (list 'cider-stateful-check-execution execution)
      (insert "    ")
      (cider-stateful-check--render-handle failing-case execution)
      (insert (cider-propertize " = " 'font-lock-comment-face)
              (cider-propertize "(" 'paren-face))
      (nrepl-dbind-response command (name)
        (insert (cider-propertize name 'var)))
      (seq-doseq (argument arguments)
        (insert " ")
        (cider-stateful-check--render-argument argument))
      (insert (cider-propertize ")" 'paren-face))
      (insert "\n      ")
      (cider-insert "=>" 'font-lock-comment-face)
      (insert " ")
      (cider-stateful-check--render-result failing-case execution)
      (cider-stateful-check--render-error failing-case execution)
      (insert "\n")
      (cider-stateful-check--render-failures failing-case failures))))

(defun cider-stateful-check--render-execution (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE."
  (funcall cider-stateful-check-render-execution failing-case execution))

(defun cider-stateful-check--render-sequential-executions (failing-case executions)
  "Render the sequential Stateful Check EXECUTIONS of the FAILING-CASE."
  (cider-propertize-region (list 'cider-stateful-check-sequential-executions executions)
    (cider-insert "\n  Sequential prefix:" 'bold t)
    (cider-insert "  ---------------------------\n" 'font-lock-comment-face)
    (seq-doseq (execution executions)
      (cider-stateful-check--render-execution failing-case execution))))

(defun cider-stateful-check--render-parallel-executions (failing-case executions)
  "Render the parallel Stateful Check EXECUTIONS of the FAILING-CASE."
  (cider-propertize-region (list 'cider-stateful-check-parallel-executions executions)
    (seq-map-indexed (lambda (executions index)
                       (let ((thread (cider-stateful-check--thread-name index)))
                         (cider-insert (format "  Thread %s:" thread) 'bold t)
                         (cider-insert "  ---------------------------\n" 'font-lock-comment-face)
                         (seq-doseq (execution executions)
                           (cider-stateful-check--render-execution failing-case execution))
                         (insert "\n")))
                     executions)))

(defun cider-stateful-check--render-executions (failing-case executions)
  "Render the sequential and parallel Stateful Check EXECUTIONS of the FAILING-CASE."
  (nrepl-dbind-response executions (sequential parallel)
    (cider-stateful-check--render-sequential-executions failing-case sequential)
    (when (cl-plusp (length parallel))
      (insert "\n")
      (cider-stateful-check--render-parallel-executions failing-case parallel))))

(defun cider-stateful-check--failing-case-eval-p (failing-case)
  "Return non-nil if FAILING-CASE is being evaluated, otherwise nil."
  (equal "true" (nrepl-dict-get failing-case "eval?")))

(defun cider-stateful-check--eval-states (failing-case)
  "Return the evaluation state of the FAILING-CASE or nil."
  (nrepl-dict-get-in failing-case '("state-machine" "state")))

(defun cider-stateful-check--render-eval-banner (failing-case)
  "Renders an [EVAL] banner if the failing FAILING-CASE is currently evaluated."
  (when (cider-stateful-check--failing-case-eval-p failing-case)
    (cider-insert " [EVAL]" 'font-lock-comment-face)))

(defun cider-stateful-check--render-first (run)
  "Render the Stateful Check RUN for the first failing case."
  (let* ((failing-case (cider-stateful-check--run-first-case run))
         (executions (nrepl-dict-get failing-case "executions")))
    (cider-propertize-region (list 'cider-stateful-check-first-case executions)
      (cider-insert "First failing test case" 'bold)
      (cider-stateful-check--render-eval-banner failing-case)
      (cider-insert "\n-----------------------\n" 'font-lock-comment-face)
      (cider-stateful-check--render-executions failing-case executions))))

(defun cider-stateful-check--render-smallest (run)
  "Render the Stateful Check RUN for the smallest failing case."
  (let* ((failing-case (cider-stateful-check--run-smallest-case run))
         (executions (nrepl-dict-get failing-case "executions")))
    (cider-propertize-region (list 'cider-stateful-check-smallest-case executions)
      (cider-insert "Smallest case after shrinking" 'bold)
      (cider-stateful-check--render-eval-banner failing-case)
      (cider-insert "\n-----------------------------\n")
      (cider-stateful-check--render-executions failing-case executions))))

(defun cider-stateful-check--render-generation-options (options)
  "Render the Stateful Check generation OPTIONS."
  (nrepl-dbind-response options (max-length max-size threads)
    (cider-insert "  Generation: " 'bold t)
    (insert (format "    Max Length ......... %s\n" max-length))
    (insert (format "    Max Size ........... %s\n" max-size))
    (insert (format "    Threads ............ %s\n" threads))))

(defun cider-stateful-check--render-run-options (options)
  "Render the Stateful Check run OPTIONS."
  (nrepl-dbind-response options (assume-immutable-results max-tries num-tests seed timeout-ms)
    (cider-insert "  Run: " 'bold t)
    (insert (format "    Immutable Results .. %s\n" assume-immutable-results))
    (insert (format "    Max Tries .......... %s\n" max-tries))
    (insert (format "    Num Tests .......... %s\n" num-tests))
    (insert (format "    Seed ............... %s\n" seed))
    (insert (format "    Timeout (ms) ....... %s\n" timeout-ms))))

(defun cider-stateful-check--render-report-options (options)
  "Render the Stateful Check report OPTIONS."
  (nrepl-dbind-response options (command-frequency? first-case?)
    (cider-insert "  Report: " 'bold t)
    (insert (format "    Command Frequency .. %s\n" command-frequency?))
    (insert (format "    First Case ......... %s\n" first-case?))))

(defun cider-stateful-check--render-options (options)
  "Render the Stateful Check OPTIONS."
  (nrepl-dbind-response options (gen report run)
    (cider-propertize-region (list 'cider-stateful-check-options options)
      (cider-insert "Options: " 'bold t)
      (cider-stateful-check--render-generation-options gen)
      (cider-stateful-check--render-run-options run)
      (cider-stateful-check--render-report-options report)
      (insert "\n"))))

(defun cider-stateful-check--render-footer (run)
  "Render the Stateful Check RUN footer."
  (nrepl-dbind-response run (options)
    (cider-propertize-region (list 'cider-stateful-check-footer run)
      (unless cider-stateful-check-render-options
        (when-let (seed (nrepl-dict-get-in run '("seed")))
          (insert "\n")
          (cider-insert "Seed: " 'bold)
          (insert (format "%s\n"seed))))
      (unless (zerop (length (nrepl-dict-get-in run'("result-data" "executions" "parallel"))))
        (insert "\n")
        (cider-insert "Note: " 'bold)
        (cider-insert "Test cases with multiple threads are not deterministic, so using the\n"
                      'font-lock-comment-face)
        (cider-insert "      same seed does not guarantee the same result.\n"
                      'font-lock-comment-face))
      (insert "\n")
      (when cider-stateful-check-render-options
        (cider-stateful-check--render-options options)))))

(defun cider-stateful-check--success-message (run)
  "Return the Stateful Check success message for RUN."
  (nrepl-dbind-response run (num-tests time-elapsed-ms)
    (propertize (format "Specification passed. Ran %s test%s in %s ms."
                        num-tests
                        (if (= 1 num-tests) "" "s")
                        time-elapsed-ms)
                'face 'cider-test-success-face)))

(defun cider-stateful-check--failure-message (run)
  "Return the Stateful Check failure message for RUN."
  (nrepl-dbind-response run (failed-after-ms num-tests shrunk)
    (nrepl-dbind-response shrunk (time-shrinking-ms)
      (propertize (format "Specification failed. Ran %s test%s in %s ms." num-tests
                          (if (= 1 num-tests) "" "s")
                          (+ failed-after-ms time-shrinking-ms))
                  'face 'cider-test-failure-face))))

(defun cider-stateful-check--render-header (run)
  "Render the Stateful Check header for RUN."
  (cider-insert "Stateful Check Summary" 'bold t)
  (when-let ((ns (cider-stateful-check--run-ns run))
             (var (cider-stateful-check--run-var run)))
    (insert (cider-propertize ns 'ns))
    (cider-insert "/" 'font-lock-comment-face)
    (cider-insert (cider-propertize var 'var))
    (insert "\n"))
  (insert "\n")
  (insert
   (if (cider-stateful-check--run-pass-p run)
       (cider-stateful-check--success-message run)
     (cider-stateful-check--failure-message run)))
  (insert "\n")
  (insert "\n"))

(defun cider-stateful-check--insert-run (run)
  "Insert the Stateful Check RUN into current buffer."
  (cider-propertize-region (list 'cider-stateful-check-run run)
    (unless (cider-stateful-check--run-pass-p run)
      (cider-stateful-check--render-smallest run)
      (when cider-stateful-check-report-first-case-p
        (insert "\n")
        (cider-stateful-check--render-first run)))))

(defun cider-stateful-check--render-run (buffer run)
  "Render the Stateful Check RUN into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq cider-stateful-check--current-run run)
      (cider-stateful-check--render-header run)
      (cider-stateful-check--insert-run run)
      (cider-stateful-check--render-footer run))))

(defun cider-stateful-check--render-test-event (event)
  "Render the CIDER test EVENT."
  (when-let (run (nrepl-dict-get event "stateful-check"))
    (nrepl-dbind-response event (ns var type)
      (cider-propertize-region (cider-intern-keys (cdr event))
        (let ((run (cider-sync-request:stateful-check-analyze-test (format "%s/%s" ns var)))
              (type-face (cider-test-type-simple-face type)))
          (cider-insert (capitalize type) type-face nil " in ")
          (cider-insert var 'font-lock-function-name-face t)
          (cider-stateful-check--insert-run run)
          (cider-stateful-check--render-footer run)
          (cider-stateful-check-test-report-mode))))))

(defun cider-stateful--show-run (run)
  "Show the Stateful Check RUN in a popup buffer."
  (cider-make-popup-buffer cider-stateful-check-buffer 'cider-stateful-check-mode 'ancillary)
  (with-current-buffer cider-stateful-check-buffer
    (cider-stateful-check--render-run cider-stateful-check-buffer run)
    (cider-popup-buffer-display (current-buffer) cider-stateful-check-auto-select-buffer)
    (goto-char (point-min))
    (cider-stateful-check--next-thing 'cider-stateful-check-execution)))

(defun cider-stateful-check--query-at-point ()
  "Return a NREPL dictionary describing the thing at point."
  (when-let (run (get-text-property (point) 'cider-stateful-check-run))
    (let ((query (nrepl-dict "run" (nrepl-dict-get run "id"))))
      (when (get-text-property (point) 'cider-stateful-check-first-case)
        (nrepl-dict-put query "case" "first"))
      (when (get-text-property (point) 'cider-stateful-check-smallest-case)
        (nrepl-dict-put query "case" "smallest"))
      (when-let (execution (get-text-property (point) 'cider-stateful-check-execution))
        (nrepl-dict-put query "handle" (nrepl-dict-get execution "handle"))
        (when-let (argument (get-text-property (point) 'cider-stateful-check-argument))
          (nrepl-dict-put query "argument" (nrepl-dict-get argument "index")))
        (when-let (argument (get-text-property (point) 'cider-stateful-check-result))
          (nrepl-dict-put query "result" "true"))
        (when-let (failure (get-text-property (point) 'cider-stateful-check-failure))
          (nrepl-dict-put query "failure" (nrepl-dict-get failure "index")))
        (when-let (event (get-text-property (point) 'cider-stateful-check-test-event))
          (nrepl-dict-put query "event" (nrepl-dict-get event "index"))))
      query)))

(defun cider-stateful-check-inspect (query)
  "Inspect the Stateful Check run object described by QUERY."
  (when-let (value (cider-sync-request:stateful-check-inspect query))
    (cider-inspector--render-value value)))

(defun cider-stateful-check-value-at-point ()
  "Return the Stateful Check test run value at point."
  (get-text-property (point) 'cider-stateful-check-value))

(defun cider-stateful-check-print-value-at-point (value)
  "Pretty print the Stateful Check test run VALUE at point."
  (interactive (list (cider-stateful-check-value-at-point)))
  (when (nrepl-dict-p value)
    (nrepl-dbind-response value (cursor)
      (let ((display-value (cider-sync-request:stateful-check-print cursor)))
        (cider-popup-buffer cider-result-buffer 'clojure-mode)
        (with-current-buffer cider-result-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%s" display-value))
            (goto-char (point-min))))))))

(defun cider-stateful-check-operate-on-point ()
  "Invoke the command for the object at point."
  (interactive)
  (when-let (query (cider-stateful-check--query-at-point))
    (cider-stateful-check-inspect query)))

(defun cider-stateful-check-operate-on-click (event)
  "Move to EVENT's position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point (or (get-text-property point 'cider-value-idx)))
           (goto-char point)
           (cider-stateful-check-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun cider-stateful-check--specifications ()
  "Scan loaded namespaces and test reports for Stateful Check specifications."
  (cider-sync-request:stateful-check-scan)
  (cider-sync-request:stateful-check-specifications))

(defun cider-stateful-check--test-specifications ()
  "Return the Stateful Check specifications from the CIDER test report."
  (seq-filter (lambda (specification)
                (equal "test" (nrepl-dict-get specification "type")))
              (cider-stateful-check--specifications)))

;;;###autoload
(transient-define-suffix cider-stateful-check-run (specification options)
  "Run the Stateful Check SPECIFICATION using OPTIONS."
  :description "Run specification"
  (interactive (list (cider-stateful-check--read-specification (cider-stateful-check--specifications))
                     (cider-stateful-check-options)))
  (nrepl-dbind-response specification (ns var)
    (message "Running specification %s in %s ..." (cider-propertize var 'bold) (cider-propertize ns 'ns))
    (cider-request:stateful-check-run
     specification options
     (lambda (response)
       (nrepl-dbind-response response (status stateful-check/run)
         (cond (stateful-check/run
                (let ((pass? (nrepl-dict-get stateful-check/run "pass?")))
                  (cond ((equal "true" pass?)
                         (if cider-test-show-report-on-success
                             (cider-stateful--show-run stateful-check/run)
                           (cider-stateful-check--render-run cider-stateful-check-buffer stateful-check/run))
                         (message (cider-stateful-check--success-message stateful-check/run)))
                        ((equal "false" pass?)
                         (cider-stateful--show-run stateful-check/run)
                         (cider-stateful-check-next-execution)
                         (message (cider-stateful-check--failure-message stateful-check/run))))))
               ((member "stateful-check/run-error" status)
                (message "Error while running Stateful Check specification."))))))))

(defun cider-stateful-check--rerun-options ()
  "Return the options to re-run a Stateful Check specification."
  (or (and current-prefix-arg (cider-stateful-check-options))
      (with-current-buffer cider-stateful-check-buffer
        (cider-stateful-check--run-options
         cider-stateful-check--current-run))
      (cider-stateful-check-options)))

;;;###autoload
(transient-define-suffix cider-stateful-check-rerun (options)
  "Rerun the Stateful Check specification with OPTIONS."
  :description "Rerun specification"
  (interactive (list (cider-stateful-check--rerun-options)))
  (if (get-buffer cider-stateful-check-buffer)
      (with-current-buffer cider-stateful-check-buffer
        (if-let (specification (cider-stateful-check--run-specification cider-stateful-check--current-run))
            (cider-stateful-check-run specification options)
          (user-error "No Stateful Check specification to re-run")))
    (user-error "No Stateful Check specification to re-run")))

;;;###autoload
(transient-define-suffix cider-stateful-check-scan ()
  "Scan all public vars and test runs for Stateful Check specifications."
  :description "Scan vars and test reports for specifications"
  (interactive)
  (let ((old-specs (cider-sync-request:stateful-check-specifications))
        (new-specs (cider-sync-request:stateful-check-scan)))
    (message "[%s/%s] Stateful Check specification scan done."
             (cider-propertize (number-to-string (- (length new-specs) (length old-specs))) 'bold)
             (cider-propertize (number-to-string (length new-specs)) 'bold))))

;;;###autoload
(defun cider-stateful-check-eval-step ()
  "Evaluate the command of a Stateful Check run."
  (interactive)
  (when-let (query (cider-stateful-check--query-at-point))
    (nrepl-dbind-response query (run case)
      (let ((buffer (current-buffer)))
        (cider-request:stateful-check-eval-step
         run case
         (lambda (response)
           (nrepl-dbind-response response (out err status stateful-check/eval-step)
             (cond (err (cider-emit-interactive-eval-err-output err))
                   (out (cider-emit-interactive-eval-output out))
                   (stateful-check/eval-step
                    (let ((run stateful-check/eval-step))
                      (with-current-buffer buffer
                        (setq cider-stateful-check--current-run run)
                        (cider-stateful-check--run-replace run))))
                   ((member "stateful-check/eval-step-error" status)
                    (message "Failed to evaluate Stateful Check command."))))))))))

;;;###autoload
(defun cider-stateful-check-eval-stop ()
  "Stop the evaluation of a Stateful Check run."
  (interactive)
  (when-let (query (cider-stateful-check--query-at-point))
    (nrepl-dbind-response query (run case)
      (let ((buffer (current-buffer)))
        (cider-request:stateful-check-eval-stop
         run case
         (lambda (response)
           (nrepl-dbind-response response (out err status stateful-check/eval-stop)
             (cond (err (cider-emit-interactive-eval-err-output err))
                   (out (cider-emit-interactive-eval-output out))
                   (stateful-check/eval-stop
                    (let ((run stateful-check/eval-stop))
                      (with-current-buffer buffer
                        (setq cider-stateful-check--current-run run)
                        (cider-stateful-check--run-replace run))))
                   ((member "stateful-check/eval-stop-error" status)
                    (message "Failed to stop the Stateful Check evaluation."))))))))))

;;;###autoload
(defun cider-stateful-check-toggle-first-case ()
  "Toggle the display of the first failing case."
  (interactive)
  (when-let (run cider-stateful-check--current-run)
    (setq cider-stateful-check-report-first-case-p
          (not cider-stateful-check-report-first-case-p))
    (cider-stateful-check--run-replace run)))

;;;###autoload
(defun cider-stateful-check-toggle-render-options ()
  "Toggle the display of the render options."
  (interactive)
  (when-let (run cider-stateful-check--current-run)
    (setq cider-stateful-check-render-options
          (not cider-stateful-check-render-options))
    (cider-stateful-check--run-replace run)))

(defun cider-stateful-check--next-thing (thing)
  "Move point to the next THING, a text property symbol, if one exists."
  (interactive)
  (when-let* ((pos (next-single-property-change (point) thing)))
    (if (get-text-property pos thing)
        (goto-char pos)
      (when-let* ((pos (next-single-property-change pos thing)))
        (goto-char pos)))))

(defun cider-stateful-check--previous-thing (thing)
  "Move point to the previous THING, a text property symbol, if one exists."
  (interactive)
  (when-let* ((pos (previous-single-property-change (point) thing)))
    (if (get-text-property pos thing)
        (goto-char pos)
      (when-let* ((pos (previous-single-property-change pos thing)))
        (goto-char pos)))))

(defun cider-stateful-check-next-execution ()
  "Move point to the next command execution, if one exists."
  (interactive)
  (cider-stateful-check--next-thing 'cider-stateful-check-execution))

(defun cider-stateful-check-previous-execution ()
  "Move point to the previous command execution, if one exists."
  (interactive)
  (cider-stateful-check--previous-thing 'cider-stateful-check-execution))

(defun cider-stateful-check-show-test-report (id)
  "Show the Stateful Check test report ID at point in the debugger."
  (interactive (list (cider-stateful-check--run-id-at-point)))
  (when id
    (when-let (run (cider-sync-request:stateful-check-analyze-test id))
      (cider-stateful--show-run run))))

(defun cider-stateful-check--define-menu (keymap)
  "Define a Stateful Check menu for the KEYMAP."
  (easy-menu-define cider-stateful-check-mode-menu keymap
    "Menu for CIDER's Stateful Check debugger."
    `("CIDER Stateful Check"
      ["Re-run specification" cider-stateful-check-rerun]
      ["Run specification" cider-stateful-check-run]
      ["Scan specifications" cider-stateful-check-scan]
      "--"
      ["Inspect object at point" cider-stateful-check-operate-on-point]
      ["Next Inspectable Object" cider-inspector-next-inspectable-object]
      ["Previous Inspectable Object" cider-inspector-previous-inspectable-object]
      "--"
      ["Quit" cider-popup-buffer-quit-function])))

(defvar cider-stateful-check-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (cider-stateful-check--define-menu map)
    (define-key map "P" #'cider-stateful-check-print-value-at-point)
    (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
    (define-key map "b" #'backward-char)
    (define-key map "d" #'cider-test-ediff)
    (define-key map "e" #'cider-stateful-check-eval-step)
    (define-key map "f" #'forward-char)
    (define-key map "g" #'cider-stateful-check-rerun)
    (define-key map "k" #'cider-stateful-check-eval-stop)
    (define-key map "n" #'cider-stateful-check-next-execution)
    (define-key map "o" #'cider-stateful-check-toggle-render-options)
    (define-key map "p" #'cider-stateful-check-previous-execution)
    (define-key map "t" #'cider-stateful-check-toggle-first-case)
    (define-key map "x" #'cider-stateful-check)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
    (define-key map (kbd "RET") #'cider-stateful-check-operate-on-point)
    (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
    (define-key map [mouse-1] #'cider-stateful-check-operate-on-click)
    (define-key map [tab] #'cider-inspector-next-inspectable-object)
    ;; Emacs translates S-TAB to BACKTAB on X.
    (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
    map))

(define-derived-mode cider-stateful-check-mode special-mode "Stateful Check"
  "Major mode for debugging Stateful Check specifications."
  (set-syntax-table clojure-mode-syntax-table)
  (setq-local cider-inspector-skip-uninteresting nil)
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (setq-local truncate-lines t))

(define-minor-mode cider-stateful-check-test-report-mode
  "Minor mode for debugging Stateful Check specifications in the CIDER test report."
  :keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map cider-popup-buffer-mode-map)
            (cider-stateful-check--define-menu map)
            (define-key map "P" #'cider-stateful-check-print-value-at-point)
            (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
            (define-key map "b" #'backward-char)
            (define-key map "d" #'cider-test-ediff)
            (define-key map "e" #'cider-stateful-check-eval-step)
            (define-key map "f" #'forward-char)
            (define-key map "k" #'cider-stateful-check-eval-stop)
            (define-key map "n" #'cider-stateful-check-next-execution)
            (define-key map "o" #'cider-stateful-check-toggle-render-options)
            (define-key map "p" #'cider-stateful-check-previous-execution)
            (define-key map "t" #'cider-stateful-check-toggle-first-case)
            (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
            (define-key map (kbd "RET") #'cider-stateful-check-operate-on-point)
            (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
            (define-key map [mouse-1] #'cider-stateful-check-operate-on-click)
            (define-key map [tab] #'cider-inspector-next-inspectable-object)
            ;; Emacs translates S-TAB to BACKTAB on X.
            (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
            map))

(defun cider-stateful-check--render-test-event-p (event)
  "Return non-nil if the CIDER test EVENT should be rendered."
  (and (nrepl-dict-p event)
       (nrepl-dict-get-in event '("stateful-check" "result-data" "specification"))))

(defun cider-stateful-check--cider-test-render-assertion (orig-fun &rest args)
  "Advice around `cider-test-render-assertion' using ORIG-FUN and ARGS."
  (let ((event (elt args 1)))
    (if (cider-stateful-check--render-test-event-p event)
        (cider-stateful-check--render-test-event event)
      (apply orig-fun args))))

;; Transient

(transient-define-infix cider-stateful-check:gen-max-length ()
  "The transient option to set `cider-stateful-check-gen-max-length`."
  :class 'transient-lisp-variable
  :description "Max command length"
  :key "-l"
  :transient t
  :variable 'cider-stateful-check-gen-max-length)

(transient-define-infix cider-stateful-check:gen-max-size ()
  "The transient option to set `cider-stateful-check-gen-max-size`."
  :class 'transient-lisp-variable
  :description "Max. size for generated values"
  :key "-s"
  :transient t
  :variable 'cider-stateful-check-gen-max-size)

(transient-define-infix cider-stateful-check:gen-threads ()
  "The transient option to set `cider-stateful-check-gen-threads`."
  :class 'transient-lisp-variable
  :description "Number of threads"
  :key "-t"
  :transient t
  :variable 'cider-stateful-check-gen-threads)

(transient-define-infix cider-stateful-check:run-assume-immutable-results-p ()
  "The transient option to set `cider-stateful-check-run-assume-immutable-results-p`."
  :class 'transient-lisp-variable
  :description "Assume immutable results"
  :key "-i"
  :transient t
  :variable 'cider-stateful-check-run-assume-immutable-results-p)

(transient-define-infix cider-stateful-check:run-max-tries ()
  "The transient option to set `cider-stateful-check-run-max-tries`."
  :class 'transient-lisp-variable
  :description "Number of fail attempts"
  :key "-T"
  :transient t
  :variable 'cider-stateful-check-run-max-tries)

(transient-define-infix cider-stateful-check:run-num-tests ()
  "The transient option to set `cider-stateful-check-run-num-tests`."
  :class 'transient-lisp-variable
  :description "Number of tests to run"
  :key "-n"
  :transient t
  :variable 'cider-stateful-check-run-num-tests)

(transient-define-infix cider-stateful-check:run-seed ()
  "The transient option to set `cider-stateful-check-run-seed`."
  :class 'transient-lisp-variable
  :description "Initial seed to use for generation"
  :key "-S"
  :transient t
  :variable 'cider-stateful-check-run-seed)

(transient-define-infix cider-stateful-check:run-timeout-ms ()
  "The transient option to set `cider-stateful-check-run-timeout-ms`."
  :class 'transient-lisp-variable
  :description "Test timeout in ms"
  :key "-x"
  :transient t
  :variable 'cider-stateful-check-run-timeout-ms)

(transient-define-infix cider-stateful-check:report-first-case-p ()
  "The transient option to set `cider-stateful-check-report-first-case-p`."
  :class 'transient-lisp-variable
  :description "Report first failing case"
  :key "-f"
  :transient t
  :variable 'cider-stateful-check-report-first-case-p)

(transient-define-infix cider-stateful-check:report-command-frequency-p ()
  "The transient option to set `cider-stateful-check-report-command-frequency`."
  :class 'transient-lisp-variable
  :description "Report command frequency"
  :key "-F"
  :transient t
  :variable 'cider-stateful-check-report-command-frequency-p)

(transient-define-prefix cider-stateful-check ()
  "A transient menu to set the Stateful Check specification run options."
  [["Stateful Check\n\nGeneration Options"
    (cider-stateful-check:gen-max-length)
    (cider-stateful-check:gen-max-size)
    (cider-stateful-check:gen-threads)]
   ["Run Options"
    (cider-stateful-check:run-assume-immutable-results-p)
    (cider-stateful-check:run-max-tries)
    (cider-stateful-check:run-num-tests)
    (cider-stateful-check:run-timeout-ms)
    (cider-stateful-check:run-seed)]
   ["Report Options"
    (cider-stateful-check:report-command-frequency-p)
    (cider-stateful-check:report-first-case-p)]]
  ["Actions"
   ("g" cider-stateful-check-rerun)
   ("r" cider-stateful-check-run)
   ("s" cider-stateful-check-scan)])

(advice-add 'cider-test-render-assertion :around #'cider-stateful-check--cider-test-render-assertion)
(define-key cider-test-report-mode-map (kbd "D") #'cider-stateful-check-show-test-report)

(provide 'cider-stateful-check)

;;; cider-stateful-check.el ends here
