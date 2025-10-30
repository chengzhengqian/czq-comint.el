;;; czq-comint-send.el --- Command sending helpers for CZQ comint -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, tools
;; URL: https://example.com/czq-comint

;;; Commentary:
;;
;; This module centralizes helper utilities for sending commands to CZQ comint
;; processes.  It introduces a lightweight render-filter stack so higher-level
;; callers can suppress or redirect output for the duration of a single command.
;; The initial implementation ports `czq-comint--send-command-quietly' to the
;; new infrastructure without changing the public API.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)

(declare-function czq-comint--debug "czq-comint" (format-string &rest args))
(declare-function czq-comint--debug-timestamp "czq-comint" ())

(defvar czq-comint-tag-name)
(defvar czq-comint-output-enabled)

(defvar-local czq-comint-send--render-filters nil
  "List of active render filters for the current CZQ comint buffer.")

(defvar czq-comint-send--filter-seq 0
  "Internal counter used to generate unique render filter identifiers.")

(defcustom czq-comint-send-quiet-teardown-delay 0.05
  "Seconds to wait before removing the quiet-send render filter."
  :type 'number
  :group 'czq-comint)

(defcustom czq-comint-send-quiet-extra-delay 0.05
  "Extra delay (seconds) added per SKIP increment for quiet sends."
  :type 'number
  :group 'czq-comint)

(defun czq-comint-send--read-delay (prompt current)
  "Read a non-negative floating delay from the minibuffer.
PROMPT is shown to the user and CURRENT provides the default."
  (let* ((input (read-from-minibuffer
                 (format "%s (seconds) [%0.3f]: " prompt current)
                 nil nil nil nil (format "%0.3f" current)))
         (trimmed (string-trim input)))
    (if (string-empty-p trimmed)
        current
      (let ((value (condition-case err
                       (read trimmed)
                     (error (user-error "Invalid number: %s" (error-message-string err))))))
        (unless (numberp value)
          (user-error "Expected a numeric value, got %S" value))
        (when (< value 0)
          (user-error "Delay must be non-negative"))
        (float value)))))

(defun czq-comint-send--quiet-delay-values ()
  "Return current quiet delay values for the active buffer."
  (list czq-comint-send-quiet-teardown-delay
        czq-comint-send-quiet-extra-delay))

;;;###autoload
(defun czq-comint-send-edit-quiet-delays (&optional buffer)
  "Inspect or adjust quiet-send delay settings for BUFFER.
When called interactively operate on the current buffer.  Prompts for
new teardown and extra-delay values; press RET to keep the current
value.  The updated values are made buffer-local."
  (interactive)
  (let* ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'czq-comint-mode)
        (user-error "Not in a czq-comint buffer"))
      (let* ((current (czq-comint-send--quiet-delay-values))
             (teardown (nth 0 current))
             (extra (nth 1 current))
             (new-teardown (czq-comint-send--read-delay
                            "Quiet teardown delay" teardown))
             (new-extra (czq-comint-send--read-delay
                         "Quiet extra increment" extra)))
        (setq-local czq-comint-send-quiet-teardown-delay new-teardown)
        (setq-local czq-comint-send-quiet-extra-delay new-extra)
        (message "Quiet delays for %s: teardown=%0.3fs extra=%0.3fs"
                 (buffer-name buffer) new-teardown new-extra)))))

(defun czq-comint-send--debug (fmt &rest args)
  "Emit a render-filter debug message controlled by `czq-comint-debug'."
  (when (and (boundp 'czq-comint-debug)
             czq-comint-debug
             (fboundp 'czq-comint--debug)
             (fboundp 'czq-comint--debug-timestamp))
    (let ((timestamp (czq-comint--debug-timestamp)))
      (apply #'czq-comint--debug (concat "%s " fmt)
             (cons timestamp args)))))

(defun czq-comint-send--next-filter-id ()
  "Return a unique identifier symbol for render filters."
  (cl-incf czq-comint-send--filter-seq)
  (intern (format "czq-comint-filter-%d" czq-comint-send--filter-seq)))

(defun czq-comint-send--register-filter (fn &optional finalize)
  "Register FN as a render filter with optional FINALIZE callback.
Return the new filter entry."
  (let ((entry (list :id (czq-comint-send--next-filter-id)
                     :fn fn
                     :timer nil
                     :finalize finalize)))
    (push entry czq-comint-send--render-filters)
    (czq-comint-send--debug "render-filter push %S depth=%d"
                            (plist-get entry :id)
                            (length czq-comint-send--render-filters))
    entry))

(defun czq-comint-send--find-filter (id)
  "Return the filter entry matching ID, or nil if not found."
  (cl-find id czq-comint-send--render-filters
           :key (lambda (entry) (plist-get entry :id))
           :test #'eq))

(defun czq-comint-send--remove-filter (entry)
  "Remove ENTRY from the render filter stack and run its finalizer."
  (let ((timer (plist-get entry :timer)))
    (when (timerp timer)
      (cancel-timer timer)
      (plist-put entry :timer nil)))
  (setq czq-comint-send--render-filters (delq entry czq-comint-send--render-filters))
  (czq-comint-send--debug "render-filter pop %S depth=%d"
                          (plist-get entry :id)
                          (length czq-comint-send--render-filters))
  (when-let ((finalize (plist-get entry :finalize)))
    (funcall finalize)))

(defun czq-comint-send--apply-render-filters (chunk)
  "Apply registered render filters to CHUNK and return the transformed string."
  (let ((result chunk))
    (dolist (entry (reverse czq-comint-send--render-filters) result)
      (let* ((id (plist-get entry :id))
             (before (or result ""))
             (after (funcall (plist-get entry :fn) before)))
        (setq result after)
        (czq-comint-send--debug "render-filter apply %S len=%d→%d"
                                id
                                (length before)
                                (length (or after "")))))
    result))

(defun czq-comint-send--quiet-filter (_chunk)
  "Render filter used by `czq-comint--send-command-quietly'."
  "")

(defun czq-comint-send--quiet-restore-tag (id extra)
  "Return a restore tag that schedules quiet filter ID for removal.
EXTRA extends the quiet window in `czq-comint-send--complete-quiet'."
  (let* ((tag (or czq-comint-tag-name "czq-comint"))
         (body (format "(czq-comint-send--complete-quiet '%S %d)" id (max 0 (or extra 0))))
         (payload (format "<%s handler=elisp>%s</%s>" tag body tag)))
    (format "printf '%%s\\n' %s\n" (shell-quote-argument payload))))

(defun czq-comint-send--quiet-delay (extra)
  "Return the delay in seconds before removing the quiet filter.
EXTRA is the numeric argument passed to `czq-comint--send-command-quietly'."
  (max 0 (+ czq-comint-send-quiet-teardown-delay
            (* (max 0 (or extra 0)) czq-comint-send-quiet-extra-delay))))

(defun czq-comint-send--teardown-filter (buffer id)
  "Timer callback removing filter ID from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((entry (czq-comint-send--find-filter id)))
        (plist-put entry :timer nil)
        (czq-comint-send--remove-filter entry)))))

(defun czq-comint-send--schedule-removal (entry extra)
  "Schedule quiet filter ENTRY for teardown with EXTRA delay increments."
  (let* ((delay (czq-comint-send--quiet-delay extra))
         (buffer (current-buffer))
         (existing (plist-get entry :timer))
         (id (plist-get entry :id)))
    (when (timerp existing)
      (cancel-timer existing)
      (plist-put entry :timer nil))
    (if (<= delay 0)
        (czq-comint-send--remove-filter entry)
      (let ((timer (run-at-time delay nil
                                #'czq-comint-send--teardown-filter
                                buffer id)))
        (plist-put entry :timer timer)
        (czq-comint-send--debug "render-filter timer %S delay=%0.3fs"
                                id delay)))))

(defun czq-comint-send--complete-quiet (id extra)
  "Finalize quiet filter ID and schedule removal after a quiet delay.
EXTRA extends the delay window by `czq-comint-send-quiet-extra-delay'
seconds per increment."
  (when-let ((entry (czq-comint-send--find-filter id)))
    (czq-comint-send--schedule-removal entry extra)))

(defun czq-comint-send--normalize-command (command)
  "Ensure COMMAND ends with a newline."
  (if (and command (string-suffix-p "\n" command))
      command
    (concat command "\n")))

;;;###autoload
(defun czq-comint--send-command-quietly (process command &optional skip-strings)
  "Send COMMAND to PROCESS while temporarily suppressing buffer output.

COMMAND is ensured to end with a newline.  SKIP-STRINGS, when non-nil,
extends the quiet window by `czq-comint-send-quiet-extra-delay' seconds per
increment before normal rendering resumes."
  (unless (and process (process-live-p process))
    (user-error "Process is not live"))
  (unless (and command (> (length command) 0))
    (user-error "COMMAND must be a non-empty string"))
  (let* ((buffer (process-buffer process)))
    (unless (buffer-live-p buffer)
      (user-error "Process buffer is unavailable"))
    (with-current-buffer buffer
      (let* ((entry (czq-comint-send--register-filter #'czq-comint-send--quiet-filter))
             (extra (max 0 (or skip-strings 0)))
             (payload (czq-comint-send--normalize-command command))
             (restore (czq-comint-send--quiet-restore-tag (plist-get entry :id) extra)))
        (comint-send-string process (concat payload restore))))))

(provide 'czq-comint-send)

;;; czq-comint-send.el ends here
