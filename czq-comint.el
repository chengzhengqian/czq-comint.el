;;; czq-comint.el --- Structured comint extensions for CZQ tags -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, tools
;; URL: https://example.com/czq-comint

;;; Commentary:
;;
;; This package provides a customized comint experience focused on
;; CZQ-specific tags such as <czq-comint ...>...</czq-comint>.  The goal
;; is to offer ergonomic helpers for discovery, execution, and navigation
;; without sacrificing the familiar comint workflow.
;;
;; This file currently contains scaffolding only; implementation details
;; will be added incrementally.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'czq-xml-parser)
(require 'czq-comint-dirtrack)
(require 'czq-comint-completion)
(require 'czq-comint-track-repl)
(require 'czq-comint-normalize)

(declare-function czq-comint-send--apply-render-filters "czq-comint-send" (chunk))

(autoload 'czq-comint--send-command-quietly "czq-comint-send"
  "Send COMMAND to PROCESS while suppressing CZQ comint buffer output." nil)

(defgroup czq-comint nil
  "Customized comint features for <czq-comint> tags."
  :prefix "czq-comint-"
  :group 'processes)

(defcustom czq-comint-tag-name "czq-comint"
  "Default tag used to identify CZQ comint blocks."
  :type 'string
  :group 'czq-comint)

(defcustom czq-comint-debug nil
  "When non-nil, emit diagnostic messages while dispatching tags."
  :type 'boolean
  :group 'czq-comint)

;;;###autoload
(defun czq-comint-toggle-debug (&optional arg)
  "Toggle `czq-comint-debug'.

With optional ARG, enable debug logging when ARG is positive,
disable it when ARG is zero or negative.  When ARG is `toggle' or
nil, flip the current state.  Return the new value."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let* ((previous czq-comint-debug)
         (new (cond
               ((memq arg '(toggle nil)) (not previous))
               ((> (prefix-numeric-value arg) 0) t)
               (t nil))))
    (setq czq-comint-debug new)
    (message "czq-comint-debug %s" (if new "enabled" "disabled"))
    new))

(defconst czq-comint--default-handlers
  '((elisp . czq-comint--handle-elisp)
    (omit . czq-comint--handle-omit))
  "Default handler table used by `czq-comint-handlers'.")

(defcustom czq-comint-command-alist nil
  "Alist mapping symbolic names to shell commands.

Each entry has the form (NAME . COMMAND).  When `czq-comint-run' is
invoked with a buffer name containing NAME (case-insensitive match),
COMMAND is executed after the shell starts.  If no entry matches the
buffer name the shell starts without running an additional command."
  :type '(alist :key-type string :value-type string)
  :group 'czq-comint)

(defcustom czq-comint-handlers czq-comint--default-handlers
  "Alist mapping handler names to functions.

Each entry should have the form (SYMBOL . FUNCTION).  When the parser
encounters a CZQ tag it looks up the handler specified by the \"handler\"
attribute and invokes the associated function with two arguments: the tag
body as a string and the remaining attribute alist."
  :type '(alist :key-type symbol :value-type function)
  :group 'czq-comint)

(defvar-local czq-comint--parser-state nil
  "Buffer-local parser state used by the comint output filter.")

(defvar-local czq-comint-current-directory nil
  "Buffer-local cache of the working directory tracked for the CZQ comint buffer.")

(defvar-local czq-comint-output-enabled t
  "When non-nil, emit text produced by the CZQ comint pre-output filter.")

(defvar-local czq-comint--normalize-filter nil
  "Optional normalization filter applied before render filters.")

(defvar-local czq-comint--normalize-finalize nil
  "Cleanup function run when the normalization filter changes.")

(defun czq-comint--handle-elisp (body attrs)
  "Evaluate BODY as Emacs Lisp and optionally return printed results.

ATTRS is the attribute alist excluding the handler entry.  Multiple
expressions in BODY are evaluated sequentially.  By default the handler
suppresses printed output unless the attribute \"results\" is set to a
truthy value (for example results=\"true\").  Errors are reported as
strings regardless of the attribute."
  (let ((emit-results (czq-comint--attr-truthy-p attrs "results"))
        (origin (current-buffer)))
    (condition-case err
        (let ((output
               (with-temp-buffer
                 (insert body)
                 (goto-char (point-min))
                 (let ((results '()))
                   (condition-case nil
                       (while t
                         (let ((form (read (current-buffer))))
                           (with-current-buffer origin
                             (push (eval form) results))))
                     (end-of-file nil))
                   (if results
                       (concat (mapconcat #'prin1-to-string (nreverse results) "\n")
                               "\n")
                     "")))))
          (if emit-results output ""))
      (error (format "[czq-comint elisp error] %s" (error-message-string err))))))

(defun czq-comint--handle-omit (_body _attrs)
  "Ignore the BODY for tags whose handler is `omit'."
  "")

(defun czq-comint--debug (format-string &rest args)
  "Emit a debug message when `czq-comint-debug' is non-nil.
FORMAT-STRING and ARGS follow `message'."
  (when czq-comint-debug
    (apply #'message (concat "[czq-comint] " format-string) args)))

(defun czq-comint--debug-timestamp ()
  "Return a short time stamp for debug messages."
  (format-time-string "%H:%M:%S"))

(defun czq-comint--debug-escape-control (string)
  "Return STRING with control characters rendered visibly."
  (when string
    (let ((result (buffer-string)))
      (with-temp-buffer
        (dolist (char (string-to-list string))
          (cond
           ((= char ?\r) (insert "\\r"))
           ((= char ?\n) (insert "\\n\n"))
           ((>= char 32) (insert char))
           (t (insert (format "\\x%02X" char)))))
        (buffer-string)))))

(defun czq-comint--debug-summarize (value &optional limit)
  "Return a truncated string representation of VALUE for debug output.
LIMIT defaults to 120 characters."
  (let* ((limit (or limit 120))
         (raw (if (stringp value)
                  (czq-comint--debug-escape-control value)
                (prin1-to-string value)))
         (len (length raw)))
    (if (<= len limit)
        raw
      (let* ((keep (max 4 (/ (max 1 (- limit 3)) 2)))
             (prefix (substring raw 0 keep))
             (suffix (substring raw (- len keep))))
        (format "%s…%s" prefix suffix)))))

(defun czq-comint--debug-summarize-token (token)
  "Return a compact representation for TOKEN used in debug logging."
  (cond
   ((stringp token)
    (format "\"%s\"" (czq-comint--debug-summarize token 80)))
   ((and (consp token) (consp (car token)))
    (let* ((attrs (car token))
           (body (cdr token))
           (handler (cdr (assoc "handler" attrs))))
      (format "<tag handler=%s attrs=%s body=%s>"
              (czq-comint--debug-summarize handler 40)
              (czq-comint--debug-summarize attrs 40)
              (czq-comint--debug-summarize body 80))))
   (t (czq-comint--debug-summarize token 80))))

(defun czq-comint--debug-format-tokens (tokens)
  "Return an indexed summary string for TOKENS."
  (let ((index 0)
        (pieces '()))
    (dolist (token tokens)
      (setq index (1+ index))
      (push (format "%d:%s" index (czq-comint--debug-summarize-token token))
            pieces))
    (mapconcat #'identity (nreverse pieces) " ")))

(defun czq-comint--normalize-handler-name (name)
  "Convert NAME from attribute form to the symbol used in handler tables."
  (cond
   ((null name) nil)
   ((symbolp name) name)
   ((stringp name) (intern (downcase name)))
   (t (intern (downcase (format "%s" name))))))

(defun czq-comint--lookup-handler (name)
  "Return cons of handler symbol and function for NAME, or nil."
  (let* ((symbol (czq-comint--normalize-handler-name name))
         (key-string (cond
                      ((stringp name) name)
                      ((symbolp name) (symbol-name name))
                      ((null name) nil)
                      (t (format "%s" name))))
         (entry (or (and symbol (assq symbol czq-comint-handlers))
                    (and key-string
                         (assoc-string key-string czq-comint-handlers t)))))
    (when (and entry symbol)
      (cons symbol (cdr entry)))))

(defun czq-comint--render-attributes (attrs)
  "Return a textual representation for ATTRS suitable for fallback output."
  (mapconcat
   (lambda (pair)
     (let* ((name (car pair))
            (value (cdr pair))
            (name-str (cond
                       ((stringp name) name)
                       ((symbolp name) (symbol-name name))
                       (t (format "%s" name)))))
       (if (and value (not (equal value "")))
           (format "%s=\"%s\"" name-str value)
         name-str)))
   attrs
   " "))

(defun czq-comint--command-for-buffer (buffer-name)
  "Return command string for BUFFER-NAME based on `czq-comint-command-alist'."
  (let ((case-fold-search t))
    (catch 'command
      (dolist (entry czq-comint-command-alist nil)
        (when (and (car entry)
                   (cdr entry)
                   (string-match-p (regexp-quote (car entry)) buffer-name))
          (throw 'command (cdr entry)))))))

(defun czq-comint--find-attr (attrs name)
  "Return the attribute cons for NAME within ATTRS, or nil.
NAME is matched case-insensitively."
  (let ((target (downcase (if (symbolp name) (symbol-name name) name))))
    (catch 'found
      (dolist (pair attrs nil)
        (let ((key (car pair)))
          (when (and (stringp key)
                     (string-equal (downcase key) target))
            (throw 'found pair)))))))

(defun czq-comint--attr-truthy-p (attrs name)
  "Return non-nil when attribute NAME within ATTRS is truthy.
Recognises common true-ish string values such as \"true\", \"t\", \"yes\", and \"1\"."
  (let* ((entry (czq-comint--find-attr attrs name))
         (value (and entry (cdr entry))))
    (cond
     ((null entry) nil)
     ((null value) t)
     ((stringp value)
      (member (downcase value) '("1" "on" "t" "true" "yes")))
     (t value))))

(defun czq-comint--fallback-token (attrs body)
  "Reconstruct original tag for ATTRS and BODY when dispatch fails."
  (let* ((attr-text (czq-comint--render-attributes attrs))
         (open (if (equal attr-text "")
                   (format "<%s>" czq-comint-tag-name)
                 (format "<%s %s>" czq-comint-tag-name attr-text))))
    (format "%s%s</%s>" open body czq-comint-tag-name)))

(defun czq-comint--normalize-handler-result (value)
  "Convert handler return VALUE into a string the filter can emit."
  (cond
   ((null value) "")
   ((stringp value) value)
   (t (format "%s" value))))

(defun czq-comint--dispatch-tag (token)
  "Dispatch TOKEN produced by the XML parser and return output string."
  (let* ((attrs (car token))
         (body (cdr token))
         (handler-entry nil)
         (handler-name nil)
         (remaining '()))
    (dolist (pair attrs)
      (let ((name (car pair)))
        (if (and (null handler-name)
                 (string-equal (cond
                                ((stringp name) name)
                                ((symbolp name) (symbol-name name))
                                (t (format "%s" name)))
                               "handler"))
            (setq handler-name (cdr pair))
          (push pair remaining))))
    (setq remaining (nreverse remaining))
    (setq handler-entry (czq-comint--lookup-handler handler-name))
    (if (null handler-entry)
        (progn
          (czq-comint--debug "No handler found for %S; emitting literal tag." handler-name)
          (czq-comint--fallback-token attrs body))
      (let* ((symbol (car handler-entry))
             (fn (cdr handler-entry)))
        (czq-comint--debug "Dispatching handler %S with attrs %S" symbol remaining)
        (condition-case err
            (czq-comint--normalize-handler-result
             (funcall fn body remaining))
          (error
           (czq-comint--debug "Handler %S error: %s" symbol (error-message-string err))
           (format "[czq-comint handler %S error] %s"
                   symbol (error-message-string err))))))))

(defun czq-comint--render-token (token)
  "Return the rendered string for TOKEN, respecting output toggles."
  (cond
   ((stringp token)
    (if czq-comint-output-enabled
        (progn
          (when czq-comint-debug
            (czq-comint--debug "%s emitting string token: %s"
                               (czq-comint--debug-timestamp)
                               (czq-comint--debug-summarize token)))
          token)
      (when czq-comint-debug
        (czq-comint--debug "%s skipping string while output disabled: %s"
                           (czq-comint--debug-timestamp)
                           (czq-comint--debug-summarize token)))
      ""))
   (t
    (czq-comint--dispatch-tag token))))

(defun czq-comint--accumulate-output (tokens)
  "Convert TOKENS from the parser into a single output string.
Each rendered token is passed through the CZQ render-filter stack
before being concatenated."
  (let ((pieces '()))
    (dolist (token tokens)
      (let* ((rendered (czq-comint--render-token token))
             (filtered (if (fboundp 'czq-comint-send--apply-render-filters)
                           (czq-comint-send--apply-render-filters rendered)
                         rendered)))
        (push filtered pieces)))
    (apply #'concat (nreverse pieces))))

(defun czq-comint--apply-normalize-filter (chunk)
  "Apply the buffer's normalization filter to CHUNK."
  (if (and chunk czq-comint--normalize-filter)
      (let ((before chunk)
            (after (funcall czq-comint--normalize-filter chunk)))
        (when czq-comint-debug
          (czq-comint--debug "normalize-filter %s len=%d→%d"
                             (or czq-comint-track-repl-name 'unknown)
                             (length before)
                             (length (or after ""))))
        after)
    chunk))

(defun czq-comint--preoutput-filter (output)
  "Comint pre-output filter that processes CZQ structured tags in OUTPUT.

The filter feeds OUTPUT through both the XML parser and the directory tracker,
updating buffer-local state for downstream consumers such as completion."
  (when czq-comint-debug
    (let ((timestamp (czq-comint--debug-timestamp)))
      (czq-comint--debug "%s raw (len=%d, output-enabled=%s) chunk=%s"
                         timestamp
                         (if output (length output) 0)
                         czq-comint-output-enabled
                         (czq-comint--debug-summarize (or output "") 160))))
  (when output
    (czq-comint-dirtrack-update output))
  (pcase-let* ((`(,state . ,tokens)
                (czq-comint-parse-chunk czq-comint--parser-state output)))
    (when czq-comint-debug
      (let ((timestamp (czq-comint--debug-timestamp)))
        (czq-comint--debug "%s tokens (count=%d) %s"
                           timestamp
                           (length tokens)
                           (czq-comint--debug-format-tokens tokens))))
    (setq czq-comint--parser-state state)
    (let* ((rendered (czq-comint--accumulate-output tokens))
           (normalized (czq-comint--apply-normalize-filter rendered))
           (suppressed (not czq-comint-output-enabled)))
      (when czq-comint-debug
        (let ((timestamp (czq-comint--debug-timestamp)))
          (czq-comint--debug "%s rendered (len=%d, suppressed=%s) chunk=%s"
                             timestamp
                             (length (or normalized ""))
                             suppressed
                             (czq-comint--debug-summarize normalized))))
      (if suppressed "" normalized))))

(defun czq-comint-set-normalize-filter (fn &optional finalize)
  "Install normalization filter FN with optional FINALIZE callback."
  (when (functionp czq-comint--normalize-finalize)
    (funcall czq-comint--normalize-finalize))
  (setq-local czq-comint--normalize-filter fn)
  (setq-local czq-comint--normalize-finalize finalize)
  (when czq-comint-debug
    (czq-comint--debug "normalize-filter install %s"
                       (if fn 'active 'nil))))

(defun czq-comint-clear-normalize-filter ()
  "Remove any active normalization filter."
  (when (functionp czq-comint--normalize-finalize)
    (funcall czq-comint--normalize-finalize))
  (setq-local czq-comint--normalize-filter nil)
  (setq-local czq-comint--normalize-finalize nil)
  (when czq-comint-debug
    (czq-comint--debug "normalize-filter cleared")))

(defun czq-comint--local-czq-variables ()
  "Return a sorted list of CZQ comint-specific buffer-local variables."
  (let ((symbols '()))
    (dolist (entry (buffer-local-variables))
      (let ((sym (car entry)))
        (when (and (symbolp sym)
                   (string-prefix-p "czq-comint" (symbol-name sym)))
          (push sym symbols))))
    (setq symbols (delete-dups symbols))
    (sort symbols (lambda (a b)
                    (string-lessp (symbol-name a) (symbol-name b))))))

;;;###autoload
(defun czq-comint-edit-locals (&optional buffer)
  "Display and optionally edit CZQ comint buffer-local variables for BUFFER.
When called interactively, operate on the current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'czq-comint-mode)
        (user-error "Not in a czq-comint buffer"))
      (let ((locals (czq-comint--local-czq-variables)))
        (with-help-window "*CZQ Comint Locals*"
          (princ (format "CZQ comint locals for %s\n\n" (buffer-name)))
          (if locals
              (dolist (sym locals)
                (princ (format "%-32s %S\n" sym (buffer-local-value sym buffer))))
            (princ "No CZQ comint-specific locals found.\n")))
        (when (and locals (y-or-n-p "Edit a CZQ comint local variable? "))
          (let* ((name (completing-read
                        "Variable: "
                        (mapcar #'symbol-name locals)
                        nil t nil nil (symbol-name (car locals))))
                 (symbol (intern name))
                 (current (buffer-local-value symbol buffer))
                 (prompt (format "Set %s (current %S): " name current))
                 (new-value (read-from-minibuffer
                             prompt
                             (prin1-to-string current)
                             read-expression-map
                             t
                             'read-expression-history)))
            (set symbol new-value)
            (message "Set %s to %S" name (symbol-value symbol))))))))

(defvar czq-comint--buffer-name "*CZQ Comint*"
  "Name of the buffer used for CZQ comint interactions.")

(defun czq-comint--process-name (buffer-name)
  "Return a process name derived from BUFFER-NAME."
  (let* ((trimmed (string-trim (replace-regexp-in-string "\\`\\*\\|\\*\\'" "" buffer-name))))
    (if (string-empty-p trimmed)
        "czq-comint"
      trimmed)))

(defun czq-comint--make-parser-state ()
  "Create a fresh parser state tuned for `czq-comint-tag-name'."
  (czq-xml-parser-state-create :tag-name czq-comint-tag-name))

(defun czq-comint-parse-chunk (state chunk)
  "Parse CHUNK using CZQ streaming parser STATE.

STATE should be a value previously returned by `czq-comint-parse-chunk'
or created via `czq-comint--make-parser-state'.  Returns a cons cell
whose car is the updated state and whose cdr is a list of parsed
segments as defined by `czq-xml-parser-step'."
  (czq-xml-parser-step
   (or state (czq-comint--make-parser-state))
   chunk))

;;;###autoload
(define-derived-mode czq-comint-mode comint-mode "CZQ-Comint"
  "Major mode derived from `comint-mode' tailored to CZQ tags.

This mode is intended to understand <czq-comint ...>...</czq-comint>
forms and expose helper commands to execute and navigate them.
The core functionality is not implemented yet."
  (setq-local czq-comint--parser-state (czq-comint--make-parser-state))
  (setq-local czq-comint-current-directory
              (file-name-as-directory (expand-file-name default-directory)))
  (setq-local czq-comint-output-enabled t)
  (setq-local czq-comint-track-repl-name 'bash)
  (setq-local czq-comint-track-repl-last-command nil)
  (czq-comint-clear-normalize-filter)
  (czq-comint-completion-refresh)
  (setq-local completion-at-point-functions
              '(czq-comint-completion-at-point))
  (setq-local comint-dynamic-complete-functions nil)
  (local-set-key (kbd "TAB") #'completion-at-point)
  (local-set-key (kbd "<tab>") #'completion-at-point)
  (set (make-local-variable 'company-backends) '(company-capf))
  (set (make-local-variable 'company-transformers) nil)
  (set (make-local-variable 'company-occurrence-weight-function) nil)
  (set (make-local-variable 'company-sort-by-occurrence) nil)
  (add-hook 'comint-preoutput-filter-functions
            #'czq-comint--preoutput-filter
            nil t)
  (add-hook 'comint-input-filter-functions
            #'czq-comint-track-repl--input-filter
            nil t)
  )

;;;###autoload
(defun czq-comint-run (buffer-name)
  "Create or reuse a CZQ comint BUFFER-NAME and switch to it.

If BUFFER-NAME contains an entry from `czq-comint-command-alist', the
matching command is executed once the shell is ready.  Otherwise the
buffer starts a plain bash session."
  (interactive
   (list (read-string "CZQ comint buffer name: "
                      (or (and (derived-mode-p 'czq-comint-mode)
                               (buffer-name))
                          czq-comint--buffer-name))))
  (unless (and buffer-name (stringp buffer-name) (not (string-empty-p buffer-name)))
    (user-error "BUFFER-NAME must be a non-empty string"))
  (let* ((origin-dir (file-name-as-directory (expand-file-name default-directory)))
         (buffer (get-buffer-create buffer-name))
         (command (czq-comint--command-for-buffer buffer-name))
         (shell (or (executable-find "bash")
                    shell-file-name
                    (user-error "Unable to locate bash or default shell")))
         (process-name (czq-comint--process-name buffer-name))
         (existing-proc (get-buffer-process buffer))
         (process existing-proc)
         (newly-started nil))
    (with-current-buffer buffer
      (setq default-directory origin-dir))
    (unless (and process (process-live-p process))
      (when (and process (not (process-live-p process)))
        (delete-process process))
      (let ((default-directory origin-dir))
        (make-comint-in-buffer process-name buffer shell nil))
      (setq process (get-buffer-process buffer))
      (setq newly-started t))
    (with-current-buffer buffer
      (unless (derived-mode-p 'czq-comint-mode)
        (czq-comint-mode))
      (setq-local czq-comint--buffer-name buffer-name)
      (setq-local czq-comint-current-directory origin-dir))
    (when (processp process)
      (with-current-buffer buffer
        (let ((target origin-dir)
              (current (and (boundp 'czq-comint-current-directory)
                            czq-comint-current-directory)))
          (unless (and current (string= (file-name-as-directory current) target))
            (czq-comint--send-command-quietly
             process (format "cd %s" (shell-quote-argument target)))
            (setq-local czq-comint-current-directory target)))))
    (when (and newly-started (processp process))
      (set-process-query-on-exit-flag process nil)
      (when (and command (not (string-empty-p command)))
        (comint-send-string process (concat command "\n"))))
    (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun czq-comint-execute-tag (tag &optional buffer)
  "Execute a CZQ comint TAG in BUFFER.

TAG is expected to match the <czq-comint ...> format; BUFFER defaults
to the current buffer.  The handler is not implemented yet."
  (interactive (list (thing-at-point 'sexp) (current-buffer)))
  (ignore tag buffer)
  (user-error "czq-comint-execute-tag is not implemented yet"))

(provide 'czq-comint)

;;; czq-comint.el ends here
