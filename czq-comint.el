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

(require 'subr-x)
(require 'comint)
(require 'czq-xml-parser)
(require 'czq-comint-dirtrack)
(require 'czq-comint-completion)

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

(defun czq-comint--accumulate-output (tokens)
  "Convert TOKENS from the parser into a single output string."
  (let ((pieces '()))
    (dolist (token tokens)
      (push (if (stringp token)
                token
              (czq-comint--dispatch-tag token))
            pieces))
    (apply #'concat (nreverse pieces))))

(defun czq-comint--preoutput-filter (output)
  "Comint pre-output filter that processes CZQ structured tags in OUTPUT.

The filter feeds OUTPUT through both the XML parser and the directory tracker,
updating buffer-local state for downstream consumers such as completion."
  (when output
    (czq-comint-dirtrack-update output))
  (pcase-let* ((`(,state . ,tokens)
                (czq-comint-parse-chunk czq-comint--parser-state output)))
    (setq czq-comint--parser-state state)
    (czq-comint--accumulate-output tokens)))

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
      (let ((commands (if (file-remote-p origin-dir)
                          "pwd\n"
                        (format "cd %s\npwd\n"
                                (shell-quote-argument origin-dir)))))
        (comint-send-string process commands)))
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
  (user-error "czq-comint-execute-tag is not implemented yet"))

(provide 'czq-comint)

;;; czq-comint.el ends here
