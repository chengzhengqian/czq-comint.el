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

(require 'comint)
(require 'czq-xml-parser)

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

(defun czq-comint--handle-elisp (body attrs)
  "Evaluate BODY as Emacs Lisp and return a printable result.

ATTRS is the attribute alist excluding the handler entry.  Multiple
expressions in BODY are evaluated sequentially; the return value is the
stringified result of the final expression.  Errors are reported as
strings."
  (ignore attrs)
  (condition-case err
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (let ((results '()))
          (condition-case nil
              (while t
                (push (eval (read (current-buffer))) results))
            (end-of-file nil))
          (if results
              (concat (mapconcat #'prin1-to-string (nreverse results) "\n") "\n")
            "")))
    (error (format "[czq-comint elisp error] %s" (error-message-string err)))))

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
  "Comint pre-output filter that processes CZQ structured tags in OUTPUT."
  (pcase-let* ((`(,state . ,tokens)
                (czq-comint-parse-chunk czq-comint--parser-state output)))
    (setq czq-comint--parser-state state)
    (czq-comint--accumulate-output tokens)))

(defvar czq-comint--buffer-name "*CZQ Comint*"
  "Name of the buffer used for CZQ comint interactions.")

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
  (add-hook 'comint-preoutput-filter-functions
            #'czq-comint--preoutput-filter
            nil t))

;;;###autoload
(defun czq-comint-run ()
  "Create or reuse a CZQ comint buffer and switch to it."
  (interactive)
  (user-error "czq-comint-run is not implemented yet"))

;;;###autoload
(defun czq-comint-execute-tag (tag &optional buffer)
  "Execute a CZQ comint TAG in BUFFER.

TAG is expected to match the <czq-comint ...> format; BUFFER defaults
to the current buffer.  The handler is not implemented yet."
  (interactive (list (thing-at-point 'sexp) (current-buffer)))
  (user-error "czq-comint-execute-tag is not implemented yet"))

(provide 'czq-comint)

;;; czq-comint.el ends here
