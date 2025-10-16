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
  ;; Mode initialization logic will be added here.
  )

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
