;;; czq-comint-track-repl.el --- Track REPL context for CZQ comint -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Lightweight facilities to keep track of the active REPL inside a CZQ
;; comint buffer.  The tracker inspects commands as they are sent to the
;; underlying process and records a buffer-local symbol describing the
;; detected language/tool.  Users can override the detection manually via
;; `czq-comint-track-repl-edit'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup czq-comint-track-repl nil
  "Customization options for CZQ comint REPL detection."
  :group 'czq-comint)

(defcustom czq-comint-track-repl-command-alist
  '(("\\`python\\(\\s-\\|\\'\\)" . python)
    ("\\`julia\\(\\s-\\|\\'\\)" . julia)
    ("\\`wolframscript\\(\\s-\\|\\'\\)" . mathematica)
    ("\\`math\\(\\s-\\|\\'\\)" . mathematica)
    ("\\`ipython\\(\\s-\\|\\'\\)" . ipython)
    ("\\`node\\(\\s-\\|\\'\\)" . node)
    ("\\`deno\\(\\s-\\|\\'\\)" . deno)
    ("\\`ruby\\(\\s-\\|\\'\\)" . ruby)
    ("\\`lein repl" . clojure)
    ("\\`sbcl\\(\\s-\\|\\'\\)" . common-lisp)
    ("\\`ghci\\(\\s-\\|\\'\\)" . haskell)
    ("\\`php -a" . php)
    ("\\`lua\\(\\s-\\|\\'\\)" . lua)
    ("\\`R \\(-?-q\\b\\)?\\s-+--vanilla\\'" . r)
    ("\\`R\\(\\s-\\|\\'\\)" . r)
    ("\\`bash\\(\\s-\\|\\'\\)" . bash)
    ("\\`zsh\\(\\s-\\|\\'\\)" . zsh)
    ("\\`fish\\(\\s-\\|\\'\\)" . fish))
  "Alist mapping regex patterns to REPL modes/states.

Each entry has the form (REGEXP . SYMBOL).  When a command sent to the process
matches REGEXP (case-insensitive), the tracker sets
`czq-comint-track-repl-name' to SYMBOL.  The default entries recognize Python,
Julia, and Mathematica/WolframScript sessions, while leaving room for common
shells and other interpreters."
  :type '(repeat (cons (regexp :tag "Command regexp")
                       (symbol :tag "REPL name")))
  :group 'czq-comint-track-repl)

(defcustom czq-comint-track-repl-announce t
  "When non-nil, announce REPL detection updates in the echo area."
  :type 'boolean
  :group 'czq-comint-track-repl)

(defvar-local czq-comint-track-repl-name nil
  "Buffer-local symbol describing the detected REPL, or nil when unknown.")

(defvar-local czq-comint-track-repl-last-command nil
  "Buffer-local copy of the last command string inspected by the tracker.")

(defun czq-comint-track-repl--normalize-command (command)
  "Return COMMAND trimmed for matching, or nil when blank."
  (let ((trimmed (and command (string-trim command))))
    (unless (string-empty-p (or trimmed ""))
      trimmed)))

(defun czq-comint-track-repl--classify-command (command)
  "Return the REPL symbol associated with COMMAND, or nil if unknown."
  (let ((candidate (czq-comint-track-repl--normalize-command command))
        (case-fold-search t))
    (when candidate
      (cl-loop for (pattern . name) in czq-comint-track-repl-command-alist
               when (string-match-p pattern candidate)
               return name))))

(defun czq-comint-track-repl-register-command (command)
  "Record COMMAND and update the detected REPL symbol if applicable."
  (let* ((normalized (czq-comint-track-repl--normalize-command command))
         (detected (czq-comint-track-repl--classify-command normalized))
         (previous czq-comint-track-repl-name))
    (setq-local czq-comint-track-repl-last-command normalized)
    (when detected
      (setq-local czq-comint-track-repl-name detected)
      (when (and czq-comint-track-repl-announce
                 (not (eq previous detected)))
        (message "[czq-comint] REPL detected: %s" detected)))
    detected))

(defun czq-comint-track-repl--input-filter (command)
  "Hook for `comint-input-filter-functions' to inspect COMMAND strings."
  (czq-comint-track-repl-register-command command)
  command)

(defun czq-comint-track-repl--name->string (name)
  "Return string description for REPL NAME."
  (cond
   ((null name) "unknown")
   ((symbolp name) (symbol-name name))
   (t (format "%s" name))))

(defun czq-comint-track-repl-edit (&optional value)
  "Interactively view or override the detected REPL.

When VALUE is non-nil, use it instead of prompting (intended for tests)."
  (interactive)
  (let* ((current (czq-comint-track-repl--name->string czq-comint-track-repl-name))
         (prompt (format "REPL name (empty to clear) [%s]: " current))
         (raw (or value
                  (read-from-minibuffer prompt nil nil nil nil current))))
    (setq raw (string-trim (or raw "")))
    (if (string-empty-p raw)
        (setq-local czq-comint-track-repl-name nil)
      (setq-local czq-comint-track-repl-name
                  (intern (downcase raw))))
    (message "[czq-comint] REPL %s"
             (czq-comint-track-repl--name->string czq-comint-track-repl-name))
    czq-comint-track-repl-name))

(provide 'czq-comint-track-repl)

;;; czq-comint-track-repl.el ends here
