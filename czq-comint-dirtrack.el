;;; czq-comint-dirtrack.el --- Prompt-based directory tracking -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, tools
;; URL: https://github.com/chengzhengqian/czq-comint.el

;;; Commentary:
;;
;; Lightweight helper that extracts the current working directory from shell
;; prompts.  Unlike the original tracker, the simplified version is stateless:
;; each call inspects the provided output chunk, derives the latest directory
;; mentioned in that chunk, and updates `czq-comint-current-directory' when a
;; match is found.

;;; Code:

(require 'subr-x)

(defgroup czq-comint-dirtrack nil
  "Directory tracking primitives for CZQ comint shells."
  :prefix "czq-comint-dirtrack-"
  :group 'processes)

(defcustom czq-comint-dirtrack-prompt-regexp
  "\\(?:^\\|\n\\)\\(?:\\[[^]\n]+\\] \\)?\\(?:[^@ \n]+@[^: \n]+:\\)?\\([^ \n]+\\)\\(?:[#$>] \\)"
  "Regular expression that captures the working directory in shell prompts.

Group 1 must contain the directory text.  The default value covers prompts of
the form “user@host:/path$ ”, optionally preceded by bracketed segments such as
“[venv] ”.  Update the regexp to accommodate custom prompt formats."
  :type 'regexp
  :group 'czq-comint-dirtrack)

(defcustom czq-comint-dirtrack-verify-directories t
  "When non-nil, ignore prompt matches that do not refer to existing directories."
  :type 'boolean
  :group 'czq-comint-dirtrack)

(defcustom czq-comint-dirtrack-announce-directory nil
  "When non-nil, emit a `message' each time the tracker detects a directory."
  :type 'boolean
  :group 'czq-comint-dirtrack)

(defcustom czq-comint-dirtrack-debug nil
  "When non-nil, log sanitized chunks and capture results after each call."
  :type 'boolean
  :group 'czq-comint-dirtrack)

(defvar czq-comint-current-directory nil
  "Cached working directory for the current CZQ comint buffer.")

(defun czq-comint-dirtrack--sanitize-chunk (chunk)
  "Return CHUNK with carriage returns and ANSI escapes removed."
  (let ((text (or chunk "")))
    (setq text (replace-regexp-in-string "\r" "" text))
    ;; Remove OSC (Operating System Command) sequences such as terminal titles.
    (setq text (replace-regexp-in-string "\e][^\a]*\a" "" text))
    ;; Remove CSI (Control Sequence Introducer) codes, e.g., colour settings.
    (setq text (replace-regexp-in-string "\e\\[[0-9;?]*[A-Za-z]" "" text))
    text))

(defun czq-comint-dirtrack--normalise-directory (path current)
  "Expand PATH into an absolute directory string.

CURRENT indicates the last known directory and is used to resolve relative
paths.  Returns nil when PATH is empty."
  (let* ((trimmed (string-trim (or path ""))))
    (cond
     ((string-empty-p trimmed) nil)
     ((file-remote-p trimmed) (file-name-as-directory trimmed))
     ((string-prefix-p "~" trimmed)
      (file-name-as-directory (expand-file-name trimmed)))
     ((file-name-absolute-p trimmed)
      (file-name-as-directory (expand-file-name trimmed)))
     (t
      (let ((base (or current
                      (and (boundp 'default-directory) default-directory)
                      default-directory)))
        (file-name-as-directory (expand-file-name trimmed base)))))))

(defun czq-comint-dirtrack-update (chunk)
  "Inspect CHUNK for prompt matches and update `czq-comint-current-directory'.

Returns the detected directory when successful, otherwise nil.  When debugging
is enabled via `czq-comint-dirtrack-debug', the function logs both the sanitized
chunk and each candidate directory."
  (let* ((sanitized (czq-comint-dirtrack--sanitize-chunk chunk))
         (current (or czq-comint-current-directory
                      (and (boundp 'default-directory) default-directory)))
         (directory nil)
         (start 0))
    (while (and sanitized (string-match czq-comint-dirtrack-prompt-regexp sanitized start))
      (let* ((raw (match-string 1 sanitized))
             (candidate (czq-comint-dirtrack--normalise-directory raw current)))
        (when czq-comint-dirtrack-debug
          (message "[czq-dirtrack debug %s] raw=%S normalized=%S"
                   (format-time-string "%H:%M:%S")
                   raw
                   candidate))
        (when (and candidate
                   (or (not czq-comint-dirtrack-verify-directories)
                       (file-directory-p candidate)))
          (setq directory candidate
                current candidate)))
      (setq start (match-end 0)))
    (when czq-comint-dirtrack-debug
      (message "[czq-dirtrack debug %s] chunk=%S directory=%s"
               (format-time-string "%H:%M:%S")
               sanitized
               (or directory "<no-update>")))
    (when directory
      (setq czq-comint-current-directory directory)
      (when czq-comint-dirtrack-announce-directory
        (message "[czq-dirtrack %s] %s"
                 (format-time-string "%H:%M:%S")
                 directory)))
    directory))

(defun czq-comint-dirtrack-display-current-directory ()
  "Display the directory tracked in the current buffer."
  (interactive)
  (if czq-comint-current-directory
      (message "[czq-dirtrack] current directory: %s" czq-comint-current-directory)
    (message "[czq-dirtrack] directory unknown")))

(defun czq-comint-dirtrack-toggle-debug ()
  "Toggle both `czq-comint-dirtrack-debug' and announcement logging.

When invoked interactively the function flips the debug/announcement flags and
reports their new states."
  (interactive)
  (setq czq-comint-dirtrack-debug (not czq-comint-dirtrack-debug))
  (setq czq-comint-dirtrack-announce-directory czq-comint-dirtrack-debug)
  (when (called-interactively-p 'any)
    (message "[czq-dirtrack] debug=%s announce=%s"
             (if czq-comint-dirtrack-debug "on" "off")
             (if czq-comint-dirtrack-announce-directory "on" "off"))))

(provide 'czq-comint-dirtrack)

;;; czq-comint-dirtrack.el ends here
