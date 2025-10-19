;;; czq-comint-completion.el --- Custom completion helpers for CZQ comint -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, tools
;; URL: https://github.com/chengzhengqian/czq-comint.el

;;; Commentary:
;;
;; This module provides a minimal completion backend for `czq-comint-mode'.  The
;; initial implementation exposes a simple completion function that filters a
;; customizable command list.  The goal is to keep the surface small while we
;; experiment with tailored completion behaviour for CZQ workflows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)

(defgroup czq-comint-completion nil
  "Completion helpers for `czq-comint-mode'."
  :prefix "czq-comint-completion-"
  :group 'processes)

(defcustom czq-comint-completion-command-list
  '("ls" "cd" "cat" "pwd" "echo" "grep" "find" "git" "make" "python" "bash"
    "tail" "head" "chmod" "chown" "mkdir" "rm" "cp" "mv")
  "Default list of commands offered by the CZQ comint completion helper."
  :type '(repeat string)
  :group 'czq-comint-completion)

(defvar-local czq-comint-completion--cached-commands nil
  "Buffer-local cache of command candidates for completion.")

(defun czq-comint-completion--collect-path-commands (&optional path)
  "Return a list of executable names found in PATH.
PATH defaults to the current `PATH' environment variable."
  (let* ((path (or path (getenv "PATH")))
         (separated (and path (split-string path path-separator t)))
         (commands '()))
    (dolist (dir separated)
      (when (file-directory-p dir)
        (dolist (entry (directory-files dir t nil t))
          (when (and (file-regular-p entry)
                     (file-executable-p entry))
            (push (file-name-nondirectory entry) commands)))))
    (delete-dups commands)))

(defun czq-comint-completion-refresh (&optional path)
  "Refresh the buffer-local command cache using PATH if supplied.
Returns the updated command list."
  (let* ((path-commands (czq-comint-completion--collect-path-commands path))
         (combined (delete-dups
                    (append czq-comint-completion-command-list
                            path-commands))))
    (setq czq-comint-completion--cached-commands combined)))

(defun czq-comint-completion-commands ()
  "Return the cached command list, refreshing it if necessary."
  (or czq-comint-completion--cached-commands
      (czq-comint-completion-refresh)))

(defun czq-comint-completion-refresh-from-process (&optional buffer)
  "Refresh command cache using the PATH reported by the REPL process for BUFFER.
When called interactively, default to the current buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (process (get-buffer-process buffer)))
    (unless (and process (process-live-p process))
      (user-error "No live comint process in %s" (buffer-name buffer)))
    (let* ((redirect-buffer (generate-new-buffer " *czq-comint-path*"))
           (marker "__CZQ_PATH__=")
           (command "printf '__CZQ_PATH__=%s\\n' \"$PATH\"\n"))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (comint-redirect-send-command-to-process command redirect-buffer process nil t))
            (with-current-buffer redirect-buffer
              (while (and (not comint-redirect-completed)
                          (accept-process-output process 0.05)))
              (goto-char (point-min))
              (when (re-search-forward (concat marker "\\(.*\\)$") nil t)
                (let ((path (match-string 1)))
                  (with-current-buffer buffer
                    (czq-comint-completion-refresh path))))))
        (kill-buffer redirect-buffer)))))

(defun czq-comint-completion--matches (prefix)
  "Return command completions for PREFIX using `czq-comint-completion-command-list'."
  (let ((case-fold-search t))
    (cl-remove-if-not
     (lambda (cmd)
       (string-prefix-p prefix cmd t))
     (czq-comint-completion-commands))))

(defun czq-comint-completion-at-point ()
  "Return completion data for the symbol at point using the command list."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (start (car bounds))
              (end (cdr bounds))
              (prefix (buffer-substring-no-properties start end)))
    (list start end
          (completion-table-dynamic
           (lambda (_ignored)
             (czq-comint-completion--matches prefix)))
          :exclusive 'no)))

(provide 'czq-comint-completion)

;;; czq-comint-completion.el ends here
