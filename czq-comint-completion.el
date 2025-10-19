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

(defcustom czq-comint-completion-debug nil
  "When non-nil, emit debug messages about completion context."
  :type 'boolean
  :group 'czq-comint-completion)

(defvar-local czq-comint-completion--cached-commands nil
  "Buffer-local cache of command candidates for completion.")

(defun czq-comint-completion-toggle-debug ()
  "Toggle `czq-comint-completion-debug' and report the new state."
  (interactive)
  (setq czq-comint-completion-debug (not czq-comint-completion-debug))
  (when (called-interactively-p 'any)
    (message "[czq-completion] debug %s" (if czq-comint-completion-debug "on" "off"))))

(defun czq-comint-completion--file-candidates (prefix)
  "Return file candidates based on PREFIX and the tracked directory."
  (let* ((root (or czq-comint-current-directory default-directory))
         (dir-part (or (file-name-directory prefix) ""))
         (partial (file-name-nondirectory prefix))
         (absolute (or (and (or (string-prefix-p "/" dir-part)
                                 (string-prefix-p "~" dir-part))
                              (expand-file-name dir-part))
                       (expand-file-name dir-part (or root default-directory))))
         (display-dir dir-part)
         (results '()))
    (when (file-directory-p absolute)
      (dolist (name (directory-files absolute nil nil t))
        (unless (member name '("." ".."))
          (when (string-prefix-p partial name t)
            (let* ((full (expand-file-name name absolute))
                   (suffix (if (file-directory-p full) "/" ""))
                   (candidate (concat display-dir name suffix)))
              (push candidate results))))))
    (nreverse results)))

(defun czq-comint-completion--collect-path-commands (&optional path)
  "Return a cons of (COMMANDS . DIRECTORIES) found in PATH.
PATH defaults to the current `PATH' environment variable."
  (let* ((path (or path (getenv "PATH")))
         (separated (and path (split-string path path-separator t)))
         (commands '())
         (scanned 0))
    (dolist (dir separated)
      (when (file-directory-p dir)
        (setq scanned (1+ scanned))
        (dolist (entry (directory-files dir t nil t))
          (when (and (file-regular-p entry)
                     (file-executable-p entry))
            (push (file-name-nondirectory entry) commands)))))
    (cons (delete-dups commands) scanned)))

(defun czq-comint-completion-refresh (&optional path)
  "Refresh the buffer-local command cache using PATH if supplied.
Returns the updated command list."
  (let* ((result (czq-comint-completion--collect-path-commands path))
         (path-commands (car result))
         (directories (cdr result))
         (combined (delete-dups
                    (append czq-comint-completion-command-list
                            path-commands))))
    (setq czq-comint-completion--cached-commands combined)
    (when (called-interactively-p 'any)
      (message "[czq-completion] scanned %d directories; cached %d commands"
               directories (length combined)))
    (list combined directories)))

(defun czq-comint-completion-commands ()
  "Return the cached command list, refreshing it if necessary."
  (or czq-comint-completion--cached-commands
      (car (czq-comint-completion-refresh))))

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
              (setq comint-redirect-completed nil)
              (comint-redirect-send-command-to-process command redirect-buffer process nil t))
            (with-current-buffer redirect-buffer
              (while (and (not comint-redirect-completed)
                          (accept-process-output process 0.05)))
              (goto-char (point-min))
              (when (re-search-forward (concat marker "\\(.*\\)$") nil t)
                (let ((path (match-string 1)))
                  (with-current-buffer buffer
                    (let* ((result (czq-comint-completion-refresh path))
                           (commands (car result))
                           (directories (cadr result)))
                      (when (called-interactively-p 'any)
                        (message "[czq-completion] scanned %d directories; cached %d commands (process)"
                                 directories (length commands)))))))))
        (kill-buffer redirect-buffer)))))

(defun czq-comint-completion--first-token-p (start)
  "Return non-nil if START is positioned at the first token in the line."
  (save-excursion
    (goto-char start)
    (let* ((line (buffer-substring-no-properties (line-beginning-position) start))
           (trimmed (string-trim-right line))
           (tokens (split-string trimmed "[ \t]+" t)))
      (and (not (< (length trimmed) (length line)))
           (<= (length tokens) 1)))))

(defun czq-comint-completion--prefix-context (start prefix)
  "Determine completion context at START for PREFIX.
Return the symbol `command or `file."
  (cond
   ((czq-comint-completion--first-token-p start)
    (if (or (string-prefix-p "./" prefix)
            (string-prefix-p "../" prefix)
            (string-prefix-p "~/" prefix)
            (string-match-p ":" prefix)
            (string-match-p "/" prefix))
        'file
      'command))
   (t 'file)))

(defun czq-comint-completion--matches (start prefix)
  "Return completion candidates at START for PREFIX.
The result depends on whether we are completing a command or file."
  (let* ((context (czq-comint-completion--prefix-context start prefix))
         (candidates (pcase context
                       ('command (czq-comint-completion-commands))
                       ('file (czq-comint-completion--file-candidates prefix))
                       (_ nil)))
         (matches (and candidates
                        (cl-remove-if-not
                         (lambda (item) (string-prefix-p prefix item t))
                         candidates))))
    (cons matches context)))

(defun czq-comint-completion-at-point ()
  "Return completion data for the token at point using CZQ candidates."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n")
                  (point)))
         (prefix (buffer-substring-no-properties start end)))
    (let* ((result (czq-comint-completion--matches start prefix))
           (matches (car result))
           (context (cdr result)))
      (when (and czq-comint-completion-debug matches)
        (message "[czq-completion] using %s candidates (%d)"
                 context (length matches)))
      (list start end matches :exclusive 'no))))

(provide 'czq-comint-completion)

;;; czq-comint-completion.el ends here
