;;; czq-comint-tests.el --- Tests for czq-comint filter -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Basic unit tests covering the CZQ comint pre-output filter and handler
;; dispatch pipeline.  These tests exercise the filter as if a terminal were
;; feeding output chunks directly into the parser.

;;; Code:

(require 'ert)
(require 'czq-comint)

(ert-deftest czq-comint-filter-text-pass-through ()
  "Plain text should flow through the filter unchanged."
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal (czq-comint--preoutput-filter "hello world\n")
                   "hello world\n"))))

(ert-deftest czq-comint-filter-elisp-handler ()
  "The default `elisp' handler evaluates body forms but suppresses output."
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal (czq-comint--preoutput-filter
                    "<czq-comint handler=\"elisp\">(+ 1 2)</czq-comint>")
                   ""))))

(ert-deftest czq-comint-filter-elisp-handler-results ()
  "`results=\"true\"' attribute makes the elisp handler echo evaluation output."
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal (czq-comint--preoutput-filter
                    "<czq-comint handler=\"elisp\" results=\"true\">(+ 1 2)</czq-comint>")
                   "3\n"))))

(ert-deftest czq-comint-filter-streaming-chunks ()
  "Streaming chunks should accumulate until a full tag is parsed."
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal (czq-comint--preoutput-filter
                    "<czq-comint handler=\"el")
                   ""))
    (should (equal (czq-comint--preoutput-filter
                    "isp\" results=\"true\">(+ 2 2)</czq-comint>")
                   "4\n"))))

(ert-deftest czq-comint-filter-omit-handler ()
  "`omit' handler discards body content."
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal (czq-comint--preoutput-filter
                    "<czq-comint handler=\"omit\">payload</czq-comint>")
                   ""))))

(ert-deftest czq-comint-filter-unknown-handler-fallback ()
  "Unknown handler names emit the original tag structure."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((input "<czq-comint handler=\"missing\">payload</czq-comint>"))
      (should (equal (czq-comint--preoutput-filter input)
                     input)))))

(ert-deftest czq-comint-command-resolution ()
  "Buffer names should resolve commands using `czq-comint-command-alist'."
  (let ((czq-comint-command-alist '(("python" . "python3")
                                    ("sql" . "psql"))))
    (should (equal (czq-comint--command-for-buffer "*CZQ Python*")
                   "python3"))
      (should (equal (czq-comint--command-for-buffer "*czq sql*")
                     "psql"))
      (should-not (czq-comint--command-for-buffer "*CZQ Unknown*"))))

(ert-deftest czq-comint-filter-updates-directory ()
  "Pre-output filter should keep the tracked directory updated from prompts."
  (with-temp-buffer
    (czq-comint-mode)
    (setq czq-comint-current-directory nil)
    (let* ((dir (file-name-as-directory (expand-file-name temporary-file-directory)))
           (prompt (format "user@host:%s$ " dir)))
      (czq-comint--preoutput-filter prompt)
      (should (equal czq-comint-current-directory dir)))))

;;;###autoload
(defun czq-test-completion-capture ()
  "Helper to capture completion candidates at point."
  (let* ((data (run-hook-wrapped 'completion-at-point-functions
                                 (lambda (fn)
                                   (let ((res (funcall fn)))
                                     (and res (list res))))))
         (entry (car data)))
    (when entry
      (cl-destructuring-bind (start end table &rest plist) entry
        (list start end table plist)))))

(ert-deftest czq-comint-mode-registers-completion ()
  "Derived mode should register the custom completion backend."
  (let ((czq-comint-completion-command-list '("ls" "load" "git" "grep")))
    (with-temp-buffer
      (czq-comint-mode)
      (should (equal completion-at-point-functions '(czq-comint-completion-at-point)))
      (should (null comint-dynamic-complete-functions))
      (should (eq (key-binding (kbd "TAB")) #'completion-at-point))
      (insert "l")
      (let ((completion-data (czq-test-completion-capture)))
        (should completion-data)
        (cl-destructuring-bind (start end table plist) completion-data
          (should (= start 1))
          (should (= end 2))
          (should (member "ls" (all-completions "" table)))
          (should (member "load" (all-completions "" table)))
          (should (eq (plist-get plist :exclusive) 'no)))))))

(provide 'czq-comint-tests)
;;; czq-comint-tests.el ends here
