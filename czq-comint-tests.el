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

(provide 'czq-comint-tests)

;;; czq-comint-tests.el ends here
