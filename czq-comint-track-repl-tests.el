;;; czq-comint-track-repl-tests.el --- Tests for CZQ REPL tracking -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Coverage for `czq-comint-track-repl.el', ensuring commands update the
;; tracking state and that users can override the detected REPL manually.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'czq-comint)
(require 'czq-comint-send)
(require 'czq-comint-track-repl)

(ert-deftest czq-comint-track-repl-defaults-to-bash ()
  "CZQ comint buffers should start with a bash REPL assumption."
  (with-temp-buffer
    (czq-comint-mode)
    (should (eq czq-comint-track-repl-name 'bash))))

(ert-deftest czq-comint-track-repl-detects-known-command ()
  "Tracker should set the REPL symbol when commands match known patterns."
  (with-temp-buffer
    (czq-comint-mode)
    (czq-comint-track-repl--input-filter "python\n")
    (should (eq czq-comint-track-repl-name 'python))
    (should (equal czq-comint-track-repl-last-command "python"))))

(ert-deftest czq-comint-track-repl-detects-mathematica ()
  "Defaults should recognize Mathematica / WolframScript entry points."
  (with-temp-buffer
    (czq-comint-mode)
    (czq-comint-track-repl--input-filter "wolframscript\n")
    (should (eq czq-comint-track-repl-name 'mathematica))
    (czq-comint-track-repl--input-filter "math -noprompt\n")
    (should (eq czq-comint-track-repl-name 'mathematica))))

(ert-deftest czq-comint-track-repl-mathematica-normalizes-crlf ()
  "Mathematica normalize filter should simulate carriage returns."
  (with-temp-buffer
    (czq-comint-mode)
    (czq-comint-track-repl-edit "mathematica")
    (let ((input "\ra\r\r+\r\rb\r\r\n"))
      (should (equal "a+b\n"
                     (czq-comint--preoutput-filter input))))
    (czq-comint-track-repl-edit "python")
    (should (equal "foo\r\n"
                   (czq-comint--preoutput-filter "foo\r\n")))))

(ert-deftest czq-comint-track-repl-resets-on-empty ()
  "Manual override accepts empty input to clear the REPL symbol."
  (with-temp-buffer
    (czq-comint-mode)
    (setq-local czq-comint-track-repl-name 'python)
    (czq-comint-track-repl-edit "")
    (should (null czq-comint-track-repl-name))))

(ert-deftest czq-comint-track-repl-manual-override ()
  "Manual editing should accept arbitrary REPL names."
  (with-temp-buffer
    (czq-comint-mode)
    (setq-local czq-comint-track-repl-name 'python)
    (czq-comint-track-repl-edit "Julia")
    (should (eq czq-comint-track-repl-name 'julia))))

(ert-deftest czq-comint-track-repl-updates-from-quiet-send ()
  "Quiet-send helper should register commands with the tracker."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((process (start-process "czq-track-test" (current-buffer) "cat")))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag process nil)
            (czq-comint--send-command-quietly process "python")
            (should (eq czq-comint-track-repl-name 'python)))
        (when (process-live-p process)
          (delete-process process)))))) 

(provide 'czq-comint-track-repl-tests)

;;; czq-comint-track-repl-tests.el ends here
