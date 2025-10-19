;;; czq-comint-dirtrack-tests.el --- Tests for CZQ directory tracker -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Unit tests exercising the simplified stateless directory tracker used by
;; `czq-comint-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'czq-comint-dirtrack)

(ert-deftest czq-comint-dirtrack-basic-prompt ()
  "Absolute directories in prompts should be detected."
  (with-temp-buffer
    (let ((czq-comint-dirtrack-verify-directories nil)
          (expected (file-name-as-directory (expand-file-name temporary-file-directory))))
      (setq-local czq-comint-current-directory nil)
      (should (equal (czq-comint-dirtrack-update
                      (format "user@host:%s$ " temporary-file-directory))
                     expected))
      (should (equal czq-comint-current-directory expected)))))

(ert-deftest czq-comint-dirtrack-newline-prefixed-prompt ()
  "Prompts preceded by a newline should still be detected."
  (with-temp-buffer
    (let ((czq-comint-dirtrack-verify-directories nil))
      (setq-local czq-comint-current-directory nil)
      (should (equal (czq-comint-dirtrack-update "\nuser@host:~/proj$ ")
                     (file-name-as-directory (expand-file-name "~/proj")))))))

(ert-deftest czq-comint-dirtrack-relative-path ()
  "Relative directories resolve against the previously detected directory."
  (with-temp-buffer
    (let ((czq-comint-dirtrack-verify-directories nil))
      (setq-local czq-comint-current-directory "/tmp/")
      (should (equal (czq-comint-dirtrack-update "user@host:logs$ ")
                     "/tmp/logs/"))
      (should (equal czq-comint-current-directory "/tmp/logs/")))))

(ert-deftest czq-comint-dirtrack-sanitises-ansi-sequences ()
  "ANSI escape codes should be ignored when extracting directories."
  (with-temp-buffer
    (let ((czq-comint-dirtrack-verify-directories nil)
          (chunk (concat "\e]0;czq@legion:~/workspace\a"
                         "\e[01;32mczq@legion\e[0m:"
                         "\e[01;34m~/workspace\e[0m$ ")))
      (setq-local czq-comint-current-directory nil)
      (should (equal (czq-comint-dirtrack-update chunk)
                     (file-name-as-directory (expand-file-name "~/workspace")))))))

(ert-deftest czq-comint-dirtrack-announce-every-prompt ()
  "Announcement toggle should message each detected directory."
  (let ((czq-comint-dirtrack-announce-directory t)
        (czq-comint-dirtrack-verify-directories nil)
        (messages '())
        (path (file-name-as-directory (expand-file-name "~/workspace"))))
    (with-temp-buffer
      (setq-local czq-comint-current-directory nil)
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "12:34:56"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (czq-comint-dirtrack-update "user@host:~/workspace$ ")
        (czq-comint-dirtrack-update "user@host:~/workspace$ ")
        (should (equal messages
                       (list (format "[czq-dirtrack 12:34:56] %s" path)
                             (format "[czq-dirtrack 12:34:56] %s" path))))))))

(ert-deftest czq-comint-dirtrack-debug-logging ()
  "Debug mode should log raw captures and sanitized chunks."
  (let ((czq-comint-dirtrack-debug t)
        (czq-comint-dirtrack-announce-directory nil)
        (czq-comint-dirtrack-verify-directories nil)
        (messages '()))
    (with-temp-buffer
      (setq-local czq-comint-current-directory nil)
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "00:00:00"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (czq-comint-dirtrack-update "user@host:/tmp$ ")))
    (should (= (length messages) 2))
    (should (string-match-p "\\[czq-dirtrack debug 00:00:00\\] raw=\"/tmp\"" (nth 1 messages)))
    (should (string-match-p "chunk=\"user@host:/tmp\\$ \"" (car messages)))
    (should (string-match-p "directory=/tmp/" (car messages)))))

(ert-deftest czq-comint-dirtrack-display-command ()
  "Display helper should show the cached directory."
  (with-temp-buffer
    (let ((czq-comint-current-directory "/var/tmp/")
          (messages '()))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (czq-comint-dirtrack-display-current-directory)
        (should (equal messages '("[czq-dirtrack] current directory: /var/tmp/")))))))

(ert-deftest czq-comint-dirtrack-toggle-debug ()
  "Debug toggle should keep both flags in sync."
  (let ((czq-comint-dirtrack-debug nil)
        (czq-comint-dirtrack-announce-directory nil))
    (czq-comint-dirtrack-toggle-debug)
    (should czq-comint-dirtrack-debug)
    (should czq-comint-dirtrack-announce-directory)
    (czq-comint-dirtrack-toggle-debug)
    (should-not czq-comint-dirtrack-debug)
    (should-not czq-comint-dirtrack-announce-directory)))

(ert-deftest czq-comint-dirtrack-announcement-disabled ()
  "Announcements should be suppressed when the toggle is nil."
  (let ((czq-comint-dirtrack-announce-directory nil)
        (czq-comint-dirtrack-verify-directories nil)
        (messages '()))
    (with-temp-buffer
      (setq-local czq-comint-current-directory nil)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (push :called messages))))
        (czq-comint-dirtrack-update (format "user@host:%s$ " temporary-file-directory))))
    (should-not messages)))

(provide 'czq-comint-dirtrack-tests)

;;; czq-comint-dirtrack-tests.el ends here
