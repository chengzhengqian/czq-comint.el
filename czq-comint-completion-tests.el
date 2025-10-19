;;; czq-comint-completion-tests.el --- Tests for CZQ comint completion -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Unit tests covering the custom completion helpers defined in
;; `czq-comint-completion.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comint)
(require 'czq-comint-completion)

(ert-deftest czq-comint-completion-basic ()
  "Custom completion should offer matches from the command list."
  (let ((czq-comint-completion-command-list '("ls" "load" "git" "grep")))
    (with-temp-buffer
      (insert "l")
      (let ((bounds (czq-comint-completion-at-point)))
        (should bounds)
        (cl-destructuring-bind (start end table &rest _)
            bounds
          (should (= start 1))
          (should (= end 2))
          (let ((candidates (all-completions "" table)))
            (should (member "ls" candidates))
            (should (member "load" candidates))
            (should-not (member "git" candidates))))))))

(ert-deftest czq-comint-completion-refresh-includes-path ()
  "Refreshing the cache should include executables from PATH."
  (let* ((temp-dir (make-temp-file "czq-path" t))
         (script (expand-file-name "foo" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\n"))
          (set-file-modes script #o755)
          (let* ((original-path (getenv "PATH"))
                 (original-exec-path exec-path))
            (unwind-protect
                (progn
                  (setenv "PATH" temp-dir)
                  (setq exec-path (list temp-dir))
                  (with-temp-buffer
                    (czq-comint-mode)
                    (czq-comint-completion-refresh)
                    (should (member "foo" czq-comint-completion--cached-commands))
                    (should (member "ls" czq-comint-completion--cached-commands))))
              (setenv "PATH" original-path)
              (setq exec-path original-exec-path))))
      (delete-directory temp-dir t))))

(ert-deftest czq-comint-completion-refresh-from-process ()
  "Process refresh should extract PATH from redirected output."
  (let* ((temp-dir (make-temp-file "czq-path-proc" t))
         (script (expand-file-name "bar" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\n"))
          (set-file-modes script #o755)
          (let ((captured-command nil)
                (comint-redirect-completed nil))
            (with-temp-buffer
              (setq-local czq-comint-completion-command-list '())
              (setq-local czq-comint-completion--cached-commands nil)
              (cl-letf (((symbol-function 'comint-redirect-send-command-to-process)
                         (lambda (command output-buffer proc echo &optional no-display)
                           (setq captured-command command)
                           (with-current-buffer output-buffer
                             (erase-buffer)
                             (insert (format "__CZQ_PATH__=%s\n" temp-dir)))
                           (setq comint-redirect-completed t)
                           command)))
                (let ((dummy-process (start-process "czq-completion-test" (current-buffer) "cat")))
                  (unwind-protect
                      (progn
                        (set-process-query-on-exit-flag dummy-process nil)
                        (czq-comint-completion-refresh-from-process (current-buffer))
                        (should (string-match-p "printf '__CZQ_PATH__" captured-command))
                        (should (member "bar" czq-comint-completion--cached-commands)))
                    (when (process-live-p dummy-process)
                      (delete-process dummy-process))))))))
      (delete-directory temp-dir t))))

(provide 'czq-comint-completion-tests)

;;; czq-comint-completion-tests.el ends here
