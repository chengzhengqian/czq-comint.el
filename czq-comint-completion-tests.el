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
(require 'czq-comint)

(ert-deftest czq-comint-completion-basic ()
  "Custom completion should offer matches from the command list."
  (let ((czq-comint-completion-command-list '("ls" "load" "git" "grep")))
    (with-temp-buffer
      (setq-local czq-comint-completion--cached-commands
                  (copy-sequence czq-comint-completion-command-list))
      (insert "l")
      (pcase-let* ((`(,start ,end ,candidates . ,_)
                    (czq-comint-completion-at-point)))
        (should (= start 1))
        (should (= end 2))
        (should (member "ls" candidates))
        (should (member "load" candidates))
        (should-not (member "git" candidates))))))

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
  "Process refresh should emit a tag that updates the command cache."
  (let* ((temp-dir (make-temp-file "czq-path-proc" t))
         (script (expand-file-name "bar" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\n"))
          (set-file-modes script #o755)
          (with-temp-buffer
            (czq-comint-mode)
            (setq-local czq-comint-completion-command-list '())
            (setq-local czq-comint-completion--cached-commands nil)
            (let ((dummy-process (start-process "czq-completion-test" (current-buffer) "cat")))
              (unwind-protect
                  (progn
                    (set-process-query-on-exit-flag dummy-process nil)
                    (let ((sent-command nil)
                          (request-messages '()))
                      (cl-letf (((symbol-function 'comint-send-string)
                                 (lambda (_proc command)
                                   (setq sent-command command)))
                                ((symbol-function 'called-interactively-p)
                                 (lambda (&rest _) t))
                                ((symbol-function 'message)
                                 (lambda (fmt &rest args)
                                   (push (apply #'format fmt args) request-messages))))
                        (czq-comint-completion-refresh-from-process (current-buffer)))
                      (should sent-command)
                      (should (string-match-p "czq_comint_path" sent-command))
                      (should (string-match-p "printf \"<czq-comint handler=elisp>"
                                              sent-command))
                      (should-not (string-match-p "base64" sent-command))
                      (should (string-match-p "<czq-comint handler=elisp>" sent-command))
                      (should (cl-some (lambda (msg)
                                         (string-match-p "requested PATH refresh" msg))
                                       request-messages)))
                    (let ((process-messages '()))
                      (cl-letf (((symbol-function 'message)
                                 (lambda (fmt &rest args)
                                   (push (apply #'format fmt args) process-messages))))
                        (czq-comint--handle-elisp
                         (format "(czq-comint-completion-refresh %S 'process)"
                                 temp-dir)
                         nil))
                      (should (member "bar" czq-comint-completion--cached-commands))
                      (should (cl-some (lambda (msg)
                                         (string-match-p "scanned 1 directories" msg))
                                       process-messages))
                      (should (cl-some (lambda (msg)
                                         (string-match-p "(process)" msg))
                                       process-messages))))
                (when (process-live-p dummy-process)
                  (delete-process dummy-process))))))
      (delete-directory temp-dir t))))

(ert-deftest czq-comint-completion-refresh-message ()
  "Direct refresh should report counts."
  (let* ((temp-dir (make-temp-file "czq-path-refresh" t))
         (script (expand-file-name "baz" temp-dir))
         (messages '()))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\n"))
          (set-file-modes script #o755)
          (cl-letf (((symbol-function 'called-interactively-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-temp-buffer
              (setq-local czq-comint-completion-command-list '("builtin"))
              (setq-local czq-comint-completion--cached-commands nil)
              (czq-comint-completion-refresh temp-dir)
              (should messages)
              (should (string-match-p "scanned 1 directories" (car messages)))
              (should (string-match-p "cached 2 commands" (car messages))))))
      (delete-directory temp-dir t))))

(ert-deftest czq-comint-completion-refresh-from-process-base64 ()
  "Process refresh should support optional base64 encoding."
  (let* ((temp-dir (make-temp-file "czq-path-proc-b64" t))
         (script (expand-file-name "baz" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\n"))
          (set-file-modes script #o755)
          (with-temp-buffer
            (czq-comint-mode)
            (setq-local czq-comint-completion-command-list '())
            (setq-local czq-comint-completion--cached-commands nil)
            (setq-local czq-comint-completion-use-base64 t)
            (let ((dummy-process (start-process "czq-completion-test-b64" (current-buffer) "cat")))
              (unwind-protect
                  (progn
                    (set-process-query-on-exit-flag dummy-process nil)
                    (let ((sent-command nil))
                      (cl-letf (((symbol-function 'comint-send-string)
                                 (lambda (_proc command)
                                   (setq sent-command command))))
                        (czq-comint-completion-refresh-from-process (current-buffer)))
                      (should sent-command)
                      (should (string-match-p "base64" sent-command))
                      (should (string-match-p "base64-decode-string" sent-command))))
                    (let* ((process-messages '())
                           (encoded (base64-encode-string temp-dir t)))
                      (cl-letf (((symbol-function 'message)
                                 (lambda (fmt &rest args)
                                   (push (apply #'format fmt args) process-messages))))
                        (czq-comint--handle-elisp
                         (format "(czq-comint-completion-refresh (base64-decode-string %S) 'process)"
                                 encoded)
                         nil))
                      (should (member "baz" czq-comint-completion--cached-commands))
                      (should (cl-some (lambda (msg)
                                         (string-match-p "scanned 1 directories" msg))
                                       process-messages)))
                (when (process-live-p dummy-process)
                  (delete-process dummy-process))))))
      (delete-directory temp-dir t))))

(ert-deftest czq-comint-completion-includes-files ()
  "Completion should offer files from the tracked directory."
  (let* ((dir (make-temp-file "czq-files" t))
         (file (expand-file-name "script.sh" dir))
         (subdir (expand-file-name "src" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#!/bin/sh\n"))
          (make-directory subdir)
          (with-temp-buffer
            (setq-local czq-comint-current-directory
                        (file-name-as-directory dir))
            (setq-local czq-comint-completion-command-list '("ls"))
            (setq-local czq-comint-completion--cached-commands '("ls"))
            (insert "ls s")
            (pcase-let* ((`(,_ ,_ ,candidates . ,_)
                          (czq-comint-completion-at-point)))
              (should (member "script.sh" candidates))
              (should (member "src/" candidates)))))
      (delete-directory dir t))))

(ert-deftest czq-comint-completion-nested-paths ()
  "Completion should resolve nested relative paths."
  (let* ((dir (make-temp-file "czq-nested" t))
         (src (expand-file-name "src" dir))
         (inner-file (expand-file-name "main.py" src))
         (child (expand-file-name "child" dir))
         (upper-file (expand-file-name "upper.txt" dir)))
    (unwind-protect
        (progn
          (make-directory src)
          (with-temp-file inner-file (insert "print('hi')\n"))
          (make-directory child)
          (with-temp-file upper-file (insert "top\n"))
          (with-temp-buffer
            (setq-local czq-comint-current-directory (file-name-as-directory dir))
            (setq-local czq-comint-completion-command-list '("ls"))
            (setq-local czq-comint-completion--cached-commands '("ls"))
            (insert "ls src/m")
            (pcase-let* ((`(,_ ,_ ,candidates . ,_)
                          (czq-comint-completion-at-point)))
              (should (member "src/main.py" candidates)))
            (erase-buffer)
            (setq-local czq-comint-current-directory (file-name-as-directory dir))
            (setq-local czq-comint-completion-command-list '("ls"))
            (setq-local czq-comint-completion--cached-commands '("ls"))
            (insert "ls ./src/m")
            (pcase-let* ((`(,_ ,_ ,candidates . ,_)
                          (czq-comint-completion-at-point)))
              (should (member "./src/main.py" candidates))))
          (with-temp-buffer
            (setq-local czq-comint-current-directory (file-name-as-directory child))
            (setq-local czq-comint-completion-command-list '("ls"))
            (setq-local czq-comint-completion--cached-commands '("ls"))
            (insert "ls ../u")
            (pcase-let* ((`(,_ ,_ ,candidates . ,_)
                          (czq-comint-completion-at-point)))
              (should (member "../upper.txt" candidates)))))
      (delete-directory dir t))))

(ert-deftest czq-comint-completion-debug-toggle ()
  "Toggling debug should report context messages."
  (let ((original czq-comint-completion-debug)
        (messages '()))
    (unwind-protect
        (progn
          (setq czq-comint-completion-debug nil)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-temp-buffer
              (setq-local czq-comint-completion-command-list '("ls"))
              (setq-local czq-comint-completion--cached-commands '("ls"))
              (czq-comint-completion-toggle-debug)
              (insert "ls")
              (czq-comint-completion-at-point)
              (insert " ./")
              (czq-comint-completion-at-point)
              (should (cl-some (lambda (m) (string-match-p "command" m)) messages))
              (should (cl-some (lambda (m) (string-match-p "file" m)) messages))
              (czq-comint-completion-toggle-debug))))
      (setq czq-comint-completion-debug original))))

(provide 'czq-comint-completion-tests)

;;; czq-comint-completion-tests.el ends here
