;;; czq-comint-send-tests.el --- Tests for CZQ comint send helpers -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Focused coverage for `czq-comint-send.el', ensuring the quiet-send helper
;; installs render filters, suppresses output, and tears them down via timers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'czq-comint)
(require 'czq-comint-send)

(defvar czq-comint-send--render-filters)

(ert-deftest czq-comint-send-quiet-suppresses-output ()
  "Quiet helper should silence output until the teardown timer fires."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((process (start-process "czq-send-test" (current-buffer) "cat")))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag process nil)
            (let ((sent "")
                  (czq-comint-output-enabled t)
                  (scheduled nil))
              (cl-letf (((symbol-function 'comint-send-string)
                         (lambda (_proc chunk)
                           (setq sent (concat sent chunk))))
                        ((symbol-function 'run-at-time)
                         (lambda (secs repeat fn &rest args)
                           (should (null repeat))
                           (let ((id (gensym "timer")))
                             (setq scheduled (list :id id
                                                   :secs secs
                                                   :fn fn
                                                   :args args))
                             id)))
                        ((symbol-function 'cancel-timer)
                         (lambda (timer)
                           (when (and scheduled
                                      (eq timer (plist-get scheduled :id)))
                             (setq scheduled nil)))))
                (czq-comint--send-command-quietly process "echo quiet")
                (should (string-match-p "echo quiet\n" sent))
                (should (string-match-p "czq-comint-send--complete-quiet" sent))
                ;; Quiet filter active: suppress arbitrary output.
                (should (equal "" (czq-comint--preoutput-filter "suppressed\n")))
                (should czq-comint-send--render-filters)
                ;; Completing the quiet send schedules the teardown timer.
                (let ((entry (car czq-comint-send--render-filters)))
                  (czq-comint-send--complete-quiet (plist-get entry :id) nil))
                (should scheduled)
                ;; Prompt arriving before the timer fires is still suppressed.
                (should (equal "" (czq-comint--preoutput-filter "prompt\n")))
                (should czq-comint-send--render-filters)
                ;; Fire the timer and ensure the filter is removed.
                (let ((fn (plist-get scheduled :fn))
                      (args (plist-get scheduled :args)))
                  (setq scheduled nil)
                  (apply fn args))
                (should (null czq-comint-send--render-filters))
                (should (equal "visible\n" (czq-comint--preoutput-filter "visible\n"))))))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest czq-comint-send-quiet-appends-newline ()
  "Quiet helper should normalize commands and reject empty payloads."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((process (start-process "czq-send-test" (current-buffer) "cat")))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag process nil)
            (let ((sent nil))
              (cl-letf (((symbol-function 'comint-send-string)
                         (lambda (_proc chunk)
                           (push chunk sent))))
                (czq-comint--send-command-quietly process "printf foo" 0))
              (let ((payload (apply #'concat (nreverse sent))))
                (should (string-match-p "printf foo\n" payload))
                (should (string-match-p "czq-comint-send--complete-quiet" payload))))
            (should-error (czq-comint--send-command-quietly process "" nil))
            (should-error (czq-comint--send-command-quietly process nil nil)))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest czq-comint-send-quiet-uses-explicit-delay ()
  "Explicit delay overrides should control the teardown timer."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((entry (czq-comint-send--register-filter #'czq-comint-send--quiet-filter))
          (czq-comint-output-enabled t)
          (scheduled nil))
      (should entry)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs repeat fn &rest args)
                   (should (null repeat))
                   (let ((id (gensym "timer")))
                     (setq scheduled (list :id id
                                           :secs secs
                                           :fn fn
                                           :args args))
                     id)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (when (and scheduled
                              (eq timer (plist-get scheduled :id)))
                     (setq scheduled nil)))))
        (czq-comint-send--complete-quiet (plist-get entry :id) 0.75))
      (should scheduled)
      (let ((actual (plist-get scheduled :secs)))
        (should (= 0.75 actual)))
      ;; Fire timer to ensure cleanup occurs.
      (let ((fn (plist-get scheduled :fn))
            (args (plist-get scheduled :args)))
        (setq scheduled nil)
        (apply fn args))
      (should (null czq-comint-send--render-filters))))
  ;; Default delay is used when override is nil.
  (with-temp-buffer
    (czq-comint-mode)
    (setq-local czq-comint-send-quiet-delay 0.2)
    (let ((entry (czq-comint-send--register-filter #'czq-comint-send--quiet-filter))
          (scheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs repeat fn &rest args)
                   (should (null repeat))
                   (let ((id (gensym "timer")))
                     (setq scheduled (list :id id
                                           :secs secs
                                           :fn fn
                                           :args args))
                     id))))
        (czq-comint-send--complete-quiet (plist-get entry :id) nil))
      (should (= 0.2 (plist-get scheduled :secs)))
      (let ((fn (plist-get scheduled :fn))
            (args (plist-get scheduled :args)))
        (setq scheduled nil)
        (apply fn args))
      (should (null czq-comint-send--render-filters)))
    (should (null czq-comint-send--render-filters))))

(ert-deftest czq-comint-send-to-buffer-captures-output ()
  "Redirect helper should funnel output into the specified buffer."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((process (start-process "czq-send-test" (current-buffer) "cat"))
          (target (generate-new-buffer " *czq-target*")))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag process nil)
            (with-current-buffer target
              (insert "stale data"))
            (let ((sent nil))
              (cl-letf (((symbol-function 'comint-send-string)
                         (lambda (_proc chunk)
                           (push chunk sent))))
                (czq-comint-send-to-buffer process "printf capture" target nil t))
              (let ((payload (apply #'concat (nreverse sent))))
                (should (string-match-p "printf capture\n" payload))
                (should (string-match-p "czq-comint-send--complete-redirect" payload))))
            (should (equal "" (czq-comint--preoutput-filter "captured\n")))
            (with-current-buffer target
              (should (equal (buffer-string) "captured\n")))
            (let ((entry (car czq-comint-send--render-filters)))
              (czq-comint-send--complete-redirect (plist-get entry :id) 0))
            (should (null czq-comint-send--render-filters)))
        (when (buffer-live-p target)
          (kill-buffer target))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest czq-comint-send-to-point-inserts-output ()
  "Redirecting to a marker should insert text at that position."
  (let ((dest (generate-new-buffer " *czq-dest*")))
    (unwind-protect
        (with-temp-buffer
          (czq-comint-mode)
          (let ((process (start-process "czq-send-test" (current-buffer) "cat")))
            (unwind-protect
                (progn
                  (set-process-query-on-exit-flag process nil)
                  (with-current-buffer dest
                    (insert "before\n" "after")
                    (goto-char (point-min))
                    (forward-line 1))
                  (let ((sent nil)
                        marker)
                    (cl-letf (((symbol-function 'comint-send-string)
                               (lambda (_proc chunk)
                                 (push chunk sent))))
                      (setq marker
                            (with-current-buffer dest
                              (goto-char (point-min))
                              (forward-line 1)
                              (czq-comint-send-to-point process "printf insert"))))
                    (let ((payload (apply #'concat (nreverse sent))))
                      (should (string-match-p "printf insert\n" payload))
                      (should (string-match-p "czq-comint-send--complete-redirect" payload)))
                    (should (equal "" (czq-comint--preoutput-filter "drop-in\n")))
                    (with-current-buffer dest
                      (should (string-equal (buffer-string) "before\ndrop-in\nafter"))
                      (should (eq (marker-buffer marker) dest)))
                    (let ((entry (car czq-comint-send--render-filters)))
                      (czq-comint-send--complete-redirect (plist-get entry :id) 0))
                    (should (null czq-comint-send--render-filters))))
              (when (process-live-p process)
                (delete-process process)))))
      (when (buffer-live-p dest)
        (kill-buffer dest))))) 

(ert-deftest czq-comint-send-to-buffer-integration ()
  "End-to-end check that a real comint process redirects output elsewhere."
  (let* ((comint-buffer (get-buffer-create " *czq-integration*"))
         (log-buffer (get-buffer-create " *czq-integration-log*"))
         (proc nil))
    (unwind-protect
        (progn
          (with-current-buffer comint-buffer
            (setq default-directory temporary-file-directory))
          (make-comint-in-buffer "czq-integration"
                                 comint-buffer
                                 "bash" nil "--noprofile" "--norc")
          (setq proc (get-buffer-process comint-buffer))
          (with-current-buffer comint-buffer
            (czq-comint-mode))
          (accept-process-output proc 0.2)
          (with-current-buffer log-buffer
            (erase-buffer))
          (with-current-buffer comint-buffer
            (czq-comint-send-to-buffer proc "printf integration" log-buffer nil t))
          (accept-process-output proc 0.3)
          (with-current-buffer log-buffer
            (should (string-match-p "integration" (buffer-string))))
          (with-current-buffer comint-buffer
            (let ((tail (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 80))
                         (point-max))))
              (should-not (string-match-p "integration" tail)))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p comint-buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer comint-buffer)))
      (when (buffer-live-p log-buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer log-buffer))))))

(ert-deftest czq-comint-send-edit-quiet-delays-updates-values ()
  "Interactive editor should update buffer-local quiet delay variables."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((inputs '("1.5")))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   (pop inputs)))
                ((symbol-function 'message) #'ignore))
        (czq-comint-send-edit-quiet-delays)))
    (should (= czq-comint-send-quiet-delay 1.5))))

(ert-deftest czq-comint-send-edit-quiet-delays-allows-ret ()
  "Hitting RET at the prompts should keep existing values."
  (with-temp-buffer
    (czq-comint-mode)
    (setq-local czq-comint-send-quiet-delay 0.3)
    (let ((inputs '("")))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   (pop inputs)))
                ((symbol-function 'message) #'ignore))
        (czq-comint-send-edit-quiet-delays)))
    (should (= czq-comint-send-quiet-delay 0.3))))

(provide 'czq-comint-send-tests)

;;; czq-comint-send-tests.el ends here
