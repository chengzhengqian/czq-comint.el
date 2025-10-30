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
                (czq-comint--send-command-quietly process "echo quiet" 0)
                (should (string-match-p "echo quiet\n" sent))
                (should (string-match-p "czq-comint-send--complete-quiet" sent))
                ;; Quiet filter active: suppress arbitrary output.
                (should (equal "" (czq-comint--preoutput-filter "suppressed\n")))
                (should czq-comint-send--render-filters)
                ;; Completing the quiet send schedules the teardown timer.
                (let ((entry (car czq-comint-send--render-filters)))
                  (czq-comint-send--complete-quiet (plist-get entry :id) 0))
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

(ert-deftest czq-comint-send-quiet-timer-respects-extra-delay ()
  "Extra skip increments should extend the teardown delay."
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
        (czq-comint-send--complete-quiet (plist-get entry :id) 2))
      (should scheduled)
      (let* ((expected (+ czq-comint-send-quiet-teardown-delay
                          (* 2 czq-comint-send-quiet-extra-delay)))
             (actual (plist-get scheduled :secs)))
        (should (equal expected actual)))
      ;; Fire timer to ensure cleanup occurs.
      (let ((fn (plist-get scheduled :fn))
            (args (plist-get scheduled :args)))
        (setq scheduled nil)
        (apply fn args))
      (should (null czq-comint-send--render-filters)))))

(ert-deftest czq-comint-send-edit-quiet-delays-updates-values ()
  "Interactive editor should update buffer-local quiet delay variables."
  (with-temp-buffer
    (czq-comint-mode)
    (let ((inputs '("1.5" "0.2")))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   (pop inputs)))
                ((symbol-function 'message) #'ignore))
        (czq-comint-send-edit-quiet-delays)))
    (should (= czq-comint-send-quiet-teardown-delay 1.5))
    (should (= czq-comint-send-quiet-extra-delay 0.2))))

(ert-deftest czq-comint-send-edit-quiet-delays-allows-ret ()
  "Hitting RET at the prompts should keep existing values."
  (with-temp-buffer
    (czq-comint-mode)
    (setq-local czq-comint-send-quiet-teardown-delay 0.3)
    (setq-local czq-comint-send-quiet-extra-delay 0.05)
    (let ((inputs '("" "")))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   (pop inputs)))
                ((symbol-function 'message) #'ignore))
        (czq-comint-send-edit-quiet-delays)))
    (should (= czq-comint-send-quiet-teardown-delay 0.3))
    (should (= czq-comint-send-quiet-extra-delay 0.05))))

(provide 'czq-comint-send-tests)

;;; czq-comint-send-tests.el ends here
