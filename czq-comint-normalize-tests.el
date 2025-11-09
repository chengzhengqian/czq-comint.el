;;; czq-comint-normalize-tests.el --- Tests for normalize filters -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>

;;; Commentary:
;; Ensures normalization filters run before render filters.

;;; Code:

(require 'ert)
(require 'czq-comint)

(ert-deftest czq-comint-normalize-filter-defaults-to-noop ()
  (with-temp-buffer
    (czq-comint-mode)
    (should (equal "plain\n" (czq-comint--preoutput-filter "plain\n")))))

(ert-deftest czq-comint-normalize-strip-cr ()
  (with-temp-buffer
    (czq-comint-mode)
    (czq-comint-set-normalize-filter #'czq-comint-normalize-strip-cr)
    (should (equal "a+b\n" (czq-comint--preoutput-filter "\ra\r\r+\r\rb\r\r\n")))
    (should (equal "In[3]:= "
                   (czq-comint--preoutput-filter "\rIn[3]:=\r")))
    (should (equal "In[3]:= "
                   (czq-comint--preoutput-filter "\rIn[3]:=                                                                \r")))
    (czq-comint-clear-normalize-filter)))

(provide 'czq-comint-normalize-tests)

;;; czq-comint-normalize-tests.el ends here
