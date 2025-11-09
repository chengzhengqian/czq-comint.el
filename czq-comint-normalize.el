;;; czq-comint-normalize.el --- Shared normalization helpers -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>

;;; Commentary:
;; Utility functions that normalize process output before render filters run.

;;; Code:

(defun czq-comint-normalize-strip-cr (chunk)
  "Drop carriage returns from CHUNK and tidy Mathematica prompts."
  (when chunk
    (let* ((text (replace-regexp-in-string "\r\n" "\n" chunk))
           (stripped (replace-regexp-in-string "\r" "" text)))
      (replace-regexp-in-string "\\(In\\[[0-9]+\\]:=\\)[ \t]*\\'"
                                "\\1 "
                                stripped))))

(provide 'czq-comint-normalize)

;;; czq-comint-normalize.el ends here
