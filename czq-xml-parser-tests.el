;;; czq-xml-parser-tests.el --- Tests for czq-xml-parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic regression tests for the streaming CZQ tag parser.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'czq-xml-parser)

(ert-deftest czq-xml-parser-basic-tag ()
  (pcase-let* ((`(,state . ,tokens)
                (czq-xml-parser-step nil "<czq-comint>body</czq-comint>")))
    (should (czq-xml-parser-state-p state))
    (should (equal tokens '((nil . "body"))))))

(ert-deftest czq-xml-parser-streaming-chunks ()
  (let* ((step1 (czq-xml-parser-step nil "hello <czq-"))
         (state (car step1))
         (tokens1 (cdr step1)))
    (should (equal tokens1 '("hello ")))
    (pcase-let* ((`(,state2 . ,tokens2)
                  (czq-xml-parser-step state "comint lang=\"elisp\">(print 1)")))
      (should (eq state state2))
      (should (null tokens2))
      (pcase-let* ((`(,state3 . ,tokens3)
                    (czq-xml-parser-step state2 "</czq-comint>world")))
        (should (equal tokens3 '(((("lang" . "elisp")) . "(print 1)") "world")))
        (should (eq state2 state3))))))

(ert-deftest czq-xml-parser-ignores-false-close ()
  (pcase-let* ((`(,state . ,tokens)
                (czq-xml-parser-step nil "<czq-comint></no")))
    (should (null tokens))
    (pcase-let* ((`(,state2 . ,tokens2)
                  (czq-xml-parser-step state "></czq-comint>")))
      (should (equal tokens2 '((nil . "</no>"))))
      (should (eq state state2)))))

(ert-deftest czq-xml-parser-trims-attributes ()
  (pcase-let* ((`(_ . ,tokens)
                (czq-xml-parser-step nil "<czq-comint   foo=\"1\"   bar=yes>body</czq-comint>")))
    (should (equal tokens '(((("foo" . "1") ("bar" . "yes")) . "body"))))))

(ert-deftest czq-xml-parser-parse-attributes-basic ()
  (should (equal (czq-xml-parser-parse-attributes "a=x b=y")
                 '(("a" . "x") ("b" . "y")))))

(ert-deftest czq-xml-parser-parse-attributes-quotes ()
  "Ensure quoted and unquoted values normalise to the same string."
  (should (equal (czq-xml-parser-parse-attributes "a=\"123\"")
                 '(("a" . "123"))))
  (should (equal (czq-xml-parser-parse-attributes "a=123")
                 '(("a" . "123")))))

(ert-deftest czq-xml-parser-open-prefix-treated-as-text ()
  "A partial tag name should fall back to plain text before parsing resumes."
  (let* ((step1 (czq-xml-parser-step nil "<czq "))
         (state1 (car step1))
         (tokens1 (cdr step1)))
    (should (equal tokens1 '("<czq ")))
    (should (eq (czq-xml-parser-state-mode state1) :text))
    (pcase-let* ((`(_ . ,tokens2)
                  (czq-xml-parser-step state1 "<czq-comint>ok</czq-comint>")))
      (should (equal tokens2 '((nil . "ok")))))))

(ert-deftest czq-xml-parser-close-prefix-treated-as-body ()
  "A malformed closing prefix should be preserved in the body and not break parsing."
  (let* ((step1 (czq-xml-parser-step nil "<czq-comint>body</czq "))
         (state1 (car step1)))
    (should (null (cdr step1)))
    (should (eq (czq-xml-parser-state-mode state1) :tag-body))
    (should (equal (czq-xml-parser--snapshot-chars (czq-xml-parser-state-body-chars state1))
                   "body</czq "))
    (should (= (czq-xml-parser-state-closing-progress state1) 0))
    (pcase-let* ((`(_ . ,tokens2)
                  (czq-xml-parser-step state1 "</czq-comint>tail")))
      (should (equal tokens2 '((nil . "body</czq ") "tail"))))))

(provide 'czq-xml-parser-tests)

;;; czq-xml-parser-tests.el ends here
