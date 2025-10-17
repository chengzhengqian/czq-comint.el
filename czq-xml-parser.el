;;; czq-xml-parser.el --- Streaming DFA parser for CZQ tags -*- lexical-binding: t; -*-

;; Author: Chengzhengqian <chengzhengqian@example.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: parsing, tools

;;; Commentary:
;;
;; This module provides a streaming parser tailored to CZQ-style tags in
;; the form <czq-comint ...>...</czq-comint>.  The parser is implemented
;; as an explicit DFA whose state can be persisted between invocations.
;; Each invocation consumes a new chunk of text and returns the updated
;; state together with any concrete parse results.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (czq-xml-parser-state
               (:constructor czq-xml-parser-state-create (&key (tag-name "czq-comint"))))
  "Internal state for the CZQ streaming DFA parser."
  tag-name
  (mode :text)
  text-chars
  pending-chars
  (tag-progress 0)
  attr-chars
  current-attr
  body-chars
  closing-chars
  (closing-progress 0))

(defun czq-xml-parser--chars->string (chars)
  "Turn reversed character list CHARS into a string."
  (apply #'string (nreverse chars)))

(defun czq-xml-parser--append-pending-to-text (state)
  "Append pending characters in STATE to the text buffer."
  (let ((pending (czq-xml-parser-state-pending-chars state)))
    (when pending
      (dolist (ch (nreverse pending))
        (push ch (czq-xml-parser-state-text-chars state))))
    (setf (czq-xml-parser-state-pending-chars state) nil)))

(defun czq-xml-parser--flush-text (state results)
  "Flush accumulated plain text from STATE into RESULTS."
  (let ((chars (czq-xml-parser-state-text-chars state)))
    (when chars
      (push (czq-xml-parser--chars->string chars) results)
      (setf (czq-xml-parser-state-text-chars state) nil))
    results))

(defun czq-xml-parser--rewind-closing (state)
  "Move partially matched closing delimiter back into the tag body."
  (let ((closing (czq-xml-parser-state-closing-chars state)))
    (when closing
      (dolist (ch (nreverse closing))
        (push ch (czq-xml-parser-state-body-chars state)))))
  (setf (czq-xml-parser-state-closing-chars state) nil)
  (setf (czq-xml-parser-state-closing-progress state) 0))

(defun czq-xml-parser--start-pending (state char)
  "Initialise pending information in STATE with CHAR."
  (setf (czq-xml-parser-state-pending-chars state) (list char))
  (setf (czq-xml-parser-state-tag-progress state) 0)
  (setf (czq-xml-parser-state-attr-chars state) nil)
  (setf (czq-xml-parser-state-current-attr state) nil)
  (setf (czq-xml-parser-state-body-chars state) nil)
  (setf (czq-xml-parser-state-closing-chars state) nil)
  (setf (czq-xml-parser-state-closing-progress state) 0))

(defun czq-xml-parser--snapshot-chars (chars)
  "Return a readable string built from reversed CHARS without mutating state."
  (if chars
      (apply #'string (nreverse (copy-sequence chars)))
    ""))

(defun czq-xml-parser-parse-attributes (attr-string)
  "Parse ATTR-STRING into an alist of (NAME . VALUE) pairs.

Entries are expected to use the `name=value' form separated by
whitespace.  Values may be wrapped in double quotes; quoting does not
affect the stored value.  Both names and values are returned as
strings.  Malformed pairs raise an error."
  (let* ((len (length (or attr-string "")))
         (pos 0)
         (pairs '()))
    (cl-labels
        ((skip-space ()
           (while (and (< pos len)
                       (memq (aref attr-string pos) '(?\s ?\t ?\n ?\r ?\f)))
             (setq pos (1+ pos))))
         (parse-name ()
           (let ((start pos))
             (while (and (< pos len)
                         (not (memq (aref attr-string pos) '(?\s ?\t ?\n ?\r ?\f ?=))))
               (setq pos (1+ pos)))
             (when (= start pos)
               (error "Expected attribute name at position %d" pos))
             (substring attr-string start pos)))
         (expect-char (char)
           (when (>= pos len)
             (error "Expected %c but reached end of attribute string" char))
           (let ((current (aref attr-string pos)))
             (unless (= current char)
               (error "Expected %c at position %d, saw %c" char pos current))
             (setq pos (1+ pos))))
         (parse-value ()
           (when (>= pos len)
             (error "Expected value at position %d" pos))
           (let ((delimiter (aref attr-string pos)))
             (if (= delimiter ?\")
                 (progn
                   (setq pos (1+ pos))
                   (let ((start pos))
                     (while (and (< pos len) (not (= (aref attr-string pos) ?\")))
                       (setq pos (1+ pos)))
                     (when (>= pos len)
                       (error "Unterminated quoted value starting at %d" start))
                     (prog1 (substring attr-string start pos)
                       (setq pos (1+ pos)))))
               (let ((start pos))
                 (while (and (< pos len)
                             (not (memq (aref attr-string pos) '(?\s ?\t ?\n ?\r ?\f))))
                   (setq pos (1+ pos)))
                 (substring attr-string start pos))))))
      (while (< pos len)
        (skip-space)
        (when (< pos len)
          (let ((name (parse-name)))
            (skip-space)
            (expect-char ?=)
            (skip-space)
            (let ((value (parse-value)))
              (push (cons name value) pairs))))))
    (nreverse pairs)))

(defun czq-xml-parser-format-state (state)
  "Return a human-readable summary describing parser STATE."
  (unless (czq-xml-parser-state-p state)
    (error "STATE is not a czq-xml-parser-state"))
  (let* ((tag-name (czq-xml-parser-state-tag-name state))
         (tag-length (length tag-name))
         (mode (czq-xml-parser-state-mode state))
         (tag-progress (czq-xml-parser-state-tag-progress state))
         (tag-progress (cond
                        ((memq mode '(:tag-post-name :tag-attr :tag-body))
                         tag-length)
                        ((eq mode :text)
                         0)
                        (t tag-progress)))
         (text (czq-xml-parser--snapshot-chars (czq-xml-parser-state-text-chars state)))
         (pending (czq-xml-parser--snapshot-chars (czq-xml-parser-state-pending-chars state)))
         (attr-buffer (czq-xml-parser--snapshot-chars (czq-xml-parser-state-attr-chars state)))
         (attr (czq-xml-parser-state-current-attr state))
         (body-buffer (czq-xml-parser--snapshot-chars (czq-xml-parser-state-body-chars state)))
         (closing-buffer (czq-xml-parser--snapshot-chars (czq-xml-parser-state-closing-chars state)))
         (closing-progress (czq-xml-parser-state-closing-progress state))
         (closing-length (length (concat "</" tag-name ">"))))
    (string-join
     (list
      (format "Mode: %S" mode)
      (format "Tag name: %s (progress %d/%d)" tag-name tag-progress tag-length)
      (format "Pending buffer: %S" pending)
      (format "Text buffer: %S" text)
      (format "Attr buffer: %S" attr-buffer)
      (format "Current attr: %S" attr)
      (format "Body buffer: %S" body-buffer)
      (format "Closing buffer: %S (progress %d/%d)" closing-buffer closing-progress closing-length))
     "\n")))

(defun czq-xml-parser-format-step (step-result)
  "Return a human-readable summary for STEP-RESULT plus emitted tokens.

STEP-RESULT should be the cons cell produced by `czq-xml-parser-step'.
The summary focuses on the DFA state while also echoing the emitted
tokens for convenience."
  (let* ((state (car step-result))
         (tokens (cdr step-result)))
    (format "%s\nTokens: %S"
            (czq-xml-parser-format-state state)
            tokens)))

(defun czq-xml-parser-step (state chunk)
  "Consume CHUNK of text with DFA STATE, returning updated state and results.

STATE is an instance of `czq-xml-parser-state'.  CHUNK should be a
string (nil is treated as an empty string).  The second value in the
returned cons cell is the list of concrete parse results produced by
this invocation.  Plain text segments are emitted as strings; parsed
tags are emitted as cons cells whose car is the attribute string and
cdr is the tag body."
  (unless (czq-xml-parser-state-p state)
    (setq state (czq-xml-parser-state-create)))
  (setq chunk (or chunk ""))
  (let* ((tag-name (czq-xml-parser-state-tag-name state))
         (tag-length (length tag-name))
         (closing-pattern (concat "</" tag-name ">"))
         (closing-length (length closing-pattern))
         (results '())
         (len (length chunk))
         (i 0))
    (while (< i len)
      (let ((char (aref chunk i))
            (advance t))
        (while advance
          (pcase (czq-xml-parser-state-mode state)
            (':text
             (if (= char ?<)
                 (progn
                   (czq-xml-parser--start-pending state char)
                   (setf (czq-xml-parser-state-mode state) :tag-open)
                   (setq advance nil))
               (push char (czq-xml-parser-state-text-chars state))
               (setq advance nil)))
            (':tag-open
             (let ((expected (aref tag-name 0)))
               (if (char-equal char expected)
                   (progn
                     (push char (czq-xml-parser-state-pending-chars state))
                     (setf (czq-xml-parser-state-tag-progress state) 1)
                     (setf (czq-xml-parser-state-mode state) :tag-name)
                     (setq advance nil))
                 (czq-xml-parser--append-pending-to-text state)
                 (setf (czq-xml-parser-state-mode state) :text))))
            (':tag-name
             (let ((progress (czq-xml-parser-state-tag-progress state)))
               (if (and (< progress tag-length)
                        (char-equal char (aref tag-name progress)))
                   (progn
                     (push char (czq-xml-parser-state-pending-chars state))
                     (setf (czq-xml-parser-state-tag-progress state) (1+ progress))
                     (when (= (1+ progress) tag-length)
                       (setf (czq-xml-parser-state-mode state) :tag-post-name))
                     (setq advance nil))
                 (czq-xml-parser--append-pending-to-text state)
                 (setf (czq-xml-parser-state-mode state) :text))))
            (':tag-post-name
             (cond
              ((memq char '(?\s ?\t ?\n ?\r ?\f))
               (setf (czq-xml-parser-state-mode state) :tag-attr)
               (push char (czq-xml-parser-state-attr-chars state))
               (setq advance nil))
              ((= char ?>)
               (setf (czq-xml-parser-state-current-attr state) "")
               (setf (czq-xml-parser-state-mode state) :tag-body)
               (setf (czq-xml-parser-state-pending-chars state) nil)
               (setf (czq-xml-parser-state-tag-progress state) 0)
               (setf (czq-xml-parser-state-body-chars state) nil)
               (setf (czq-xml-parser-state-closing-chars state) nil)
               (setf (czq-xml-parser-state-closing-progress state) 0)
               (setq results (czq-xml-parser--flush-text state results))
               (setq advance nil))
              (t
               (setf (czq-xml-parser-state-mode state) :tag-attr)
               (push char (czq-xml-parser-state-attr-chars state))
               (setq advance nil))))
            (':tag-attr
             (if (= char ?>)
                 (let ((attr (string-trim
                              (czq-xml-parser--chars->string
                               (or (czq-xml-parser-state-attr-chars state) '())))))
                   (setf (czq-xml-parser-state-current-attr state) attr)
                   (setf (czq-xml-parser-state-attr-chars state) nil)
                   (setf (czq-xml-parser-state-mode state) :tag-body)
                   (setf (czq-xml-parser-state-pending-chars state) nil)
                   (setf (czq-xml-parser-state-tag-progress state) 0)
                   (setf (czq-xml-parser-state-body-chars state) nil)
                   (setf (czq-xml-parser-state-closing-chars state) nil)
                   (setf (czq-xml-parser-state-closing-progress state) 0)
                   (setq results (czq-xml-parser--flush-text state results))
                   (setq advance nil))
               (push char (czq-xml-parser-state-attr-chars state))
               (setq advance nil)))
            (':tag-body
             (let ((closing-progress (czq-xml-parser-state-closing-progress state)))
               (if (zerop closing-progress)
                   (if (= char ?<)
                       (progn
                         (push char (czq-xml-parser-state-closing-chars state))
                         (setf (czq-xml-parser-state-closing-progress state) 1)
                         (setq advance nil))
                     (push char (czq-xml-parser-state-body-chars state))
                     (setq advance nil))
                 (let ((expected (aref closing-pattern closing-progress)))
                   (if (char-equal char expected)
                       (progn
                         (push char (czq-xml-parser-state-closing-chars state))
                         (let ((new-progress (1+ closing-progress)))
                           (setf (czq-xml-parser-state-closing-progress state) new-progress)
                           (if (= new-progress closing-length)
                               (let* ((body (czq-xml-parser--chars->string
                                             (or (czq-xml-parser-state-body-chars state) '())))
                                      (attr (or (czq-xml-parser-state-current-attr state) ""))
                                      (attr-list (czq-xml-parser-parse-attributes attr)))
                                 (push (cons attr-list body) results)
                                 (setf (czq-xml-parser-state-mode state) :text)
                                 (setf (czq-xml-parser-state-body-chars state) nil)
                                 (setf (czq-xml-parser-state-current-attr state) nil)
                                 (setf (czq-xml-parser-state-closing-chars state) nil)
                                 (setf (czq-xml-parser-state-closing-progress state) 0)
                                 (setq advance nil))
                             (setq advance nil))))
                     (czq-xml-parser--rewind-closing state))))))
            (_
             (error "Unknown czq-xml-parser state: %S"
                    (czq-xml-parser-state-mode state)))))
        (setq i (1+ i))))
    (setq results (czq-xml-parser--flush-text state results))
    (cons state (nreverse results))))

(provide 'czq-xml-parser)

;;; czq-xml-parser.el ends here
