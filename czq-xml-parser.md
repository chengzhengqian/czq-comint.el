# CZQ XML Parser Walkthrough

The snippets below can be evaluated sequentially in an Emacs `*scratch*` buffer
to observe how the streaming DFA parser evolves its state.  Adjust the load path
if you are evaluating from outside the project root.

```emacs-lisp
;; Bootstrapping: make the parser definitions available.
(add-to-list 'load-path "/home/chengzhengqian/workspace/czq_emacs_packages/czq-comint/")
(require 'czq-xml-parser)

;; Start with a fresh state (nil is accepted; the parser will create one).
(setq czq-demo-state nil)
```

Feed an initial chunk and inspect the state with the formatter:

```emacs-lisp
(setq czq-demo-step
      (czq-xml-parser-step czq-demo-state "hello <czq-"))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
(setq czq-demo-state (car czq-demo-step))
```

You should see that the plain text buffer contains `"hello "` and the DFA mode
is waiting to complete the tag name.

Continue with another chunk that finishes the opening tag:

```emacs-lisp
(setq czq-demo-step
      (czq-xml-parser-step czq-demo-state "comint lang=\"elisp\">(print 1)\n"))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
(setq czq-demo-state (car czq-demo-step))
```

At this point no tokens are emitted yet—the body buffer accumulates the form
until the closing delimiter arrives.

Finally, provide the closing tag and review the emitted tokens:

```emacs-lisp
(setq czq-demo-step
      (czq-xml-parser-step czq-demo-state "</czq-comint>tail"))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
(setq czq-demo-state (car czq-demo-step))
```

The formatter output now shows the parser back in `:text` mode, the body buffer
cleared, and the `Tokens` line containing one parsed tag followed by the
remaining `"tail"` text fragment.  Each tag now appears as
`((ATTR . VALUE) ... . BODY)`, where the car is an alist of attribute strings.

## Parsing Attributes

Attribute strings can be normalised into `(name . value)` pairs using the new
helper:

```emacs-lisp
(czq-xml-parser-parse-attributes "a=x b=\"y\" c=123")
;; => (("a" . "x") ("b" . "y") ("c" . "123"))
```

;; Tokens line now looks like:
;; Tokens: (((("lang" . "elisp")) . "(print 1)") "tail")

## Handling Partial Tags

To see how the parser recovers from truncated tag boundaries, start a fresh
state and feed a broken opening tag followed by a valid block:

```emacs-lisp
(setq czq-demo-state nil)
(setq czq-demo-step (czq-xml-parser-step czq-demo-state "<czq "))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
(setq czq-demo-state (car czq-demo-step))
;; The partial sequence is emitted as plain text and the state returns to :text.

(setq czq-demo-step (czq-xml-parser-step czq-demo-state "<czq-comint>ok</czq-comint>"))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
```

Likewise, an unfinished closing prefix remains inside the body until the correct
terminator arrives:

```emacs-lisp
(setq czq-demo-state nil)
(setq czq-demo-step (czq-xml-parser-step czq-demo-state "<czq-comint>body</czq "))
(message "%s" (czq-xml-parser-format-step czq-demo-step))
(setq czq-demo-state (car czq-demo-step))

(setq czq-demo-step (czq-xml-parser-step czq-demo-state "</czq-comint>tail"))
(message "%s" (czq-xml-parser-format-step czq-demo-step))


```
