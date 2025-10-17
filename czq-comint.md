# CZQ Comint Filter Notes

This document captures a quick walkthrough for experimenting with the CZQ
comint pre-output filter and its handler dispatch helpers.  It mirrors the
workflow you can run inside `*scratch*` while developing.

## Quick Setup
- Ensure both `czq-xml-parser.el` and `czq-comint.el` are loaded: `(require 'czq-comint)`.
- Optional: enable debug logging to watch handler dispatch decisions.

```elisp
(setq czq-comint-debug t)
```

## Scratch Snippets

Start a fresh buffer state and feed it simulated terminal output:

```elisp
(let ((output "<czq-comint handler=\"elisp\">(+ 1 2)</czq-comint>"))
  (with-temp-buffer
    (czq-comint-mode)
    (list
     :result (czq-comint--preoutput-filter output)
     :state  (czq-xml-parser-format-state czq-comint--parser-state))))
```

- `:result` shows what will be inserted into the comint buffer (`"3\n"` in the example).
- `:state` prints the parser summary after the chunk is processed.

To observe streaming behaviour, split the tag across chunks:

```elisp
(with-temp-buffer
  (czq-comint-mode)
  (list
   (czq-comint--preoutput-filter "<czq-comint handler=\"el")
   (czq-comint--preoutput-filter "isp\">(+ 4 5)</czq-comint>")
   (czq-xml-parser-format-state czq-comint--parser-state)))
```

The list returned is `("" "9\n" STATE)` which confirms the first chunk keeps
the parser mid-tag and the second produces the handler output.

## Running Tests

Run both parser and comint filter tests together:

```sh
emacs --batch -L . \
  -l czq-xml-parser-tests.el \
  -l czq-comint-tests.el \
  -f ert-run-tests-batch-and-exit
```

All tests should report as passing.  Failures will appear with tracebacks that
include the offending comint handler or parser state for easy debugging.
