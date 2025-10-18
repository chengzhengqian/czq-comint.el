# CZQ Comint

CZQ Comint extends `comint-mode` with structured tag parsing so that shell
buffers can embed commands like `<czq-comint handler="elisp">…</czq-comint>`.
Tags are processed by a streaming DFA parser and dispatched to per-handler
functions, letting you evaluate code or suppress text without leaving the
terminal flow.

## Features
- Streaming parser (`czq-xml-parser.el`) that survives chunked process output.
- Tag dispatch with attribute parsing; tokens arrive as `(attrs . body)`.
- Built-in handlers:
  - `elisp` evaluates the body in a temporary buffer, optionally echoing
    results when `results="true"`.
  - `omit` discards the tag entirely.
- Debug logging via `czq-comint-debug` to trace handler resolution.
- Buffer-aware startup commands through `czq-comint-command-alist`.
- Helper script `scripts/czq-comint-emit-tag.sh` to emit sample tags for manual
  testing against a live shell.

## Installation (straight.el)

```elisp
(straight-use-package
 '(czq-comint :type git
              :host github
              :repo "chengzhengqian/czq-comint.el"))

(use-package czq-comint
  :straight t
  :commands (czq-comint-mode czq-comint-run))
```

Adjust the `:repo` value if you host the project elsewhere. Loading the package
pulls in both `czq-comint.el` and the XML parser.

## Usage

### Start a terminal

1. Configure optional startup commands:

   ```elisp
   (setq czq-comint-command-alist
         '(("python" . "python3")
           ("sql"    . "psql")))
   ```

2. Run `M-x czq-comint-run` and supply a buffer name. If the name contains a
   key from `czq-comint-command-alist`, that command runs once bash is ready.
   Otherwise you get a plain bash prompt. The buffer is automatically put into
   `czq-comint-mode`.

### Emit structured tags

- Send output such as `<czq-comint handler="elisp">(+ 1 2)</czq-comint>` from
  the process. The parser accumulates partial chunks until the tag closes.
- Toggle `czq-comint-debug` to `t` to watch dispatch decisions in `*Messages*`.
- Use the provided script for quick manual testing:

  ```sh
  scripts/czq-comint-emit-tag.sh | tee >(cat >/tmp/czq-output.log)
  ```

  Pipe the script into the comint buffer to exercise the built-in handlers and
  the fallback behaviour for unknown handlers.

### Custom handlers

Extend `czq-comint-handlers` with your own `(symbol . function)` pairs. Each
function receives the tag body string and an alist of the remaining attributes.
Return a string to insert into the buffer, or `nil`/`""` to suppress output.

## Testing

Run the automated suite to verify both the parser and the handler dispatcher:

```sh
emacs --batch -L . \
  -l czq-xml-parser-tests.el \
  -l czq-comint-tests.el \
  -f ert-run-tests-batch-and-exit
```

For manual smoke tests, feed `scripts/czq-comint-emit-tag.sh` into a
`czq-comint-mode` buffer and confirm the expected handler output or omission.
