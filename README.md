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
- Prompt-based directory tracking that mirrors the shell’s `pwd` and exposes
  `czq-comint-dirtrack-display-current-directory` for quick inspection.
- Completion backend seeded from both a customizable command list and the shell
  process’ live `$PATH`; refresh interactively with
  `czq-comint-completion-refresh-from-process`.
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
   `czq-comint-mode`, starts in the directory from which you invoked
   `czq-comint-run`, and immediately synchronises the shell with that `pwd`.

3. Call `M-x czq-comint-dirtrack-display-current-directory` at any time to echo
   the tracked working directory in the echo area—handy when shells emit ornate
   prompts or when you want to double-check completion context.

### Refresh PATH-aware completion

- CZQ comint’s completion list combines `czq-comint-completion-command-list`
  with executables discovered via the buffer’s `$PATH` and file names from the
  tracked working directory.  On mode activation the cache is seeded from
  Emacs’ own environment; run `czq-comint-completion-refresh-from-process`
  after changing `$PATH` inside the REPL to pull the live shell value.
- File suggestions follow `czq-comint-current-directory`, so completions stay in
  sync with whatever the directory tracker reports.
- Toggle `czq-comint-completion-debug` (or call
  `czq-comint-completion-toggle-debug`) to log whether the backend is offering
  command or file candidates.
- File suggestions follow `czq-comint-current-directory`, so completions stay in
  sync with whatever the directory tracker reports.
- After exporting a new path inside the REPL, run
  `M-x czq-comint-completion-refresh-from-process` to rescan the shell and
  update the cached candidates immediately.

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

### Buffer-local state

- `czq-comint-current-directory` — directory tracker cache synchronised with
  the most recent shell prompt.
- `czq-comint-completion--cached-commands` — merged command list used by the
  completion backend; refreshed on mode start and on demand.

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
