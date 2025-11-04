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
- Flexible send helpers (`czq-comint--send-command-quietly`,
  `czq-comint-send-to-buffer`, `czq-comint-send-to-point`) that either silence
  prompts, redirect output into another buffer, or insert results at point
  while keeping the comint buffer clean.
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
   `czq-comint-run`, and immediately synchronises the shell with that `pwd`
   via `czq-comint--send-command-quietly`, so you do not see duplicate prompts.

3. Call `M-x czq-comint-dirtrack-display-current-directory` at any time to echo
   the tracked working directory in the echo area—handy when shells emit ornate
   prompts or when you want to double-check completion context.

### Refresh PATH-aware completion

- Completions combine `czq-comint-completion-command-list`, executables found
  on the buffer’s `$PATH`, and files rooted at
  `czq-comint-current-directory`.  The cache is seeded from Emacs’ environment
  when the mode starts.
- After exporting a new path within the REPL, run
  `M-x czq-comint-completion-refresh-from-process`.  By default the helper
  prints a `<czq-comint>` tag whose body embeds the live `$PATH` with only
  backslashes and double quotes escaped, so the `elisp` handler can call
  `czq-comint-completion-refresh` with the literal string.  Output is
  temporarily silenced via the render filter while the command runs; the helper
  immediately prints a second tag via `printf '%s\n' …` to call
  `czq-comint-send--complete-quiet` once the shell finishes responding.  Wait
  for the `[czq-completion] scanned … (process)` message before expecting new
  command candidates.
- If the shell environment contains tricky characters, enable the optional
  base64 transport: set the buffer-local
  `czq-comint-completion-use-base64` (or call
  `czq-comint-completion-toggle-base64`).  In that mode the shell encodes the
  path with `base64` and the handler decodes it before refreshing the cache.
  Disable the toggle to fall back to plain quoting or override
  `czq-comint-completion--process-refresh-command` with a custom strategy when
  `base64` is unavailable.
- Toggle `czq-comint-completion-debug` (or call
  `czq-comint-completion-toggle-debug`) to log whether the backend is offering
  command or file candidates.
- Inspect or tweak buffer-local settings (such as `czq-comint-output-enabled`
  or `czq-comint-completion-use-base64`) with `M-x czq-comint-edit-locals`,
  which shows current values and prompts for updates.

### Silent REPL helpers

- Use `czq-comint--send-command-quietly` when you need to run setup commands
  without flashing prompts or echoed text.  The helper installs a render filter
  that discards process output, sends the command (adding a trailing newline if
  necessary), and queues a `<czq-comint handler=elisp>` restore tag that calls
  `czq-comint-send--complete-quiet` to remove the filter after a short timer.
  Tweak `czq-comint-send-quiet-delay` to control how long the quiet window
  lasts before prompts reappear.  The optional third argument to
  `czq-comint--send-command-quietly` extends the quiet window for that invocation—for
  example, `(czq-comint--send-command-quietly proc \"source env/bin/activate\" 1.0)`
  holds the filter for a full second so activation banners stay hidden.  Set the
  buffer-local default via `(setq-local czq-comint-send-quiet-delay 5.0)` when you
  want a longer window everywhere.
- Use `czq-comint-send-edit-quiet-delays` to inspect or update the delay
  interactively in any `czq-comint-mode` buffer.
- Redirect command output elsewhere when you need to capture results: call
  `czq-comint-send-to-buffer` to stream output into another buffer (optionally
  clearing it first) or `czq-comint-send-to-point` to splice results into the
  current buffer at point.  Both helpers drop their filters as soon as the
  restore tag runs, keeping comint output tidy.
- `czq-comint-run` calls the helper automatically to align a new shell with the
  invoking `default-directory`.  You can reuse it from other code—for example,
  to `source` a virtualenv or trigger a PATH refresh—without leaving stray
  prompts in the buffer.

### Emit structured tags

- Send output such as `<czq-comint handler="elisp">(+ 1 2)</czq-comint>` from
  the process. The parser accumulates partial chunks until the tag closes.
- Toggle debug logging with `M-x czq-comint-toggle-debug` (or set
  `czq-comint-debug` non-nil) to watch dispatch decisions in `*Messages*`.
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
- `czq-comint-output-enabled` — when non-nil the pre-output filter inserts
  text; set it to nil to temporarily silence buffer output while still letting
  handlers run.

## Testing

Run the automated suite to verify both the parser and the handler dispatcher:

```sh
emacs --batch -L . \
  -l czq-xml-parser-tests.el \
  -l czq-comint-send-tests.el \
  -l czq-comint-tests.el \
  -f ert-run-tests-batch-and-exit
```

For manual smoke tests, feed `scripts/czq-comint-emit-tag.sh` into a
`czq-comint-mode` buffer and confirm the expected handler output or omission.
