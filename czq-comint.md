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
(let ((output "<czq-comint handler=\"elisp\" results=\"true\">(+ 1 2)</czq-comint>"))
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
   (czq-comint--preoutput-filter "isp\" results=\"true\">(+ 4 5)</czq-comint>")
   (czq-xml-parser-format-state czq-comint--parser-state)))
```

The list returned is `("" "9\n" STATE)` which confirms the first chunk keeps
the parser mid-tag and the second produces the handler output.

By default the built-in `elisp` handler evaluates code for its side effects only.
Add `results="true"` (or any `true`/`yes` style value) to surface the printed
result; errors always surface regardless of the attribute.

## Launching Sessions

Configure per-buffer startup commands by extending
`czq-comint-command-alist`.  Each entry matches a substring in the buffer
name you pass to `czq-comint-run`.  When matched, the command is injected into
the new bash session once it starts; otherwise the shell opens without extra
input.

```elisp
(setq czq-comint-command-alist
      '(("python" . "python3")
        ("sql"    . "psql")))
```

Then invoke:

```elisp
(czq-comint-run "*CZQ Python*") ; runs python3 inside bash
(czq-comint-run "*CZQ Notes*")  ; plain bash shell
```

`czq-comint-run` captures the directory from which you issue the command,
instructs the shell to `cd` there, and records the location in the buffer-local
`czq-comint-current-directory`.  To double-check the tracked path, call:

```elisp
(czq-comint-dirtrack-display-current-directory)
```

The helper displays the cached directory in the echo area, which is especially
useful when prompts are heavily customised or when completions depend on the
current `pwd`.

## PATH-aware Completion

CZQ comint installs a custom completion backend that draws from both a
customizable command list (`czq-comint-completion-command-list`) and the
executables discovered on the buffer’s `$PATH`.  The path scan happens when the
mode starts, but you can ask the running shell for its live `$PATH` at any time:

```elisp
(czq-comint-completion-refresh-from-process)
```

This is particularly handy after running `export PATH=…` within the REPL.  The
command asks the shell to emit a `<czq-comint …>` tag that carries the live
`$PATH`, escaping only backslashes and double quotes so the handler can pass the
literal string to `czq-comint-completion-refresh` with a context of `'process`.
It then emits a message
such as `[czq-completion] scanned … (process)`.  Because the refresh travels
through normal process output it is asynchronous—wait for that message before
expecting new candidates.  If you switch to base64 mode, make sure the shell
offers a `base64` utility (common on POSIX installations).

Internally the refresh uses `czq-comint--send-command-quietly` to momentarily
disable buffer output while the helper command runs.  The same utility is used
when `czq-comint-run` issues the initial `cd`, ensuring the inevitable prompt
that bash prints in response never appears in the buffer.  The helper emits a
small `<czq-comint …>` tag that restores `czq-comint-output-enabled` once the
shell has finished responding.  Pass a *skip* count when the command is expected
to echo additional plain-text output (for example, `pwd` plus a prompt).  You
can reuse the helper whenever you need to send setup commands without flashing
extra prompts.

When the cache is refreshed from inside Emacs (for example during mode
initialisation or by calling `czq-comint-completion-refresh` manually) the code
operates on the raw `$PATH` string from the current environment; no encoding is
required.  If the shell environment includes characters that defeat the simple
escaping, enable the buffer-local
`czq-comint-completion-use-base64` (or call
`czq-comint-completion-toggle-base64`).  In that mode the shell encodes the path
with `base64`, the handler decodes it, and the rest of the workflow remains the
same.  Projects that cannot rely on the binary can override
`czq-comint-completion--process-refresh-command` to emit a differently quoted
tag.

File suggestions are
drawn directly from `czq-comint-current-directory`, so the completion menu stays
aligned with the prompt tracker.  Enable
`czq-comint-completion-debug` (or call
`czq-comint-completion-toggle-debug`) to log whether completions are sourced
from commands or files while inspecting behaviour.

Buffer-local helpers you may want to inspect during development:

- `czq-comint-current-directory` — updated by the prompt tracker and used to
  scope filesystem completion.
- `czq-comint-completion--cached-commands` — current completion candidates;
  regenerated by `czq-comint-completion-refresh` and
  `czq-comint-completion-refresh-from-process`.
- `czq-comint-output-enabled` — controls whether the pre-output filter inserts
  text into the buffer; set it to nil to hush the shell without disabling
  handlers or state updates.

Use `M-x czq-comint-edit-locals` inside a CZQ buffer to view these values and
interactively change them.  The command opens a scratch window listing each
variable and then prompts for the one you would like to update, parsing the new
value as Lisp so you can toggle booleans or provide strings and lists directly.

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
