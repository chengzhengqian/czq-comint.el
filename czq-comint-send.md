# CZQ Comint Send Helpers

_Path_: `czq-comint-send.md`

```elisp
;; Adjust the path if you cloned the repo elsewhere.
(setq czq-root (expand-file-name "czq-comint"
                                  (file-name-directory
                                   (or load-file-name default-directory))))
  (add-to-list 'load-path czq-root)
  (require 'czq-comint-send)
```

This note documents the new `czq-comint-send.el` module.  The goal is to keep
all REPL-send mechanics (quiet execution, output redirection, future streaming
features) decoupled from `czq-comint.el` so higher-level packages can call into
the same, well-documented API.

## Quick Start: Quiet Commands

```elisp
(require 'czq-comint)
(require 'czq-comint-send)

(czq-comint-run "*test-comint*")
(let* ((proc (get-buffer-process "*test-comint*")))
  (with-current-buffer (process-buffer proc)
    (czq-comint--send-command-quietly proc "echo go")
    (accept-process-output proc (+ czq-comint-send-quiet-teardown-delay 0.05))))
```

The helper:

1. Verifies the process is live and its buffer is still around.
2. Registers a *render filter* that silently drops every chunk emitted by the
   process.
3. Sends your command (ensuring it ends with `\n`), then appends a small
   `<czq-comint handler=elisp>` tag that executes
   `(czq-comint-send--complete-quiet …)` inside Emacs once the shell responds.
4. `czq-comint-send--complete-quiet` schedules a short timer (defaults to
   `czq-comint-send-quiet-teardown-delay`) that removes the render filter once
   the prompt is expected to have arrived.  Pass a numeric third argument to
   `czq-comint--send-command-quietly` when you need to extend that delay;
   each increment adds `czq-comint-send-quiet-extra-delay` seconds.

The example simply waits just over the teardown delay so the timer has a chance
to fire.  Once the delay elapses the shell resumes normal rendering and the
next prompt becomes visible automatically.

Because output suppression is scoped to a render filter, concurrent quiet
commands do not fight over `czq-comint-output-enabled`.  The filter stack is
buffer-local and unwinds cleanly even if the restore tag never arrives (for
example, when the process dies).

## Helper Commands

- `czq-comint-send-edit-quiet-delays` — interactively inspect or adjust the
  quiet window in the current buffer.  The command prompts for both teardown and
  per-increment delays (press RET to keep the existing value) and makes the
  results buffer-local.

## Render Filter Pipeline

Every chunk processed by `czq-comint--preoutput-filter` now flows through a
stack of render filters *after* tag parsing but *before* it reaches comint.
Filters run in LIFO order, so the most recently registered filter sees the
chunk first and can short-circuit emission if needed.  Each filter receives the
rendered string and returns the string (possibly transformed) to feed into the
next filter.  They may capture side effects (collect output, forward it
elsewhere, etc.).

Key building blocks in `czq-comint-send.el`:

| Helper | Purpose |
|--------|---------|
| `czq-comint-send--register-filter` | Push a filter onto the stack, optionally with a finalizer. |
| `czq-comint-send--remove-filter` | Pop a filter manually, running its finalizer. |
| `czq-comint-send--apply-render-filters` | Internal entrypoint invoked from `czq-comint.el` after accumulation. |

While only the quiet helper is public today, future sending APIs should follow
the same pattern:

1. Register a filter that implements the desired behaviour (record, redirect,
   transform, etc.).
2. Send the command (quietly or not).
3. Arrange for a follow-up tag or hook to call `czq-comint-send--remove-filter`
   when you're done.

Internally, `czq-comint--preoutput-filter` (from `czq-comint.el`) invokes
`czq-comint-send--apply-render-filters` after it converts parser tokens into a
single string.  Whatever the filters return becomes the text that comint shows.
Send helpers therefore only need to register a filter before talking to the
process and make sure they remove it afterwards.  For quiet sends this cleanup
is triggered by the restore tag, which executes
`czq-comint-send--complete-quiet` inside Emacs.  Because the filter stack is
buffer-local and stack-based, multiple concurrent sends do not interfere with
each other: each command owns its filter entry and removes it when finished.

## Example: Capturing Output

```elisp
(defun czq-comint-send-capture (process command)
  "Send COMMAND and return the captured output as a string."
  (let (accumulator)
    (with-current-buffer (process-buffer process)
      (let* ((entry
              (czq-comint-send--register-filter
               (lambda (chunk)
                 (setq accumulator (concat accumulator chunk))
                 ""))))
        (unwind-protect
            (progn
              (comint-send-string process (concat command "\n"))
              (accept-process-output process 0.1)) ; wait for response
          (czq-comint-send--remove-filter entry))))
    accumulator))
```

This snippet sketches how the render filter stack can be used to build richer
send helpers without touching `czq-comint.el`.  You can extend the idea to
redirect into a log buffer, emit notifications, or feed completion data back to
source buffers.

## Design Notes

- Filters operate on already-rendered strings, so they see the same text the
  user would.  Handlers and directory tracking ran earlier, so state is kept in
  sync even when output is suppressed.
- Quiet sends keep their render filter alive until the teardown timer fires.
  Tweak `czq-comint-send-quiet-teardown-delay` (and the extra delay increments)
  to decide how long the suppression window lasts.
- Both timing variables are buffer-local.  Call
  `(setq-local czq-comint-send-quiet-teardown-delay 5.0)` for a one-off long
  window, or adjust `czq-comint-send-quiet-extra-delay` when the optional third
  argument should add a larger increment.
- `czq-comint--send-command-quietly` remains the backwards-compatible public
  entry point.  Higher-level features should wrap it (or compose their own
  filters) instead of reusing the legacy `czq-comint-output-enabled` flag.
- Upcoming modules (`czq-comint-source.el`, completion adapters, etc.) can
  rely on this layer for all process I/O concerns, keeping the rest of the
  codebase agnostic of how commands actually reach the REPL.
