# CZQ Comint Send Helpers

`czq-comint-send.el` concentrates every “send this command, but shape the
output” concern in one place.  The module exposes a small render-filter stack,
the public quiet-send helper, and interactive tooling to tune the timers that
keep prompts hidden while shell commands run.

## Loading

```elisp
;; Adjust the path if you cloned the repo elsewhere.
(add-to-list 'load-path "/path/to/czq-comint/")
(require 'czq-comint)
(require 'czq-comint-send)
```

`czq-comint` pulls in `czq-comint-send` automatically, but requiring it
explicitly makes the quiet-edit command available even when you are not inside
`czq-comint-mode`.

## Render Filters in a Nutshell

`czq-comint--preoutput-filter` (defined in `czq-comint.el`) asks
`czq-comint-send--apply-render-filters` to post-process the fully rendered
string that would otherwise reach the comint buffer untouched.  Filters live in
a buffer-local LIFO stack; each one receives the text produced by the next
filter in the stack (or the raw string for the most recently registered entry)
and returns the string to forward downstream.

Key building blocks:

| Function | Purpose |
| -- | -- |
| `czq-comint-send--register-filter` | Push a filter closure and optional finalizer. |
| `czq-comint-send--remove-filter` | Pop an entry, cancelling any pending timer and running its finalizer. |
| `czq-comint-send--apply-render-filters` | Internal entry point invoked from the pre-output filter. |
| `czq-comint-send--debug` | Mirrors `czq-comint-debug` output so you can trace filter lifetime. |

Higher-level helpers should always follow the same recipe: register a filter,
send the command, then arrange to remove the filter when the command finishes
(either immediately or via a tag/timer).

## Quiet Commands

`czq-comint--send-command-quietly` is the one public entry point today.  It:

1. Validates that the process and its buffer are both live.
2. Registers a render filter that replaces every chunk with `""`, keeping the
   shell silent.
3. Normalises the command so it always ends with a newline.
4. Appends a restore tag:

   ```elisp
   <czq-comint handler=elisp>
     (czq-comint-send--complete-quiet 'FILTER-ID DELAY)
   </czq-comint>
   ```

   The tag runs inside Emacs once the shell echoes it, giving us a hook to tear
   down the quiet window.

5. Sends both payload and tag through `comint-send-string`.

`czq-comint-send--complete-quiet` looks up the filter entry and schedules a
timer via `czq-comint-send--schedule-removal`.  The timer delay defaults to
`czq-comint-send-quiet-delay`, but you can override that per command via the
optional third argument to `czq-comint--send-command-quietly`.  When the timer fires,
`czq-comint-send--teardown-filter` removes the entry so the next prompt renders
normally.  Passing zero (or any negative number, which is clamped to zero)
collapses the window immediately.

Because the suppression lives inside the render-filter layer, concurrent quiet
operations coexist peacefully—each call owns exactly one filter entry and the
stack is unwound independently even if a process exits mid-command.

### Example Session

```elisp
(czq-comint-run "*quiet-demo*")
(let ((proc (get-buffer-process "*quiet-demo*")))
  (czq-comint--send-command-quietly proc "echo setup" 0.3)
  ;; Wait long enough for the teardown timer; adjust if you customise delays.
  (accept-process-output proc 0.5))
```

## Timer and Delay Controls

The quiet window is governed by a single customizable variable:

- `czq-comint-send-quiet-delay` — defaults to `0.05` seconds and becomes
  buffer-local as soon as it is set.

Override it programmatically for one buffer:

```elisp
(with-current-buffer "*quiet-demo*"
  (setq-local czq-comint-send-quiet-delay 0.4))
```

Or call `M-x czq-comint-send-edit-quiet-delays`, which prompts for the new
value and accepts `RET` to keep the current setting.  The helper ensures the
value is a non-negative float and immediately reports the new configuration.

## Building New Helpers

Quiet sends are just one application of the filter stack.  A custom helper can
capture command output without displaying it, forward it to another buffer, or
pipe it through a formatter before comint renders it.  The skeleton below shows
how to capture output synchronously:

```elisp
(defun czq-comint-send-capture (process command)
  (let ((result ""))
    (with-current-buffer (process-buffer process)
      (let ((entry (czq-comint-send--register-filter
                    (lambda (chunk)
                      (setq result (concat result chunk))
                      ""))))
        (unwind-protect
            (progn
              (comint-send-string process (concat command "\n"))
              (accept-process-output process 0.1))
          (czq-comint-send--remove-filter entry))))
    result))
```

This mirrors the quiet helper’s pattern: register, send, clean up.  Any helper
that reuses the stack should continue to rely on the restore tag technique (or
an equivalent callback) so filter state cannot leak if the process exits
unexpectedly.

## Where It’s Used Today

- `czq-comint-run` employs quiet sends to `cd` into the originating directory
  without flashing intermediate prompts.
- `czq-comint-completion-refresh-from-process` leverages the helper twice: once
  to fetch the live `$PATH`, and again to install a restore tag that re-enables
  output as soon as the shell reports completion.

Any future modules can adopt the same approach without coordinating with
`czq-comint.el`; the render-filter layer is purpose-built for that reuse.
