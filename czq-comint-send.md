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
string that would otherwise reach the comint buffer untouched.  Before the
render stack runs, CZQ comint optionally applies a normalization filter (managed
by the REPL tracker) so mode-specific cleanups—such as stripping `^M` carriage
returns—happen independently of quiet/redirect helpers.  Render filters live in
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

Each parser token is rendered and passed through the stack as soon as it is
produced, so helpers can react incrementally instead of waiting for an entire
chunk of process output.

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
;; 1) Start (or reuse) a CZQ comint buffer.
(czq-comint-run "*czq-demo*")

;; 2) Record the process handle for convenience.
(setq proc (get-buffer-process "*czq-demo*"))

;; 3) Quietly run a setup command, waiting for the restore tag to arrive.
(with-current-buffer (process-buffer proc)
  (setq-local czq-comint-send-quiet-delay 0.2)
  (czq-comint--send-command-quietly proc "echo setup" 0.3)
  (accept-process-output proc 0.5))
```

## Timer and Delay Controls

The quiet window is governed by a single customizable variable:

- `czq-comint-send-quiet-delay` — defaults to `0.05` seconds and becomes
  buffer-local as soon as it is set.

Override it programmatically for one buffer:

```elisp
;; Assuming *czq-demo* already exists from the previous example:
(with-current-buffer "*czq-demo*"
  (setq-local czq-comint-send-quiet-delay 0.4))
```

Or call `M-x czq-comint-send-edit-quiet-delays`, which prompts for the new
value and accepts `RET` to keep the current setting.  The helper ensures the
value is a non-negative float and immediately reports the new configuration.

## Redirect Output to a Buffer

Use `czq-comint-send-to-buffer` when you want a command’s output to land in a
separate buffer instead of the comint stream:

```elisp
;; 1) Make sure *czq-demo* is running and PROC is set (see the quiet example).

;; 2) Prepare the destination buffer.
(setq log-buffer (get-buffer-create "*czq-log*"))
(with-current-buffer log-buffer
  (erase-buffer))

;; 3) Pipe the command output into the log buffer.
(with-current-buffer (process-buffer proc)
  (czq-comint-send-to-buffer proc "printf capture" log-buffer t))

;; 4) Give the shell a moment to emit the restore tag, then inspect the buffer.
(accept-process-output proc 0.1)
(with-current-buffer log-buffer
  (buffer-string))

;; Nothing new should appear in *czq-demo*; verify it remains unchanged.
(with-current-buffer (process-buffer proc)
  (buffer-substring-no-properties (max 1 (- (point-max) 40)) (point-max)))
```
- If text still shows up in the comint buffer, double-check that it is in
  `czq-comint-mode` (start it with `czq-comint-run`).  The redirection helpers
  rely on the CZQ pre-output filter.

- The third argument names the destination buffer.  Pass a non-nil fourth
  argument to clear the buffer before the command runs.
- Each chunk is appended verbatim, and the redirect filter disappears as soon
  as the restore tag executes, so the comint buffer stays unchanged.

## Insert Output at Point

`czq-comint-send-to-point` splices command output directly into a buffer at a
marker (or the current cursor position):

```elisp
;; 1) Create (or reuse) a destination buffer and choose an insertion point.
(setq dest-buffer (get-buffer-create "*czq-notes*"))
(with-current-buffer dest-buffer
  (erase-buffer)
  (insert "before\nafter")
  (goto-char (point-min))
  (forward-line 1)
  (setq insertion-marker (point-marker))
  (set-marker-insertion-type insertion-marker t))

;; 2) Insert the command’s output at that marker.
(with-current-buffer (process-buffer proc)
  (czq-comint-send-to-point proc "printf insert" insertion-marker))
(accept-process-output proc 0.1)

;; 3) Review the updated buffer.
(with-current-buffer dest-buffer
  (buffer-string))
```

- When `MARKER` is nil the helper captures `(point)` from the current buffer and
  creates a dedicated marker that advances as text is inserted.  Provide your
  own marker when you want multiple commands to share the same insertion point.
- Because insertion happens inside the buffer that owns the marker, standard
  read-only safeguards still apply unless you temporarily bind
  `inhibit-read-only`.

## Tracking the REPL

`czq-comint-track-repl.el` watches the commands you send to the process and
tries to infer which REPL is currently active.  New CZQ comint buffers assume
`bash` until a more specific pattern matches.  The result lives in the
buffer-local variable `czq-comint-track-repl-name`, while
`czq-comint-track-repl-last-command` records the most recent command that was
inspected.  Detection relies on `czq-comint-track-repl-command-alist`, a
customizable list of regular expressions mapped to REPL symbols (for example,
`"^python"` → `python`, `"^julia"` → `julia`,
`"^wolframscript"`/`"^math"` → `mathematica`).  The default entries cover the
Python, Julia, and Mathematica/WolframScript toolchains along with a handful of
common shells and REPLs.

When `czq-comint-track-repl-name` is `mathematica`, CZQ comint automatically
normalizes `\r\n` line endings to Unix `\n` so WolframScript output no longer
sprinkles `^M` characters through the buffer.

You can review or override the detected REPL via:

```elisp
M-x czq-comint-track-repl-edit
```

The command displays the current value, allows you to provide a new symbol, and
accepts an empty response to clear the detection.  Programmatic helpers such as
`czq-comint--send-command-quietly`, `czq-comint-send-to-buffer`, and
`czq-comint-send-to-point` automatically register the commands they emit so the
tracker stays in sync even when you are not typing directly into the comint
buffer.

## Normalization Filters

Before render filters run, CZQ comint can pass each rendered chunk through a
single normalization filter managed by the REPL tracker (or user code).  This
stage is ideal for cleaning up control characters, simulating carriage returns,
or applying any REPL-specific fix-ups that should happen regardless of quiet
sends/redirects.

### Automatic Mathematica Cleanup

WolframScript/Mathematica emits prompts that repaint the current line using
carriage returns.  CZQ comint detects those sessions and installs the
`czq-comint-normalize-strip-cr` helper, which converts every `\r\n` to `\n`,
drops stray `\r`, and trims the padded whitespace that trails `In[n]:=` while
leaving a single space after the prompt so the cursor lands exactly where the
native REPL would place it.

With `czq-comint-debug` enabled you will see log entries such as:

```
[czq-comint] normalize-filter install active
[czq-comint] normalize-filter mathematica len=185→162
```

indicating that the normalizer reduced the chunk’s length after stripping the
extra carriage returns.  Clearing or changing the REPL automatically removes the
filter (`normalize-filter cleared`).

### Manual Installation

You can supply your own normalizer when experimenting with other REPLs:

```elisp
(with-current-buffer "*czq-demo*"
  ;; Example: drop ANSI color sequences before render filters see them.
  (czq-comint-set-normalize-filter
   (lambda (chunk)
     (replace-regexp-in-string "\\x1b\\[[0-9;]*m" "" chunk))))
```

Later, call `czq-comint-clear-normalize-filter` to remove the hook.  Because the
normalization stage feeds directly into the render-filter stack, quiet sends and
redirect helpers automatically see the cleaned-up text without any additional
changes.

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
