#!/bin/sh

# Emit a CZQ comint tag that evaluates an Emacs Lisp expression.
# Use this script to feed the tag into a czq-comint buffer for quick testing.

set -e

cat <<'EOF'
Plain output line before any tag.
<czq-comint handler="elisp">(message "czq-comint demo tag triggered")</czq-comint>

Plain output between tags to ensure text passthrough.
<czq-comint handler="elisp" results="true">(+ 40 2)</czq-comint>

Content that should vanish thanks to the omit handler:
<czq-comint handler="omit">This text is omitted.</czq-comint>

Unknown handlers should be echoed back literally:
<czq-comint handler="unknown">Fallback payload</czq-comint>
EOF
