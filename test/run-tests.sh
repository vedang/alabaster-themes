#!/bin/bash

# Test runner for Alabaster theme package

echo "Running Alabaster Theme Tests..."
echo "==============================="

# Change to the correct directory
cd "$(dirname "$0")/.."

# Ensure buttercup is available
BUTTERCUP_PATH=$(emacs --batch --eval "(progn (package-initialize) (princ (file-name-directory (locate-library \"buttercup\"))))" 2>/dev/null)
if [ -z "$BUTTERCUP_PATH" ]; then
    echo "Error: Cannot locate buttercup path."
    exit 1
fi
emacs --batch -L . -L "$BUTTERCUP_PATH" -l test/alabaster-test.el -f buttercup-run-discover

echo ""
echo "==============================="
echo "Tests completed!"
