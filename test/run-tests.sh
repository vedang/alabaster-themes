#!/bin/bash

# Test runner for Alabaster theme package

echo "Running Alabaster Theme Tests..."
echo "==============================="

# Change to the correct directory
cd "$(dirname "$0")/.."

# Ensure buttercup is available
if ! emacs --batch --eval "(progn (require 'buttercup))" 2>/dev/null; then
    echo "Error: buttercup is not installed."
    echo "Install it with: M-x package-install RET buttercup RET"
    exit 1
fi

# Run the test suite
emacs --batch -l test/alabaster-test.el -f buttercup-run-discover

echo ""
echo "==============================="
echo "Tests completed!"