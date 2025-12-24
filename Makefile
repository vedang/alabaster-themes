HOME := $(shell echo $$HOME)
HERE := $(shell echo $$PWD)

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Eeu

.DEFAULT_GOAL := help

.PHONY: help
help:    ## A brief listing of all available commands
	@awk '/^[a-zA-Z0-9_-]+:.*##/ { \
		printf "%-25s # %s\n", \
		substr($$1, 1, length($$1)-1), \
		substr($$0, index($$0,"##")+3) \
	}' $(MAKEFILE_LIST)

.PHONY: test
test:  ## Run tests for the package
	@./test/run-tests.sh

.PHONY: compile
compile:  ## Byte-compile all the elisp files
	@echo "Compiling Emacs Lisp files..."
	@emacs --batch -L . --eval "(setq byte-compile-error-on-warn nil)" -f batch-byte-compile *.el
	@echo "Compilation complete."

.PHONY: clean
clean:  ## Drop all the elc files
	@echo "Cleaning compiled files..."
	@if [ -n "$$(find . -name '*.elc' -type f)" ]; then \
		find . -name '*.elc' -type f -print | xargs rm -f; \
		echo "Removed all .elc files."; \
	else \
		echo "No .elc files found to clean."; \
	fi

.PHONY: check
check: check-package checkdoc  ## Run linting checks for the package
	@echo "All linting checks completed."

.PHONY: check-package
check-package:
	@echo "Running package-lint checks..."
	@emacs --batch -l package -f package-initialize \
		--eval "(unless (package-installed-p 'package-lint) \
			(package-refresh-contents) \
			(package-install 'package-lint))" \
		-l package-lint \
		-f package-lint-batch-and-exit \
		*.el

.PHONY: checkdoc
checkdoc:
	@echo "Running checkdoc checks..."
	@emacs --batch -l checkdoc \
		--eval "(progn \
			(let ((checkdoc-autofix-flag t) \
			      (checkdoc-create-error-function \
				(lambda (msg start end &optional unfixable) \
				  (message \"%s:%d:%d: %s\" \
					   (file-name-nondirectory buffer-file-name) \
					   (line-number-at-pos start) \
					   (current-column) msg)))) \
			(checkdoc-file \"alabaster-themes.el\") \
			(checkdoc-file \"alabaster-themes-light-theme.el\") \
			(checkdoc-file \"alabaster-themes-dark-theme.el\") \
			(checkdoc-file \"alabaster-themes-light-bg-theme.el\") \
			(checkdoc-file \"alabaster-themes-light-mono-theme.el\") \
			(checkdoc-file \"alabaster-themes-dark-mono-theme.el\") \
			(message \"Checkdoc completed.\")))"
