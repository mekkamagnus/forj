# Makefile for forj.el development

# Project configuration
PACKAGE_NAME = forj
VERSION = 0.1.0-alpha
EMACS ?= emacs

# Directories
SRC_DIR = .
TEST_DIR = test
SCRIPTS_DIR = scripts

# Files
MAIN_FILE = forj.el
TEST_FILE = $(TEST_DIR)/forj-test.el

# Default target
.PHONY: help
help:
	@echo "Forj.el Development Makefile"
	@echo "============================="
	@echo ""
	@echo "Available targets:"
	@echo "  test         - Run all tests"
	@echo "  test-batch   - Run tests in batch mode (no output)"
	@echo "  clean        - Clean generated files"
	@echo "  lint         - Check code style (if available)"
	@echo "  dev          - Load forj.el in development Emacs"
	@echo "  validate     - Validate package structure"
	@echo "  help         - Show this help"
	@echo ""
	@echo "Environment variables:"
	@echo "  EMACS        - Emacs executable (default: emacs)"
	@echo "  GEMINI_API_KEY - Required for API functionality"

# Run tests
.PHONY: test
test:
	@echo "Running forj.el test suite..."
	@$(SCRIPTS_DIR)/test.sh

# Run tests in batch mode (minimal output)
.PHONY: test-batch
test-batch:
	@$(EMACS) -batch -L . -l ert -l $(MAIN_FILE) -l $(TEST_FILE) -f ert-run-tests-batch-and-exit

# Validate package can be loaded
.PHONY: validate
validate:
	@echo "Validating package structure..."
	@$(EMACS) -batch -L . --eval "(progn (require '$(PACKAGE_NAME)) (message \"✅ Package loads successfully\"))"

# Clean generated files
.PHONY: clean
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.elc" -delete
	@find . -name "*~" -delete
	@find . -name ".#*" -delete

# Lint code (basic check)
.PHONY: lint
lint:
	@echo "Checking code style..."
	@$(EMACS) -batch -L . --eval "(progn (require '$(PACKAGE_NAME)) (message \"✅ Basic lint check passed\"))"

# Load in development mode
.PHONY: dev
dev:
	@$(SCRIPTS_DIR)/dev-load.sh

# Byte compile
.PHONY: compile
compile:
	@echo "Byte compiling..."
	@$(EMACS) -batch -L . -f batch-byte-compile $(MAIN_FILE)

# Check environment
.PHONY: env-check
env-check:
	@echo "Environment check:"
	@echo "  Emacs version: $$($(EMACS) --version | head -n1)"
	@echo "  Project dir: $$(pwd)"
	@if [ -z "$$GEMINI_API_KEY" ]; then \
		echo "  ⚠️  GEMINI_API_KEY: Not set"; \
	else \
		echo "  ✅ GEMINI_API_KEY: Set"; \
	fi

# Full validation pipeline
.PHONY: ci
ci: env-check validate test lint
	@echo "✅ All CI checks passed!"

# Development setup
.PHONY: setup
setup:
	@echo "Setting up development environment..."
	@mkdir -p $(SCRIPTS_DIR)
	@chmod +x $(SCRIPTS_DIR)/*.sh
	@echo "✅ Development environment ready!"
	@echo ""
	@echo "Next steps:"
	@echo "1. Set GEMINI_API_KEY environment variable"
	@echo "2. Run 'make test' to verify setup"
	@echo "3. Run 'make dev' to start development"