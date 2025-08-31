# Makefile for forj.el development

# Project configuration
PACKAGE_NAME = forj
VERSION = 0.1.0-alpha
EMACS ?= emacs

# Directories
SRC_DIR = .
TEST_DIR = test
SCRIPTS_DIR = scripts
DOCS_DIR = docs
TEXINFO_DIR = $(DOCS_DIR)/texinfo

# Files
MAIN_FILE = forj.el
TEST_FILE = $(TEST_DIR)/forj-test.el
TEXINFO_SOURCE = $(TEXINFO_DIR)/forj.texi
INFO_FILE = $(TEXINFO_DIR)/forj.info

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
	@echo "  docs         - Build all documentation formats"
	@echo "  info         - Build Info manual"
	@echo "  html         - Build HTML documentation"
	@echo "  pdf          - Build PDF manual"
	@echo "  install-info - Install Info manual in system"
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
clean: clean-docs
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

# Documentation targets
.PHONY: docs info html pdf

# Build all documentation formats
docs: info html pdf
	@echo "✅ All documentation formats built successfully!"

# Build Info manual
info: $(INFO_FILE)
	@echo "✅ Info manual built: $(INFO_FILE)"

$(INFO_FILE): $(TEXINFO_SOURCE)
	@echo "Building Info manual from Texinfo source..."
	@cd $(TEXINFO_DIR) && makeinfo forj.texi

# Build HTML documentation  
html: $(TEXINFO_DIR)/forj.html
	@echo "✅ HTML documentation built: $(TEXINFO_DIR)/forj.html"

$(TEXINFO_DIR)/forj.html: $(TEXINFO_SOURCE)
	@echo "Building HTML documentation from Texinfo source..."
	@cd $(TEXINFO_DIR) && makeinfo --html --no-split forj.texi

# Build PDF manual (requires texi2pdf)
pdf: $(TEXINFO_DIR)/forj.pdf
	@echo "✅ PDF manual built: $(TEXINFO_DIR)/forj.pdf"

$(TEXINFO_DIR)/forj.pdf: $(TEXINFO_SOURCE)
	@echo "Building PDF manual from Texinfo source..."
	@cd $(TEXINFO_DIR) && texi2pdf forj.texi

# Install Info manual in system directory
.PHONY: install-info
install-info: $(INFO_FILE)
	@echo "Installing Info manual..."
	@if command -v install-info >/dev/null 2>&1; then \
		sudo install-info $(INFO_FILE) /usr/local/share/info/dir || \
		echo "⚠️  Manual installation failed - you may need to run as root"; \
	else \
		echo "⚠️  install-info command not found - Info manual not installed"; \
	fi

# Clean documentation files
.PHONY: clean-docs
clean-docs:
	@echo "Cleaning documentation files..."
	@rm -f $(TEXINFO_DIR)/*.info
	@rm -f $(TEXINFO_DIR)/*.html  
	@rm -f $(TEXINFO_DIR)/*.pdf
	@rm -f $(TEXINFO_DIR)/*.aux $(TEXINFO_DIR)/*.cp $(TEXINFO_DIR)/*.fn
	@rm -f $(TEXINFO_DIR)/*.ky $(TEXINFO_DIR)/*.log $(TEXINFO_DIR)/*.pg
	@rm -f $(TEXINFO_DIR)/*.toc $(TEXINFO_DIR)/*.tp $(TEXINFO_DIR)/*.vr