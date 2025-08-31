# Texinfo Integration for Forj.el

This document describes the complete Texinfo documentation system now integrated into Forj.el.

## 📁 Directory Structure

```
docs/
├── texinfo/                    # Texinfo documentation directory
│   ├── forj.texi              # Master document (main entry point)
│   ├── installation.texi      # Installation instructions
│   ├── tutorial.texi          # Getting started tutorial
│   ├── configuration.texi     # Configuration options
│   ├── api-reference.texi     # Function reference (auto-generated)
│   ├── troubleshooting.texi   # Common issues
│   ├── forj.info             # Generated Info manual
│   ├── forj.html             # Generated HTML documentation
│   └── forj.pdf              # Generated PDF manual (optional)
└── [existing markdown files]  # Maintained for GitHub
```

## 🔨 Build System

### Make Targets

```bash
# Build all documentation formats
make docs

# Build specific formats
make info          # Build Info manual
make html          # Build HTML documentation  
make pdf           # Build PDF manual (requires texi2pdf)

# Install Info manual system-wide
make install-info

# Clean generated files
make clean-docs
```

### Build Requirements

- **makeinfo**: For Info and HTML generation (part of Texinfo)
- **texi2pdf**: For PDF generation (optional)
- **install-info**: For system Info integration (optional)

## 📚 Auto-Generation Features

### API Reference Generation

The API reference is automatically generated from the Emacs Lisp source code:

```elisp
;; Generate API reference from current symbols
M-x forj-texinfo-write-api-reference

;; Update during build process
M-x forj-texinfo-update-api-reference

;; Validate all documented symbols exist
M-x forj-texinfo-validate-references

;; Preview individual symbol documentation
M-x forj-texinfo-preview-symbol RET forj-prompt
```

### Auto-Generation Features

- **Function Documentation**: Extracts docstrings and function signatures
- **Variable Documentation**: Includes customization variables and internal state
- **Interactive Commands**: Separates user-facing commands from internal functions
- **Categorization**: Organizes by functionality (Core, File Operations, Git Integration, etc.)
- **Cross-References**: Proper Texinfo linking and indexing

## 🎯 Integration with Emacs

### Info Manual Access

Once built and installed, users can access the manual via:

```
C-h i                           # Open Info browser
m Forj RET                      # Navigate to Forj manual
C-h i m Forj RET                # Direct access
```

### Standard Emacs Help Integration

The Texinfo system integrates with Emacs' standard help:

```elisp
C-h f forj-prompt RET           # Function help
C-h v forj-max-file-size RET    # Variable help
C-h i m Forj RET                # Full manual
```

## 📖 Documentation Formats

### 1. Info Manual (Primary)
- **Native Emacs integration**: `C-h i`
- **Hyperlinked navigation**: Cross-references and indexes
- **Search capabilities**: Built-in Info search
- **Offline access**: No internet required

### 2. HTML Documentation
- **Web deployment**: Can be hosted on project website
- **Search engines**: Indexable by Google/search engines
- **Mobile friendly**: Responsive design
- **Print stylesheets**: CSS optimized for printing

### 3. PDF Manual (Optional)
- **Professional documentation**: Publication-quality output
- **Offline reference**: Complete manual in portable format
- **Print-ready**: Proper page breaks and formatting
- **Table of contents**: Bookmarks and navigation

## 🔧 Configuration

### Customization Options

```elisp
;; Texinfo output directory
(setq forj-texinfo-output-dir "docs/texinfo")

;; API reference filename
(setq forj-texinfo-api-file "api-reference.texi")
```

### Integration with Package Development

Add to your development workflow:

```bash
# Before releasing
make docs                       # Build all documentation
make install-info              # Install for testing

# During development
make info && make html         # Quick build for review
```

## 🚀 Usage Examples

### For Users

```bash
# Install Forj.el
git clone https://github.com/username/forj.el.git
cd forj.el

# Build and install documentation
make info
make install-info

# Access in Emacs
emacs
C-h i m Forj RET
```

### For Developers

```bash
# Update API documentation after code changes
make clean-docs
emacs -batch -L . --eval "(progn (require 'forj-texinfo) (forj-texinfo-write-api-reference))"
make info

# Generate all formats for release
make docs
```

### For Package Maintainers

```elisp
;; Add to package build process
(when (file-exists-p "docs/texinfo/forj.texi")
  (shell-command "cd docs/texinfo && makeinfo forj.texi"))
```

## 🔄 Maintenance Workflow

### Updating Documentation

1. **Manual Content**: Edit `.texi` files directly
2. **API Reference**: Regenerate after code changes
3. **Build Process**: Use Make targets for consistency
4. **Testing**: Verify Info navigation and cross-references

### Best Practices

- **Keep Markdown**: Maintain existing `.md` files for GitHub
- **Auto-Generation**: Use `forj-texinfo-update-api-reference` regularly
- **Version Control**: Commit generated files for releases
- **Cross-References**: Use `@xref` and `@pxref` for internal links

## 🎨 Advanced Features

### Custom Symbol Documentation

```elisp
;; Preview how a symbol will be documented
(forj-texinfo-preview-symbol 'forj-prompt)

;; Generate documentation for specific symbols
(forj-texinfo-generate-api-reference 
 '((:name forj-prompt :type function :args (user-input) :interactive t)))
```

### Build System Integration

The Makefile includes dependencies and intelligent rebuilding:

```makefile
# Automatic dependency tracking
$(INFO_FILE): $(TEXINFO_SOURCE) api-reference.texi
    @cd $(TEXINFO_DIR) && makeinfo forj.texi

# Clean integration
clean: clean-docs
    # ... other cleanup
```

## ✅ Benefits

### For Users
- **Native Emacs Experience**: Standard `C-h i` access
- **Professional Documentation**: Multi-format output
- **Always Current**: Auto-generated from source code
- **Comprehensive**: Installation through troubleshooting

### For Developers  
- **Automated**: API docs generated from code
- **Standard Compliance**: Follows Emacs documentation conventions
- **Multi-Format**: Info, HTML, PDF from single source
- **Maintainable**: Modular structure with clear separation

### For the Project
- **Professional Image**: Standard Emacs package documentation
- **User Adoption**: Lowers barrier to entry with good docs
- **Community Standard**: Expected by Emacs package users
- **Distribution Ready**: Ready for MELPA and package archives

This Texinfo integration transforms Forj.el from a development project into a professional Emacs package with documentation that meets community standards and user expectations.