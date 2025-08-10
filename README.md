# Forj.el - AI Co-pilot for Emacs Lisp Development

**⚠️ Alpha Release - In Active Development**

Transform your Emacs into an intelligent coding assistant specialized for Emacs Lisp development. Forj.el provides AI-powered code analysis, refactoring, and validation with deep integration into your Emacs workflow.

## Features

- **Smart Code Analysis**: AI-powered syntax validation with precise error reporting
- **Intelligent Refactoring**: Context-aware code improvements and modernization
- **Real-time Validation**: Custom parentheses checker for AI-generated code
- **Seamless Integration**: Native Emacs commands with keyboard-driven workflow
- **Secure API Management**: Safe credential storage using Emacs auth-source

## Quick Start

```elisp
;; Clone and install from Git repository
git clone https://github.com/username/forj.el.git

;; Add to your Emacs configuration
(add-to-list 'load-path "/path/to/forj.el")
(require 'forj)

;; Configure your API key (one time setup)
M-x customize-group RET forj RET

;; Start using Forj
M-x forj-prompt RET "Refactor this function for better readability"
```

## Installation

**Currently in Alpha - Install from Git Repository**

### Step 1: Clone Repository
```bash
git clone https://github.com/username/forj.el.git
cd forj.el
```

### Step 2: Install in Emacs
```elisp
;; Add to your Emacs configuration (~/.emacs.d/init.el)
(add-to-list 'load-path "/path/to/forj.el")
(require 'forj)

;; Optional: Set up key bindings
(global-set-key (kbd "C-c f p") 'forj-prompt)
(global-set-key (kbd "C-c f c") 'forj-check-syntax)
```

### Step 3: Configure API Key
```elisp
;; Configure through Emacs customize interface
M-x customize-group RET forj RET

;; Or set directly in your config
(setq forj-api-provider 'gemini)
;; Add your API key to auth-source or set directly (not recommended)
```

### Future Installation (Coming Soon)
```elisp
;; Will be available on MELPA after beta release
(use-package forj
  :ensure t
  :bind (("C-c f p" . forj-prompt)
         ("C-c f c" . forj-check-syntax)))
```

## Usage Examples

### Code Refactoring
```elisp
;; Select a function and ask for improvements
M-x forj-prompt RET "Make this function more idiomatic and add error handling"

;; Before:
(defun get-file-content (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; After: (AI-generated improvements)
(defun forj-get-file-content (file)
  "Read and return the contents of FILE.
Signals an error if FILE cannot be read."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    (file-error
     (user-error "Cannot read file %s: %s" file (error-message-string err)))))
```

### Syntax Validation
```elisp
;; Check current buffer for syntax errors
M-x forj-check-syntax

;; Returns structured error information:
;; {:status 'invalid
;;  :errors [{:line 15 :column 23 :type 'unmatched-paren 
;;           :message "Unmatched closing brace, expected closing parenthesis"
;;           :suggestion "Replace '}' with ')' at line 15, column 23"}]}
```

### Interactive Code Explanation
```elisp
;; Highlight complex code and ask for explanation
M-x forj-explain-region

;; Example: Select this macro
(defmacro with-forj-context (buffer &rest body)
  "Execute BODY with BUFFER as current buffer, preserving point and mark."
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (save-excursion
       (save-mark-and-excursion
         ,@body))))

;; AI explains: "This macro creates a safe context for buffer operations..."
```

### Project-wide Analysis
```elisp
;; Analyze entire project for improvements
M-x forj-analyze-project RET "Find functions that could benefit from error handling"

;; Get AI recommendations for:
;; - Functions missing error handling
;; - Deprecated function usage
;; - Performance improvements
;; - Documentation gaps
```

### File Reading and Project Context
```elisp
;; Read a single file safely
(forj-read-file "forj.el")
;; => "(defun forj-prompt (prompt)..."

;; List all Emacs Lisp files in project
(forj-list-files "." "\\.el$")
;; => ("forj.el" "forj-api.el" "test/forj-test.el")

;; Read multiple files for AI context
(forj-read-project-files '("el" "md") 5)
;; => (("forj.el" . ";;; forj.el...") ("README.md" . "# Forj.el..."))

;; AI prompt with project context
M-x forj-prompt RET "Based on my project files, suggest improvements to error handling"
```

### Safe File Operations
```elisp
;; Read large files with size limits
(forj-read-file "huge-log.txt" 1000)  ; Only read first 1KB
;; => "Log entry 1...\n[Content truncated at 1000 bytes]"

;; Handle file reading errors gracefully
(condition-case err
    (forj-read-file "non-existent.txt")
  (file-error (message "Could not read file: %s" err)))
```

## Configuration

### Basic Setup (Alpha Version)
```elisp
;; Basic configuration after manual installation
(require 'forj)

;; Customize settings
(setq forj-api-provider 'gemini)          ; API provider
(setq forj-response-timeout 30)           ; Response timeout in seconds
(setq forj-auto-validate t)               ; Auto-validate generated code
(setq forj-conversation-buffer "*forj*")   ; Conversation buffer name

;; Key bindings
(global-set-key (kbd "C-c f p") 'forj-prompt)          ; Main prompt command
(global-set-key (kbd "C-c f c") 'forj-check-syntax)     ; Syntax checker
(global-set-key (kbd "C-c f e") 'forj-explain-region)   ; Explain selected code
(global-set-key (kbd "C-c f r") 'forj-refactor-function) ; Refactor current function
(global-set-key (kbd "C-c f h") 'forj-show-conversation) ; Show conversation history

;; Future use-package configuration (when on MELPA)
;; (use-package forj
;;   :ensure t
;;   :custom
;;   (forj-api-provider 'gemini)
;;   (forj-response-timeout 30)
;;   :bind
;;   (("C-c f p" . forj-prompt)
;;    ("C-c f c" . forj-check-syntax)))
```

### Advanced Configuration
```elisp
(setq forj-custom-prompts
      '((refactor . "Refactor this code following modern Emacs Lisp best practices")
        (document . "Add comprehensive docstrings and comments to this code")
        (optimize . "Optimize this code for better performance")
        (test . "Generate ERT tests for this function")))

;; Custom validation rules
(setq forj-validation-rules
      '((max-line-length . 80)
        (require-docstrings . t)
        (prefer-cl-lib . t)
        (check-autoloads . t)))
```

## API Reference

### Core Functions

#### `forj-prompt (prompt)`
Main interaction function for AI assistance.

**Parameters:**
- `prompt` (string): Your request to the AI assistant

**Returns:** 
- Success: AI response as string
- Error: Error message with suggestions

**Example:**
```elisp
(forj-prompt "Add error handling to the current function")
```

#### `forj-check-syntax (&optional buffer)`
Validate Emacs Lisp syntax with detailed error reporting.

**Parameters:**
- `buffer` (buffer, optional): Buffer to check (defaults to current buffer)

**Returns:**
```elisp
{:status 'valid|'invalid
 :balanced t|nil
 :errors [{:line INTEGER :column INTEGER :type SYMBOL 
          :message STRING :suggestion STRING}]
 :message STRING}
```

**Example:**
```elisp
(forj-check-syntax (current-buffer))
;; => {:status 'valid :balanced t :message "Code is syntactically valid"}
```

#### `forj-refactor-function ()`
Refactor the function at point with AI assistance.

**Interactive:** Yes  
**Returns:** Refactored function code

#### `forj-explain-region (start end)`
Explain the selected code region.

**Parameters:**
- `start` (integer): Start of region
- `end` (integer): End of region

**Returns:** AI explanation as string

### Utility Functions

#### `forj-conversation-history ()`
Return the current conversation history.

**Returns:** List of conversation turns

#### `forj-clear-conversation ()`
Clear the current conversation history.

**Interactive:** Yes

#### `forj-show-conversation ()`
Display conversation history in dedicated buffer.

**Interactive:** Yes

#### `forj-read-file (file-path &optional max-size)`
Read contents of a file safely with optional size limits.

**Parameters:**
- `file-path` (string): Path to file to read
- `max-size` (integer, optional): Maximum bytes to read (default 50000)

**Returns:** String containing file contents or truncated content

**Example:**
```elisp
(forj-read-file "README.md")
;; => "# Forj.el - AI Co-pilot for Emacs..."

(forj-read-file "large-file.txt" 1000)  ; Limit to 1KB
;; => "Content...[truncated]"
```

#### `forj-list-files (&optional directory pattern)`
List files in directory matching optional pattern.

**Parameters:**
- `directory` (string, optional): Directory to scan (default current directory)
- `pattern` (string, optional): File pattern to match (default all text files)

**Returns:** List of file paths

**Example:**
```elisp
(forj-list-files)
;; => ("README.md" "forj.el" "test/forj-test.el")

(forj-list-files "." "\\.el$")
;; => ("forj.el" "forj-api.el")
```

#### `forj-read-project-files (&optional file-types max-files)`
Read multiple project files for AI context.

**Parameters:**
- `file-types` (list, optional): List of file extensions (default '("el" "md" "txt"))
- `max-files` (integer, optional): Maximum files to read (default 10)

**Returns:** Alist of (file-path . content) pairs

**Example:**
```elisp
(forj-read-project-files '("el") 5)
;; => (("forj.el" . "(defun forj-prompt...") 
;;     ("test/forj-test.el" . "(require 'ert)..."))
```

#### `forj-file-metadata (file-path)`
Get comprehensive metadata for a file including size, modification time, and type.

**Parameters:**
- `file-path` (string): Path to file for metadata extraction

**Returns:** Plist with :path, :size, :modified-time, :type, :readable, :writable

**Example:**
```elisp
(forj-file-metadata "forj.el")
;; => (:path "/path/to/forj.el" :size 15234 :modified-time (25086 12345) 
;;     :type elisp :readable t :writable t)
```

#### `forj-list-files-with-metadata (&optional directory pattern)`
List files with comprehensive metadata including size, modification time, and file type.

**Parameters:**
- `directory` (string, optional): Directory to scan (default current directory)
- `pattern` (string, optional): File pattern to match (default all files)

**Returns:** List of metadata plists for each file

**Example:**
```elisp
(forj-list-files-with-metadata "." "\\.el$")
;; => ((:path "forj.el" :size 15234 :type elisp :readable t :writable t)
;;     (:path "test/forj-test.el" :size 8956 :type elisp :readable t :writable t))
```

#### `forj-browse-files (&optional directory)`
Interactive file browser for selecting project files with rich metadata display.

**Interactive:** Yes  
**Parameters:**
- `directory` (string, optional): Directory to browse (default current directory)

**Returns:** Selected file path or nil if cancelled

**Example:**
```elisp
;; Interactive usage
M-x forj-browse-files

;; Programmatic usage
(forj-browse-files "/path/to/project")
;; Shows: "forj.el [elisp, 15234 bytes, 2025-08-10 14:30]"
;;        "README.md [markdown, 8456 bytes, 2025-08-10 12:15]"
```

#### `forj-scan-directory-recursive (&optional directory max-depth max-files)`
Recursively scan directory for project files with depth and file count limits.

**Parameters:**
- `directory` (string, optional): Directory to scan (default current directory)
- `max-depth` (integer, optional): Maximum recursion depth (default 5)
- `max-files` (integer, optional): Maximum files to find (default 100)

**Returns:** List of file metadata plists

**Example:**
```elisp
(forj-scan-directory-recursive "." 3 50)
;; => ((:path "./forj.el" :size 15234 :type elisp ...)
;;     (:path "./src/utils.el" :size 3456 :type elisp ...)
;;     (:path "./docs/README.md" :size 8456 :type markdown ...))
```

#### `forj-browse-and-read-file (&optional directory)`
Interactive file browser that reads selected file and logs operation to conversation.

**Interactive:** Yes  
**Parameters:**
- `directory` (string, optional): Directory to browse (default current directory)

**Returns:** File content or nil if cancelled

**Example:**
```elisp
M-x forj-browse-and-read-file
;; Interactive selection, then logs to conversation:
;; "Read file: forj.el (15234 bytes)"
```

#### `forj-scan-and-display-project (&optional directory)`
Comprehensive project scanner that analyzes directory structure and logs results to conversation.

**Interactive:** Yes  
**Parameters:**
- `directory` (string, optional): Directory to analyze (default current directory)

**Returns:** List of file metadata plists

**Example:**
```elisp
M-x forj-scan-and-display-project
;; Logs to conversation:
;; "Starting project scan in: /path/to/project"
;; "Project scan complete: 25 files, 156.7 KB total, 0.045 seconds"
;; "File types: elisp: 8, markdown: 3, text: 2, json: 1, yaml: 1"
```

#### `forj-edit-file-region (file-path start-line end-line new-content &optional no-backup)`
Edit specific region in a file with backup and validation.

**Parameters:**
- `file-path` (string): Path to file to edit
- `start-line` (integer): Starting line number (1-based)
- `end-line` (integer): Ending line number (1-based)
- `new-content` (string): New content to replace the region
- `no-backup` (boolean, optional): Skip backup creation if non-nil

**Returns:** Plist with :success, :backup-path, :error-message, :lines-changed

**Example:**
```elisp
(forj-edit-file-region "test.el" 5 7 "(defun new-func () \"Replacement\")")
;; => (:success t :backup-path "test.el.bak.20250810-143022" :lines-changed 1)
```

#### `forj-backup-file (file-path &optional backup-name)`
Create timestamped backup of a file.

**Parameters:**
- `file-path` (string): Path to file to backup
- `backup-name` (string, optional): Custom backup filename

**Returns:** Plist with :success, :backup-path, :error-message

**Example:**
```elisp
(forj-backup-file "important.el")
;; => (:success t :backup-path "important.el.bak.20250810-143022")
```

#### `forj-restore-backup (backup-path original-path)`
Restore file from backup.

**Parameters:**
- `backup-path` (string): Path to backup file
- `original-path` (string): Path where to restore the file

**Returns:** Plist with :success, :error-message, :restored-size

**Example:**
```elisp
(forj-restore-backup "important.el.bak.20250810-143022" "important.el")
;; => (:success t :restored-size 15234)
```

#### `forj-write-file-with-git-awareness (file-path content &optional no-backup)`
Write file with Git integration, status checking, and conversation logging.

**Parameters:**
- `file-path` (string): Path to file to write
- `content` (string): Content to write
- `no-backup` (boolean, optional): Skip backup creation if non-nil

**Returns:** Plist with :success, :backup-path, :error-message, :modified-time

**Features:**
- Warns about uncommitted Git changes
- Optionally auto-stages changes
- Logs operations to conversation buffer
- Creates timestamped backups

**Example:**
```elisp
(forj-write-file-with-git-awareness "src/main.el" new-code)
;; Conversation log: "Wrote file: main.el (1234 bytes, 0.045s), backup created, auto-staged"
```

#### `forj-in-git-repo-p (&optional directory)`
Check if directory is in a Git repository.

**Parameters:**
- `directory` (string, optional): Directory to check (default current directory)

**Returns:** Git root directory path if in repo, nil otherwise

**Example:**
```elisp
(forj-in-git-repo-p)
;; => "/Users/name/project" or nil
```

#### `forj-git-file-status (file-path)`
Get Git status information for a file.

**Parameters:**
- `file-path` (string): Path to file to check

**Returns:** Plist with :status, :staged, :modified, :untracked

**Example:**
```elisp
(forj-git-file-status "src/main.el")
;; => (:status modified :staged nil :modified t :untracked nil)
```

### File Locking and Concurrency

#### `forj-lock-file (file-path &optional timeout)`
Lock file for exclusive access.

**Parameters:**
- `file-path` (string): Path to file to lock
- `timeout` (integer, optional): Timeout in seconds (default 30)

**Returns:** Plist with :success, :lock-id, :error-message

#### `forj-with-file-lock (file-path &rest body)`
Macro to execute code with file locked.

**Usage:**
```elisp
(forj-with-file-lock "important.el"
  (let ((content (forj-read-file "important.el")))
    (forj-write-file "important.el" (concat content "\n;; Added line"))))
```

### Customization Variables

#### `forj-api-provider`
API provider for AI requests.
- **Type:** symbol
- **Default:** `'gemini`
- **Options:** `'gemini`, `'openai` (future), `'anthropic` (future)

#### `forj-response-timeout`
Timeout for API responses in seconds.
- **Type:** integer  
- **Default:** 30

#### `forj-auto-validate`
Automatically validate AI-generated code.
- **Type:** boolean
- **Default:** t

#### `forj-max-file-size`
Maximum file size to read in bytes.
- **Type:** integer
- **Default:** 50000

#### `forj-supported-extensions`
List of file extensions to consider for project context.
- **Type:** list of strings
- **Default:** '("el" "md" "txt" "org" "lisp")

#### `forj-excluded-patterns`
Patterns for files/directories to exclude from scanning.
- **Type:** list of strings  
- **Default:** '(".git" ".DS_Store" "*.elc" "*.log" "*~")

#### `forj-confirm-destructive-operations`
Whether to prompt for confirmation before destructive operations.
- **Type:** boolean
- **Default:** t

#### `forj-enable-git-integration`
Whether to enable Git repository integration features.
- **Type:** boolean
- **Default:** t

#### `forj-warn-uncommitted-changes`
Whether to warn before modifying files with uncommitted Git changes.
- **Type:** boolean
- **Default:** t

#### `forj-auto-stage-changes`
Whether to automatically stage changes after successful edits in Git repo.
- **Type:** boolean
- **Default:** nil

## Troubleshooting

### Common Issues

**API Key Not Found**
```
Error: No API key found for provider 'gemini'
```
**Solution:** Configure your API key using `M-x customize-group RET forj RET` or add to your auth-source.

**Slow Response Times**
```
Warning: API response took 45 seconds
```
**Solution:** Check your internet connection or increase `forj-response-timeout`.

**Syntax Validation Errors**
```
Error: Unbalanced parentheses detected
```
**Solution:** Use `M-x forj-check-syntax` for detailed error location and suggestions.

### Debug Mode

Enable debug mode for detailed logging:
```elisp
(setq forj-debug t)
(setq forj-log-level 'debug)
```

Check the `*Messages*` buffer for detailed logs.

### Performance Issues

If Forj.el is slow:
1. Reduce `forj-response-timeout` for faster failures
2. Disable `forj-auto-validate` for immediate responses
3. Use `forj-clear-conversation` to reduce context size

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup
```bash
# Clone the repository
git clone https://github.com/username/forj.el.git
cd forj.el

# Run tests (when available)
emacs -batch -l ert -l test/forj-test.el -f ert-run-tests-batch-and-exit

# Load in development Emacs
emacs -Q -l forj.el

# Or load specific files for testing
emacs -Q -l forj-core.el -l forj-paren-checker.el
```

### Development Workflow
1. Write README examples first (README-driven development)
2. Convert examples to tests (Test-driven development)  
3. Implement features to pass tests
4. Update documentation

## Roadmap

### Alpha Phase (Current)
- [ ] Core parentheses validation for AI code (forj-paren-checker)
- [ ] Basic AI integration with secure API key management
- [ ] Core buffer operations (read, replace, prompt)
- [ ] Basic test framework setup

### Beta Phase (Coming Soon)
- [ ] Multi-file project context understanding
- [ ] Advanced refactoring suggestions
- [ ] MELPA package submission
- [ ] Community feedback integration

### v1.0 Release
- [ ] Integration with popular Emacs packages
- [ ] Support for multiple AI providers
- [ ] Comprehensive documentation and examples
- [ ] Performance optimization

## License

GPL-3.0 - see [LICENSE](LICENSE) file for details.

## Support

**Alpha Release - Expect Breaking Changes**

- **Issues:** [GitHub Issues](https://github.com/username/forj.el/issues)
- **Discussions:** [GitHub Discussions](https://github.com/username/forj.el/discussions)
- **Reddit:** [r/emacs](https://reddit.com/r/emacs) - tag with `forj.el`
- **Development:** See [docs/roadmap.md](docs/roadmap.md) for current progress

### Alpha Limitations
- Features may be incomplete or unstable
- API may change without notice
- Not recommended for production use
- Documentation may be ahead of implementation

---

**Forge ahead with AI-powered Emacs Lisp development!** 🔥