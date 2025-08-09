# Forj.el Troubleshooting Guide

**Purpose**: Document problems encountered during Emacs Lisp implementation and their solutions.

**Note**: This document is updated throughout development to capture real implementation challenges and solutions for future reference.

## Development Environment Issues

### Emacs Package Loading

#### Problem: Package won't load with "Symbol's function definition is void"
**Symptoms**: 
```
Debugger entered--Lisp error: (void-function forj-paren-check)
```

**Common Causes**:
- Function defined but not yet loaded
- Circular dependency between functions
- Missing `require` or `autoload` declarations
- Function name typo or incorrect prefix

**Solution**:
```elisp
;; Ensure proper require statements
(require 'cl-lib)  ; For cl-lib functions

;; Use forward declarations for circular dependencies
(declare-function forj-helper-function "forj-helper")

;; Check function exists before calling
(when (fboundp 'forj-paren-check)
  (forj-paren-check code))
```

#### Problem: "Package lacks a file header" during package installation
**Symptoms**:
```
Warning: Package forj lacks a file header
```

**Solution**:
```elisp
;;; forj.el --- AI co-pilot for Emacs Lisp development -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: convenience, tools, ai
;; URL: https://github.com/username/forj.el

;;; Commentary:
;; Brief description of the package

;;; Code:
(require 'cl-lib)
;; Your code here
(provide 'forj)
;;; forj.el ends here
```

### Test Framework Issues

#### Problem: ERT tests not discovered or running
**Symptoms**:
- `M-x ert` shows "No tests defined"
- Tests defined but not executing

**Solution**:
```elisp
;; Ensure proper test file structure
;;; test/forj-test.el --- Tests for forj.el -*- lexical-binding: t -*-

(require 'ert)
(require 'forj)  ; Load the package being tested

;; Test naming convention: package-test-feature-description
(ert-deftest forj-test-paren-checker-balanced ()
  "Test that forj-paren-checker handles balanced parentheses."
  (should (eq 'valid (plist-get (forj-paren-check "(defun test ())") :status))))

;; For batch testing
;; emacs -batch -l ert -l test/forj-test.el -f ert-run-tests-batch-and-exit
```

## API Integration Issues

### Gemini API Problems

#### Problem: "API key not found" error
**Symptoms**:
```
Error: No API key found for provider 'gemini'
```

**Solution**:
```bash
# Set environment variable before starting Emacs
export GEMINI_API_KEY="your-actual-api-key-here"
emacs

# Or set in shell profile
echo 'export GEMINI_API_KEY="your-api-key"' >> ~/.bashrc
source ~/.bashrc
```

```elisp
;; In code, validate API key exists
(defun forj-get-api-key ()
  "Retrieve Gemini API key from environment variable."
  (or (getenv "GEMINI_API_KEY")
      (user-error "GEMINI_API_KEY environment variable not set")))
```

#### Problem: API requests timing out
**Symptoms**:
```
Error: API request timed out after 30 seconds
```

**Solution**:
```elisp
;; Implement proper timeout handling
(defun forj-api-call (prompt)
  "Make API call with proper timeout handling."
  (condition-case err
      (with-timeout (30  ; 30 second timeout
          (user-error "API request timed out"))
        (forj-make-request prompt))
    (error
     (message "API call failed: %s" (error-message-string err))
     nil)))
```

#### Problem: JSON parsing errors from API response
**Symptoms**:
```
Error: Invalid JSON response from Gemini API
```

**Solution**:
```elisp
;; Robust JSON parsing with error handling
(defun forj-parse-api-response (response)
  "Parse API response with error handling."
  (condition-case err
      (json-parse-string response :object-type 'plist)
    (json-parse-error
     (message "Failed to parse API response: %s" (error-message-string err))
     (message "Raw response: %s" (substring response 0 (min 200 (length response))))
     nil)))
```

## Buffer and File Operations

### Buffer Management Issues

#### Problem: "Buffer is read-only" when trying to modify
**Symptoms**:
```
Buffer *forj* is read-only
```

**Solution**:
```elisp
;; Check and handle read-only buffers
(defun forj-insert-safely (text)
  "Insert TEXT into buffer, handling read-only status."
  (if buffer-read-only
      (let ((inhibit-read-only t))
        (insert text))
    (insert text)))

;; Or toggle read-only temporarily
(defun forj-modify-buffer (content)
  "Modify buffer content safely."
  (let ((was-read-only buffer-read-only))
    (when was-read-only
      (read-only-mode -1))
    (insert content)
    (when was-read-only
      (read-only-mode 1))))
```

#### Problem: Point position lost after buffer operations
**Symptoms**: Cursor jumps to unexpected location after function execution

**Solution**:
```elisp
;; Always use save-excursion for temporary point movement
(defun forj-analyze-buffer ()
  "Analyze buffer without affecting point position."
  (save-excursion
    (goto-char (point-min))
    ;; Do analysis work
    (analysis-function)))

;; For operations that should preserve point and mark
(defun forj-modify-region (start end replacement)
  "Replace region while preserving point and mark."
  (save-mark-and-excursion
    (delete-region start end)
    (goto-char start)
    (insert replacement)))
```

### File System Operations

#### Problem: Permission denied when writing files
**Symptoms**:
```
Error: Permission denied, cannot write to /path/to/file.el
```

**Solution**:
```elisp
;; Check file permissions before writing
(defun forj-write-file-safely (filename content)
  "Write CONTENT to FILENAME with permission checking."
  (let ((dir (file-name-directory filename)))
    (unless (file-writable-p dir)
      (user-error "Directory %s is not writable" dir))
    (when (file-exists-p filename)
      (unless (file-writable-p filename)
        (user-error "File %s is not writable" filename)))
    ;; Create backup before writing
    (when (file-exists-p filename)
      (copy-file filename (concat filename ".bak") t))
    (write-region content nil filename)))
```

#### Problem: File encoding issues with special characters
**Symptoms**: Special characters corrupted when reading/writing files

**Solution**:
```elisp
;; Specify encoding explicitly
(defun forj-read-file-utf8 (filename)
  "Read file with UTF-8 encoding."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents filename)
      (buffer-string))))

(defun forj-write-file-utf8 (filename content)
  "Write file with UTF-8 encoding."
  (let ((coding-system-for-write 'utf-8))
    (write-region content nil filename)))
```

## Code Validation Issues

### forj-paren-checker Problems

#### Problem: False positives in string literals
**Symptoms**: Parentheses inside strings incorrectly flagged as unbalanced

**Example**:
```elisp
(defun test () 
  (message "This has (parens) inside string"))  ; Incorrectly flagged
```

**Solution**:
```elisp
;; Track string state in parser
(defun forj-paren-check (code)
  "Parse code with proper string literal handling."
  (let ((in-string nil)
        (in-comment nil)
        (paren-stack nil)
        (pos 0))
    (while (< pos (length code))
      (let ((char (aref code pos)))
        (cond
         ;; Handle string literals
         ((and (eq char ?\") (not in-comment))
          (if in-string
              ;; Check if it's escaped
              (unless (and (> pos 0) (eq (aref code (1- pos)) ?\\))
                (setq in-string nil))
            (setq in-string t)))
         ;; Handle comments
         ((and (eq char ?\;) (not in-string))
          (setq in-comment t))
         ;; Handle parentheses only when not in string or comment
         ((and (not in-string) (not in-comment))
          (when (memq char '(?\( ?\[ ?\{))
            (push char paren-stack))
          (when (memq char '(?\) ?\] ?\}))
            (pop paren-stack))))
        (when (eq char ?\n)
          (setq in-comment nil))
        (setq pos (1+ pos))))
    ;; Return result based on paren-stack state
    (if paren-stack
        (list :status 'invalid :errors (list "Unbalanced parentheses"))
      (list :status 'valid))))
```

#### Problem: Incorrect line/column reporting
**Symptoms**: Error locations don't match actual position in code

**Solution**:
```elisp
;; Track line and column numbers accurately
(defun forj-paren-check-with-position (code)
  "Parse code with accurate line/column tracking."
  (let ((line 1)
        (column 1)
        (pos 0)
        (errors nil))
    (while (< pos (length code))
      (let ((char (aref code pos)))
        ;; Track newlines for line counting
        (when (eq char ?\n)
          (setq line (1+ line)
                column 1))
        ;; Detect errors and record position
        (when (error-detected-p char)
          (push (list :line line :column column :error "Description")
                errors))
        (setq pos (1+ pos)
              column (1+ column))))
    errors))
```

## Performance Issues

### Memory Usage Problems

#### Problem: High memory consumption during large file processing
**Symptoms**: Emacs becomes slow or unresponsive with large files

**Solution**:
```elisp
;; Process files in chunks to manage memory
(defun forj-process-large-file (filename)
  "Process large file in manageable chunks."
  (with-temp-buffer
    (let ((chunk-size 8192)  ; 8KB chunks
          (total-size (nth 7 (file-attributes filename))))
      (with-temp-message (format "Processing %s..." filename)
        (insert-file-contents filename)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((chunk-start (point))
                (chunk-end (min (+ (point) chunk-size) (point-max))))
            (process-chunk (buffer-substring chunk-start chunk-end))
            (goto-char chunk-end)
            ;; Allow Emacs to process other events
            (sit-for 0.01)))))))
```

### Response Time Issues

#### Problem: API responses taking longer than 3 seconds
**Symptoms**: User interface freezes during API calls

**Solution**:
```elisp
;; Implement asynchronous API calls
(defun forj-async-api-call (prompt callback)
  "Make asynchronous API call with CALLBACK."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (json-encode prompt-data)))
    (url-retrieve 
     api-endpoint
     (lambda (status)
       (if (plist-get status :error)
           (message "API call failed: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "\n\n")
         (let ((response (buffer-substring-no-properties (point) (point-max))))
           (funcall callback response))))
     nil t)))

;; Usage with progress indication
(defun forj-prompt-async (prompt)
  "Handle prompt with async API call and progress indication."
  (forj-show-activity "🤔 Thinking...")
  (forj-async-api-call 
   prompt
   (lambda (response)
     (forj-hide-activity)
     (forj-display-response response))))
```

## Error Handling Best Practices

### Comprehensive Error Recovery

```elisp
;; Template for robust error handling
(defun forj-robust-operation (input)
  "Perform operation with comprehensive error handling."
  (condition-case err
      (progn
        ;; Validate input
        (unless input
          (user-error "Input cannot be nil"))
        (unless (stringp input)
          (user-error "Input must be a string"))
        
        ;; Perform operation
        (let ((result (dangerous-operation input)))
          ;; Validate result
          (unless result
            (error "Operation returned no result"))
          result))
    
    ;; Handle specific error types
    (file-error
     (message "File operation failed: %s" (error-message-string err))
     nil)
    (json-parse-error
     (message "JSON parsing failed: %s" (error-message-string err))
     nil)
    (user-error
     ;; Re-signal user errors (don't log these)
     (signal (car err) (cdr err)))
    (error
     ;; Log unexpected errors
     (message "Unexpected error in forj-robust-operation: %s" 
              (error-message-string err))
     (when forj-debug
       (message "Error details: %S" err))
     nil)))
```

## Debug Mode Implementation

### Enable Detailed Logging

```elisp
;; Debug mode variable
(defvar forj-debug nil
  "Enable debug mode for detailed logging.")

;; Debug logging function
(defun forj-debug (format-string &rest args)
  "Log debug message if forj-debug is enabled."
  (when forj-debug
    (message "[FORJ DEBUG] %s" (apply #'format format-string args))))

;; Usage in functions
(defun forj-complex-operation (data)
  "Complex operation with debug logging."
  (forj-debug "Starting operation with data: %S" data)
  (let ((result (process-data data)))
    (forj-debug "Operation completed, result: %S" result)
    result))

;; Enable debug mode
;; M-x set-variable RET forj-debug RET t
```

## Common Pitfalls and Solutions

### 1. Lexical vs Dynamic Binding
**Problem**: Unexpected variable behavior

**Solution**: Always use lexical binding
```elisp
;;; forj.el --- Description -*- lexical-binding: t -*-
```

### 2. Interactive Function Design
**Problem**: Functions not callable with M-x

**Solution**: Proper interactive declarations
```elisp
(defun forj-prompt (prompt-text)
  "Prompt user with PROMPT-TEXT."
  (interactive "sEnter prompt: ")  ; 's' for string input
  ;; Function body
  )
```

### 3. Proper Package Cleanup
**Problem**: Package leaves state when disabled

**Solution**: Implement cleanup functions
```elisp
(defun forj-cleanup ()
  "Clean up forj resources."
  (when (buffer-live-p (get-buffer "*forj*"))
    (kill-buffer "*forj*"))
  (setq forj-conversation-history nil))

;; Hook into package disable
(add-hook 'kill-emacs-hook #'forj-cleanup)
```

---

**Note**: This troubleshooting guide will be continuously updated during development as new issues are encountered and resolved. Each problem should include the exact error message, symptoms, root cause analysis, and tested solution.