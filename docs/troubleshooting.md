# Forj.el Troubleshooting Guide

This guide helps resolve common issues with Forj.el installation and usage.

## Common Issues

### C-c C-c keybinding not working in prompt interface

**Symptoms**:
- "C-c C-c is undefined" error when trying to submit prompt
- Forj prompt buffer opens but keybinding doesn't work
- Evil mode conflicts preventing keybinding from working

**Root Cause**:
1. Keymap initialization using `defvar` with complex initialization fails in some loading contexts
2. Evil mode overriding keybindings
3. Function symbol references not resolving properly during keymap creation

**Solution**:
1. Use explicit keymap initialization after variable declaration:
```elisp
(defvar forj-prompt-mode-map nil "Keymap for forj-prompt-mode.")
(let ((map (make-keymap)))
  (define-key map "\C-c\C-c" 'forj-prompt-submit)
  (setq forj-prompt-mode-map map))
```

2. Add evil-mode integration to the mode definition:
```elisp
(when (and (boundp 'evil-mode) evil-mode)
  (evil-insert-state)
  (evil-define-key 'insert forj-prompt-mode-map
    "\C-c\C-c" 'forj-prompt-submit))
```

3. Use mode hooks to ensure proper keymap activation:
```elisp
(defun forj-prompt-mode-setup ()
  (use-local-map forj-prompt-mode-map)
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-insert-state)))
(add-hook 'forj-prompt-mode-hook #'forj-prompt-mode-setup)
```

**Prevention**: Always test keybindings with both batch loading and interactive loading.

### `forj-start` function not available after loading forj.el

**Symptoms**:
- `M-x forj-start` shows "No match" or "command not found"
- Loading forj.el appears successful but function is missing

**Root Causes**:
1. Context management system modules failed to load due to missing dependencies
2. Circular dependency issues between modules
3. Syntax errors in context management files
4. File permission issues preventing module loading

**Diagnosis Steps**:

1. **Check loading messages**:
   ```elisp
   ;; Look for error messages in *Messages* buffer
   (switch-to-buffer "*Messages*")
   
   ;; Should see messages like:
   ;; Loading forj-context.el...
   ;; Loading forj-context-suggestions.el...  
   ;; Loading forj-prompt-interface.el...
   ```

2. **Use centralized loading ONLY**:
   ```elisp
   ;; CORRECT: Always load through main entry point
   (load-file "/path/to/forj/forj.el")
   
   ;; INCORRECT: Never load individual modules directly
   ;; (load-file "/path/to/forj/forj-context.el")           ; ❌ Don't do this
   ;; (load-file "/path/to/forj/forj-prompt-interface.el")  ; ❌ Don't do this
   ```

3. **Check function availability**:
   ```elisp
   ;; Check if function is defined
   (fboundp 'forj-start) ; Should return t
   
   ;; If context system failed, check for fallback
   (fboundp 'forj-start) ; Should still return t (fallback version)
   ```

**Solutions**:

### Solution 1: Use Fixed Version (Recommended)
The latest version includes graceful fallback handling. Reload forj.el:

```elisp
(load-file "/path/to/forj/forj.el")
```

You should see either:
- Full context system loaded successfully, or
- "Context system unavailable, providing fallback forj-start function"

### Solution 2: Manual Function Definition
If the function is still missing, define it manually:

```elisp
(defun forj-start ()
  "Launch Forj application (manual fallback)."
  (interactive)
  (let ((buffer (forj-conversation-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n\n=== Forj.el Started (Manual Mode) ===\n")
        (insert "Basic functionality available.\n")
        (insert "Use M-x forj-prompt for AI assistance.\n\n")))
    (display-buffer buffer)
    (message "Forj started manually. Use M-x forj-prompt for AI assistance.")))
```

### Solution 3: Debug Module Loading
Check each context module individually:

```elisp
;; Test forj-context.el
(condition-case err
    (load-file "/path/to/forj/forj-context.el")
  (error (message "forj-context error: %s" err)))

;; Test forj-context-suggestions.el  
(condition-case err
    (load-file "/path/to/forj/forj-context-suggestions.el")
  (error (message "forj-context-suggestions error: %s" err)))

;; Test forj-prompt-interface.el
(condition-case err
    (load-file "/path/to/forj/forj-prompt-interface.el")
  (error (message "forj-prompt-interface error: %s" err)))
```

### Duplicate UI Buffers - Two Main Buffers Issue

**Symptoms**:
- Two main UI buffers created when using `forj-start`: both `*Forj*` and `*forj*`
- User expects only one unified conversation buffer
- Confusing interface with unnecessary dashboard buffer

**Root Cause**:
The issue was in `forj-prompt-interface.el:295` where `forj-start()` created a separate `*Forj*` dashboard buffer instead of using the main conversation buffer.

**Original problematic code**:
```elisp
(defun forj-start ()
  "Launch Forj application with front page/dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*Forj*"))        ; ❌ Creates unnecessary buffer
        (origin-buffer (current-buffer)))
    (with-current-buffer buffer
      (forj-main-mode)                                ; ❌ Uses dashboard mode
      (forj-setup-front-page)                         ; ❌ Dashboard setup
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (message "Forj application started. Press 'p' for prompt interface or M-x forj-prompt")))
```

**Solution Applied**:
Modified `forj-start()` to directly open the conversation buffer:

```elisp
(defun forj-start ()
  "Launch Forj application directly to conversation buffer."
  (interactive)
  (let ((buffer (forj-conversation-buffer)))          ; ✅ Use existing conversation buffer
    (message "Forj AI Assistant ready. Type your prompt and press RET to send.")
    buffer))                                          ; ✅ Return buffer for consistency
```

**Additional Changes**:
1. **Removed unused dashboard code**: `forj-main-mode`, `forj-setup-front-page`, and related key bindings
2. **Updated help documentation**: Reflects single buffer approach
3. **Maintained API compatibility**: `forj-start` still works as expected

**Verification**:
After the fix, `forj-start` creates only one buffer (`*forj*` conversation buffer) instead of two.

**Prevention**: 
- Always question whether multiple UI entry points are necessary
- Favor single, unified interfaces over multiple specialized ones
- Test buffer creation in isolation to ensure clean behavior

### AI Responses Appearing in Messages Buffer Instead of Conversation Buffer

**Symptoms**:
- AI responses show debugging output in `*Messages*` buffer instead of conversation buffer
- Debug messages like `"forj-parse-api-response: extracted text = ..."` appear in Messages
- API request succeeds (JSON parsing works) but response doesn't appear in `*forj*` buffer
- User sees processing messages but no actual AI response content

**Root Cause**:
The issue was in the response processing flow where excessive debug messaging was obscuring the actual response display, and the response parsing was working but not being properly routed to the conversation buffer.

**Original problematic code**:
```elisp
(defun forj-parse-api-response (response)
  "Parse Gemini API RESPONSE and extract text content."
  (let ((candidates (cdr (assq 'candidates response))))
    (message "forj-parse-api-response: candidates = %s" candidates)  ; ❌ Excessive debug
    (if candidates
        (let ((content (cdr (assq 'content (aref candidates 0)))))
          (message "forj-parse-api-response: content = %s" content)    ; ❌ Excessive debug
          ;; ... more debug messages ...
          ))))
```

**Solution Applied**:
1. **Cleaned up debug messages**: Removed excessive debug output that was cluttering the Messages buffer
2. **Ensured proper response flow**: Fixed response parsing to properly return extracted text
3. **Improved error handling**: Better handling of nil responses and edge cases

**Fixed code**:
```elisp
(defun forj-parse-api-response (response)
  "Parse Gemini API RESPONSE and extract text content."
  (condition-case err
      (let ((candidates (cdr (assq 'candidates response))))
        (if candidates
            (let ((content (cdr (assq 'content (aref candidates 0)))))
              (if content
                  (let ((parts (cdr (assq 'parts content))))
                    (if parts
                        (let ((text (cdr (assq 'text (aref parts 0)))))
                          (when text
                            (message "AI response extracted successfully")    ; ✅ Clean success message
                            text))
                      ;; ... proper error handling ...
```

**Additional Improvements**:
1. **Streamlined forj-display-response**: Removed debug messages and improved reliability
2. **Better error reporting**: Response parsing errors now use forj-api-error system
3. **Cleaner user experience**: Only essential messages appear in Messages buffer

**Verification**:
After the fix, AI responses properly appear in the `*forj*` conversation buffer with minimal debug output in Messages.

**Prevention**: 
- Use debug messages sparingly and only for actual debugging sessions
- Test the complete request→parse→display flow end-to-end
- Ensure response parsing functions return proper values, not just log them

### M-x forj-prompt Not Showing Responses in Conversation Buffer

**Symptoms**:
- Running `M-x forj-prompt` opens interactive prompt buffer successfully
- User can type prompt and press C-c C-c to submit
- API request succeeds (can see network activity) but no response appears in `*forj*` buffer
- No error messages, but conversation buffer remains empty

**Root Cause**:
There are two different `forj-prompt` functions in the codebase:
1. **Interactive prompt interface** (`forj-prompt-interface.el`) - Opens input buffer 
2. **Direct API function** (`forj-api.el`) - Processes prompts and displays responses

The interactive version calls `forj-api-request-with-context` which gets the response but doesn't display it.

**Original problematic code**:
```elisp
(defun forj-api-request-with-context (prompt context-data)
  "Send API request with structured context included."
  (if (fboundp 'forj-api-request)
      (let* ((context-summary (forj-format-context-for-api context-data))
             (enhanced-prompt (format "Context:\n%s\n\nUser Request:\n%s"
                                      context-summary prompt)))
        (forj-api-request enhanced-prompt))    ; ❌ Returns response but doesn't display
    (error "API integration not available.")))
```

**Solution Applied**:
Modified `forj-api-request-with-context` to display the response after getting it:

```elisp
(defun forj-api-request-with-context (prompt context-data)
  "Send API request with structured context included."
  (if (fboundp 'forj-api-request)
      (let* ((context-summary (forj-format-context-for-api context-data))
             (enhanced-prompt (format "Context:\n%s\n\nUser Request:\n%s"
                                      context-summary prompt))
             (response (forj-api-request enhanced-prompt)))       ; ✅ Store response
        ;; Display the user input and AI response                ; ✅ Display both input and output
        (when (fboundp 'forj-display-user-input)
          (forj-display-user-input prompt))
        (when (and response (fboundp 'forj-display-response))
          (forj-display-response response)
          ;; Show the conversation buffer to user
          (display-buffer (forj-conversation-buffer)))
        response)                                                 ; ✅ Return response
    (error "API integration not available.")))
```

**Verification**:
After the fix, `M-x forj-prompt` → type request → `C-c C-c` → response appears in `*forj*` buffer.

**Testing**:
```elisp
;; Test the fix works
(load-file "test-prompt-fix.el")
;; Then: M-x test-forj-prompt-interactive
;; Submit a test prompt, then: M-x test-verify-response
```

**Prevention**: 
- Ensure all API integration functions that get responses also display them
- Test both direct API calls and interactive workflow paths
- Maintain clear separation between data processing and UI display functions

## Other Common Issues

### API Key Not Found

**Symptoms**:
```
Error: GEMINI_API_KEY environment variable not set
```

**Solutions**:
1. Set environment variable before starting Emacs:
   ```bash
   export GEMINI_API_KEY="your-api-key-here"
   emacs
   ```

2. Set in Emacs configuration:
   ```elisp
   (setenv "GEMINI_API_KEY" "your-api-key-here")
   ```

3. Restart Emacs after setting the environment variable.

### Prompt Interface Not Working

**Symptoms**:
- `M-x forj-prompt` opens buffer but @ and / keys don't work
- Context management features unavailable

**Solutions**:
1. Check if context system loaded:
   ```elisp
   ;; Should return t if context system is available
   (boundp 'forj-context-available)
   ```

2. Use basic version:
   ```elisp
   ;; Call the original forj-prompt function
   (call-interactively 'forj-prompt)
   ```

### File Reading Errors

**Symptoms**:
- "File not readable" errors
- Permission denied when loading modules

**Solutions**:
1. Check file permissions:
   ```bash
   ls -la /path/to/forj/*.el
   # All files should be readable (r-- permissions)
   ```

2. Fix permissions:
   ```bash
   chmod 644 /path/to/forj/*.el
   ```

### Performance Issues

**Symptoms**:
- Slow context collection
- High memory usage
- Emacs becomes unresponsive

**Solutions**:
1. Reduce context size:
   ```elisp
   (setq forj-context-max-size 25000)  ; Reduce from default 50000
   ```

2. Disable auto-suggestions:
   ```elisp
   (setq forj-context-auto-suggestions nil)
   ```

3. Clear cache if needed:
   ```elisp
   (forj-context-clear-cache)
   ```

## Diagnostic Commands

### Check Installation Status

```elisp
(defun forj-diagnose ()
  "Diagnose Forj.el installation status."
  (interactive)
  (with-output-to-temp-buffer "*Forj Diagnosis*"
    (princ "Forj.el Diagnostic Report\n")
    (princ "========================\n\n")
    
    ;; Core functions
    (princ (format "forj-start available: %s\n" (fboundp 'forj-start)))
    (princ (format "forj-prompt available: %s\n" (fboundp 'forj-prompt)))
    
    ;; Context system
    (princ (format "Context system: %s\n" 
                  (if (boundp 'forj-context-available) 
                      (if forj-context-available "Available" "Failed to load")
                    "Not loaded")))
    
    ;; API integration
    (princ (format "API system: %s\n" 
                  (if (fboundp 'forj-api-request) "Available" "Not loaded")))
    
    ;; Environment
    (princ (format "API key set: %s\n" 
                  (if (getenv "GEMINI_API_KEY") "Yes" "No")))
    
    ;; Recent messages
    (princ "\nRecent Messages:\n")
    (princ "================\n")
    (with-current-buffer "*Messages*"
      (princ (buffer-substring-no-properties 
              (max (point-min) (- (point-max) 2000)) 
              (point-max))))))
```

### Test Basic Functionality

```elisp
(defun forj-test-basic ()
  "Test basic Forj.el functionality."
  (interactive)
  (let ((tests '()))
    
    ;; Test 1: Function availability
    (push (list "forj-start function" (fboundp 'forj-start)) tests)
    (push (list "forj-prompt function" (fboundp 'forj-prompt)) tests)
    
    ;; Test 2: API key
    (push (list "API key configured" (not (null (getenv "GEMINI_API_KEY")))) tests)
    
    ;; Test 3: Context system
    (push (list "Context system" 
                (and (boundp 'forj-context-available) forj-context-available)) tests)
    
    ;; Test 4: File operations
    (push (list "File operations" (fboundp 'forj-read-file)) tests)
    
    ;; Display results
    (with-output-to-temp-buffer "*Forj Tests*"
      (princ "Forj.el Functionality Tests\n")
      (princ "===========================\n\n")
      (dolist (test tests)
        (princ (format "%s: %s\n" 
                      (car test) 
                      (if (cadr test) "PASS" "FAIL")))))
    
    ;; Return summary
    (let ((passed (length (cl-remove-if-not #'cadr tests)))
          (total (length tests)))
      (message "Tests completed: %d/%d passed" passed total))))
```

## Manual Installation Verification

If you're having issues, try this step-by-step verification:

```elisp
;; 1. Load core system
(load-file "/path/to/forj/forj.el")

;; 2. Check basic functions
(fboundp 'forj-prompt)  ; Should return t

;; 3. Test API key
(getenv "GEMINI_API_KEY")  ; Should return your key

;; 4. Try basic AI interaction
(call-interactively 'forj-prompt)

;; 5. If context system loaded, try advanced features
(call-interactively 'forj-start)
```

## Getting Help

If issues persist:

1. **Check the Messages buffer** (`C-h e` or `M-x view-echo-area-messages`)
2. **Run diagnostics** with the functions above
3. **Report the issue** with:
   - Emacs version (`M-x emacs-version`)
   - Error messages from `*Messages*` buffer
   - Results from `forj-diagnose` function
   - Steps you tried

## Recovery Procedures

### Complete Reset

If Forj.el is completely broken:

```elisp
;; 1. Unload all forj features
(mapatoms (lambda (sym)
            (when (string-match "^forj-" (symbol-name sym))
              (when (fboundp sym) (fmakunbound sym))
              (when (boundp sym) (makunbound sym)))))

;; 2. Clear from load history
(setq load-history 
      (cl-remove-if (lambda (entry)
                     (and (stringp (car entry))
                          (string-match "forj" (car entry))))
                   load-history))

;; 3. Reload from scratch
(load-file "/path/to/forj/forj.el")
```

### Minimal Working Configuration

For a guaranteed working setup:

```elisp
;; Minimal forj.el setup that should always work
(add-to-list 'load-path "/path/to/forj")

;; Set API key
(setenv "GEMINI_API_KEY" "your-key-here")

;; Load just the core
(require 'forj)

;; Test basic functionality
(forj-prompt "Hello, can you help me with Emacs Lisp?")
```

This minimal setup bypasses the context management system and provides basic AI assistance functionality.