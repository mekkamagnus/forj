# forj-paren-checker Documentation

## Overview

`forj-paren-checker` is a custom parentheses balance checker designed specifically for AI agents to validate generated Emacs Lisp code. It provides detailed syntax analysis with precise error reporting to enable AI self-validation and correction workflows.

## Purpose

The primary purpose is to enable coding agents to automatically validate the syntactic correctness of Emacs Lisp code they generate, providing immediate feedback for iterative improvement.

## Core Function Specification

### `forj-paren-check (code-string)`

**Purpose**: Analyze Emacs Lisp code for parentheses balance and syntax errors

**Parameters**:
- `code-string` (string): The Emacs Lisp code to analyze

**Return Value**: 
A structured data object containing validation results:

```elisp
;; Success case
{:status 'valid
 :balanced t
 :message "Code is syntactically valid"}

;; Error case
{:status 'invalid
 :balanced nil
 :errors [{:line 15
          :column 23
          :type 'unmatched-paren
          :paren-type 'close-paren
          :expected ")"
          :found "}"
          :message "Unmatched closing brace, expected closing parenthesis"
          :suggestion "Replace '}' with ')' at line 15, column 23"}
         {:line 8
          :column 12
          :type 'unclosed-paren
          :paren-type 'open-paren
          :expected ")"
          :found nil
          :message "Unclosed parenthesis"
          :suggestion "Add closing ')' for parenthesis opened at line 8, column 12"}]
 :message "Found 2 syntax errors"}
```

## Error Types

### Unmatched Parentheses
- `unmatched-paren`: Wrong closing parenthesis type
- `unclosed-paren`: Missing closing parenthesis
- `extra-paren`: Unexpected closing parenthesis

### Context-Aware Errors
- `string-unclosed`: Unclosed string literal
- `comment-in-string`: Comment marker inside string
- `escaped-quote`: Improperly escaped quotes

## Features

### 1. Detailed Error Reporting
- **Line and column numbers** for precise error location
- **Error type classification** for programmatic handling
- **Human-readable messages** for debugging
- **Correction suggestions** for AI self-repair

### 2. Context Awareness
- **String literal handling**: Ignores parentheses inside strings
- **Comment handling**: Ignores parentheses in comments
- **Escape sequence support**: Properly handles escaped characters
- **Multi-line structure support**: Tracks position across lines

### 3. Nested Structure Support
- **Parentheses**: `()`
- **Square brackets**: `[]` 
- **Curly braces**: `{}` (for hash tables, etc.)
- **Mixed nesting**: Validates proper nesting order

### 4. AI Integration Features
- **Machine-readable output**: Structured data for programmatic consumption
- **Batch validation**: Process multiple code snippets
- **Incremental checking**: Validate code as it's being generated
- **Recovery suggestions**: Actionable fixes for common errors

## Usage Examples for AI Agents

### Basic Validation
```elisp
;; AI generates code and validates it
(let ((generated-code "(defun hello-world () (message \"Hello, World!\"))"))
  (forj-paren-check generated-code))
;; Returns: {:status 'valid :balanced t :message "Code is syntactically valid"}
```

### Error Detection
```elisp
;; AI generates invalid code
(let ((invalid-code "(defun broken-func () (let ((x 1) (message x))"))
  (forj-paren-check invalid-code))
;; Returns error with specific location and suggestion
```

### Iterative Correction Workflow
```elisp
(defun ai-code-validation-loop (code-string)
  "AI self-correction workflow using paren-checker."
  (let ((result (forj-paren-check code-string)))
    (if (eq (plist-get result :status) 'valid)
        code-string
      (let ((corrected-code (ai-apply-corrections code-string 
                                                  (plist-get result :errors))))
        (ai-code-validation-loop corrected-code)))))
```

## Integration Patterns

### 1. Pre-Execution Validation
```elisp
(defun ai-safe-eval (code-string)
  "Validate code before evaluation."
  (let ((check-result (forj-paren-check code-string)))
    (if (plist-get check-result :balanced)
        (eval (read code-string))
      (error "Code validation failed: %s" 
             (plist-get check-result :message)))))
```

### 2. Real-time Generation Feedback
```elisp
(defun ai-generate-with-validation (prompt)
  "Generate code with immediate validation feedback."
  (let* ((generated-code (ai-generate-code prompt))
         (validation (forj-paren-check generated-code)))
    (if (plist-get validation :balanced)
        generated-code
      (ai-generate-with-validation 
       (format "%s\nFix these errors: %s" 
               prompt 
               (mapconcat (lambda (err) (plist-get err :message))
                         (plist-get validation :errors)
                         "; "))))))
```

### 3. Batch Code Analysis
```elisp
(defun ai-validate-code-batch (code-snippets)
  "Validate multiple code snippets for AI training feedback."
  (mapcar (lambda (code)
            (cons code (forj-paren-check code)))
          code-snippets))
```

## Implementation Notes

### Performance Considerations
- **Single-pass parsing**: Analyze code in one traversal
- **Memory efficient**: Stream processing for large code blocks
- **Fast validation**: Sub-millisecond response for typical functions

### Error Recovery
- **Partial parsing**: Continue analysis after errors when possible
- **Error boundaries**: Identify scope of syntax errors
- **Suggestion ranking**: Prioritize most likely fixes

### Testing Strategy
Following TDD methodology:
1. **Write failing tests** for each error type
2. **Implement minimal** paren checking logic
3. **Refactor and enhance** while maintaining test coverage

## Future Enhancements

### Advanced Features
- **Semantic validation**: Check for undefined functions/variables
- **Style checking**: Emacs Lisp coding conventions
- **Performance analysis**: Identify potential bottlenecks
- **Documentation validation**: Check docstring completeness

### AI-Specific Features
- **Confidence scoring**: Rate likelihood of suggested fixes
- **Learning integration**: Improve suggestions based on corrections
- **Template validation**: Check against common Emacs Lisp patterns