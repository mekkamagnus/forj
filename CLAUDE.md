# CLAUDE.md - Forj.el Development Guidelines

## Project Overview
Forj.el is an AI co-pilot for Emacs with deep Emacs Lisp integration. Focus on creating a seamless, keyboard-driven coding assistant that understands Emacs conventions and workflows.

## Emacs Lisp Development Best Practices

### Code Style & Conventions
- Use `forj-` prefix for all public functions and variables
- Use `forj--` prefix for private/internal functions
- Follow Emacs naming conventions: `verb-noun` for functions, descriptive names for variables
- Keep lines under 80 characters when possible
- Use proper docstrings with triple semicolons `;;;` for file headers, double `;;` for section comments

### Function Design
- Write pure functions when possible - avoid side effects
- Use `interactive` forms for user-facing commands
- Provide meaningful error messages with `user-error` for user mistakes, `error` for programmer mistakes
- Use `cl-lib` prefixed functions instead of deprecated `cl` functions
- Prefer `let*` over nested `let` bindings for readability

### Buffer and Text Manipulation
- Always use `save-excursion` when moving point temporarily
- Use `with-current-buffer` instead of `set-buffer`
- Handle text properties correctly with functions like `buffer-substring-no-properties`
- Respect `buffer-read-only` status
- Use proper undo boundaries with `undo-boundary`

### Error Handling
- Use `condition-case` for recoverable errors
- Provide user-friendly error messages
- Log debugging information to `*Messages*` buffer with `message`
- Use `user-error` for expected user errors (shows in minibuffer without backtrace)

### Performance Considerations
- Use `with-temp-buffer` for temporary string manipulation
- Avoid repeated buffer searches - cache results when appropriate
- Use `looking-at` and `looking-back` for position-based checks
- Consider `save-restriction` and `narrow-to-region` for focused operations

### Integration with Emacs
- Follow standard keybinding conventions (`C-c` prefix for user bindings)
- Use appropriate hooks for initialization (`after-init-hook`, mode hooks)
- Integrate with `auth-source` for secure credential storage
- Support standard Emacs features like `eldoc`, `completion-at-point`
- Use `define-minor-mode` or `define-derived-mode` appropriately

### Development Workflow - RDD + TDD Integration

**MANDATORY DEVELOPMENT PROCESS**:
1. **README-FIRST**: Write comprehensive README with usage examples
2. **RED**: Write failing tests based on README examples - verify they fail
3. **GREEN**: Write minimal code to make tests pass
4. **REFACTOR**: Clean up code while keeping tests green
5. **REPEAT**: Continue with next README feature

### Readme-Driven Development (RDD)

**ALWAYS START WITH README**:
- Write README as if project already exists and works perfectly
- Include realistic usage examples and API documentation
- Define user experience before implementation
- Use README examples as integration test specifications

**RDD Best Practices**:
- Document the API you want to build, not what's easy to build
- Include error handling and edge cases in examples
- Write from user perspective, focusing on solving real problems
- Validate README with potential users before coding

**README Structure for Emacs Packages**:
```markdown
# Package-Name - Brief Description

## Installation
[Package manager instructions]

## Quick Start  
[Minimal working example]

## Usage Examples
[Realistic usage scenarios with code]

## Configuration
[Customization options with defcustom examples]

## API Reference
[Function documentation with parameters and return values]
```

**README → Test → Code Workflow**:
1. Write README example: `(forj-prompt "Fix syntax errors")`
2. Convert to test: `(should (plist-get (forj-prompt "Fix syntax") :success))`  
3. Write minimal implementation to pass test
4. Refactor while keeping README examples working

### TDD Workflow

**TDD Best Practices**:
- ALWAYS CREATE TEST BEFORE WRITING CODE - NO EXCEPTIONS
- Use `ert` (Emacs Regression Testing) framework
- Test file naming: `test-forj-[feature].el` or `forj-test.el`
- Run tests with `M-x ert` or `emacs -batch -l ert -l forj-test.el -f ert-run-tests-batch-and-exit`
- Each test should test ONE specific behavior
- Use descriptive test names: `forj-test-read-buffer-returns-string`

**Testing Patterns**:
```elisp
;; Basic test structure
(ert-deftest forj-test-function-name ()
  "Test description of expected behavior."
  (should (equal expected-value (actual-function-call))))

;; Buffer testing with temp buffer
(ert-deftest forj-test-buffer-operation ()
  "Test buffer manipulation functions."
  (with-temp-buffer
    (insert "test content")
    (should (string= "test content" (buffer-string)))))

;; Error testing
(ert-deftest forj-test-error-conditions ()
  "Test that function properly handles error conditions."
  (should-error (forj-function-that-should-fail)))
```

**Testing Requirements**:
- Test interactive functions by simulating user input with `cl-letf`
- Use `with-temp-buffer` for isolated testing environments
- Include edge cases: empty buffers, read-only buffers, nil inputs
- Test error conditions and recovery
- Mock external dependencies (API calls, file system)
- Test with different Emacs versions if targeting broad compatibility

**Test Organization**:
- Group related tests in the same file
- Use setup/teardown with `ert` fixtures when needed
- Keep tests independent - no shared state between tests
- Run full test suite before committing code

### Package Structure
- Follow standard Emacs package conventions
- Include proper package headers (`;;; Package-Requires:`, `;;; Version:`)
- Use `autoload` cookies for user-facing commands
- Group related functionality into logical sections
- Provide comprehensive customization options with `defcustom`

### Security Considerations
- Never log or expose API keys in plain text
- Use `auth-source` for credential management
- Sanitize user input before passing to external APIs
- Be cautious with `eval` and dynamic code execution
- Validate file paths and permissions before file operations

### Documentation
- Write comprehensive docstrings for all public functions
- Include usage examples in docstrings
- Document customization variables with clear descriptions
- Provide meaningful `interactive` prompts
- Consider adding info manual for complex packages

## Forj.el Specific Guidelines

### Core Functions Priority
1. Buffer content reading (`forj-read-buffer`)
2. Text replacement (`forj-replace-region`)
3. User interaction (`forj-prompt`)
4. API integration (secure, efficient)

### Integration Goals
- Seamless workflow integration
- Keyboard-driven interface
- Context-aware assistance
- Respect for Emacs conventions and user customizations

### Performance Targets
- API responses under 3 seconds
- Minimal impact on Emacs responsiveness
- Efficient memory usage for conversation history
- Lazy loading of non-essential features

## Development Process Summary

### 1. README-First Development
- Write complete README with usage examples as if package exists
- Focus on user experience and API design
- Include installation, configuration, and troubleshooting sections
- Validate with potential users before implementation

### 2. Test-Driven Implementation
- Convert README examples to integration tests
- Write unit tests for each function before implementation
- Follow RED-GREEN-REFACTOR cycle strictly
- Maintain >90% test coverage

### 3. Documentation Validation
- Ensure all README examples actually work
- Keep documentation in sync with implementation
- Add real usage examples from development process
- Include common error scenarios and solutions

### 4. Quality Gates
- All README examples must pass as integration tests
- No code commits without corresponding tests
- Documentation must be updated with any API changes
- Performance targets must be met for all documented features

This RDD + TDD approach ensures Forj.el will have excellent user experience, comprehensive documentation, and robust implementation from the start.