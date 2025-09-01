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
- **IMPORTANT**: All subsequent `forj.el` functions must pass `forj-paren-checker` validation before running tests.
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

### API Configuration (Development Only)

- **AI Provider**: Google Gemini API exclusively during development
- **Model**: Use Gemini 2.5 Pro model for all API calls
- **API Key**: Retrieve from environment variable `GEMINI_API_KEY`
- **Example Configuration**:

```elisp
(setq forj-api-provider 'gemini)
(setq forj-api-model "gemini-2.0-flash-exp")
(setq forj-api-key (getenv "GEMINI_API_KEY"))
```

### Security Considerations

- Never log or expose API keys in plain text
- **MANDATORY**: Get API key from `GEMINI_API_KEY` environment variable only
- Do not store API keys in code or configuration files
- Sanitize user input before passing to external APIs
- Be cautious with `eval` and dynamic code execution
- Validate file paths and permissions before file operations

### Documentation

- Write comprehensive docstrings for all public functions
- Include usage examples in docstrings
- Document customization variables with clear descriptions
- Provide meaningful `interactive` prompts
- Consider adding info manual for complex packages

### Problem Documentation

- **MANDATORY**: Document all implementation problems in `docs/troubleshoot.md`
- Record exact error messages, symptoms, and root causes
- Include tested solutions with code examples
- Update troubleshooting guide continuously during development
- Create searchable reference for common Emacs Lisp pitfalls

### Progress Tracking

- **MANDATORY**: Check off completed items in `docs/roadmap.md` as development progresses
- Mark tasks as completed `[x]` immediately after finishing implementation
- Update roadmap status in real-time so progress can be monitored
- Use roadmap as single source of truth for development status
- Keep roadmap synchronized with actual implementation state

## Forj.el Specific Guidelines

### Core Functions Priority

1. **forj-paren-checker** (FIRST - validate all code before execution)
2. Buffer content reading (`forj-read-buffer`)
3. Text replacement (`forj-replace-region`)
4. User interaction (`forj-prompt`)
5. API integration (Google Gemini API only)

### Integration Goals

- Seamless workflow integration
- Keyboard-driven interface
- Context-aware assistance
- Respect for Emacs conventions and user customizations

### Performance Targets

- Gemini API responses under 3 seconds
- Minimal impact on Emacs responsiveness
- Efficient memory usage for conversation history
- Lazy loading of non-essential features

### Development API Requirements

- **Environment Setup**: Export `GEMINI_API_KEY` environment variable
- **Model Selection**: Use "gemini-2.0-flash-exp" for development testing
- **API Endpoint**: Use Google Gemini REST API exclusively
- **Request Format**: JSON requests with conversation context
- **Response Handling**: Stream responses for real-time feedback when possible

### Architecture Decision Records (ADR) Workflow

- Use ADRs to record a single, important architectural decision per document.
- Keep ADRs atomic and concise (1–3 pages); do not embed long design docs — link to them in References.
- Template: copy and fill [`templates/adr-template.md`](templates/adr-template.md:1).
- Storage and naming: place completed ADRs under `docs/adr/` and name files `000X-short-title.md` where `000X` is the next sequential number.
- Process:
  - Create the ADR at the time of decision (or immediately after).
  - Fill metadata (Title, Status, Date, Authors), Context, Decision, Alternatives, Rationale, Consequences, References, and Implementation notes.
  - Commit the ADR alongside the related PR/issue and add links to the issue/PR.
  - When a decision is superseded, create a new ADR and set Status: superseded; do not edit historical ADR content.
- Traceability: always link ADRs to related issues, PRs, benchmarks, or runbooks to make rationale discoverable.
- Operational notes: include migration/rollback steps and any operational impacts in the Consequences/Implementation Notes sections.

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

## Testing Interactive Features via TCP/Emacsclient

### Overview

For testing interactive Emacs features like forj's conversation interface, use emacsclient with TCP socket connection to programmatically interact with a running Emacs instance. This enables automated testing of UI components, conversation flows, and user interaction patterns.

### Setup Requirements

1. **Running Emacs Instance**: Start Emacs with server enabled
2. **Socket Identification**: Locate the Emacs server socket path
3. **TCP Connection**: Use emacsclient with proper socket parameters

### Socket Discovery

Find your Emacs server socket:
```bash
# Typical socket locations:
/var/folders/*/T/emacs*/server
/tmp/emacs*/server
~/.emacs.d/server/server
```

Example socket path:
```bash
/var/folders/k7/9pkqzxmj2j9cfzv5nclrn_l80000gn/T/emacs501/server
```

### Testing Commands

#### Basic Connection Test
```bash
emacsclient --socket-name "/path/to/socket" -e '(+ 1 2)'
```

#### Buffer Management
```bash
# List all buffers with file associations
emacsclient --socket-name "/path/to/socket" -e '(mapcar (lambda (buf) (list (buffer-name buf) (buffer-file-name buf))) (buffer-list))'

# Check current working directory
emacsclient --socket-name "/path/to/socket" -e '(pwd)'

# Switch to specific directory
emacsclient --socket-name "/path/to/socket" -e '(cd "/path/to/project")'
```

#### Forj-Specific Testing

**Kill and Restart Forj:**
```bash
# Kill forj conversation buffer
emacsclient --socket-name "/path/to/socket" -e '(kill-buffer "*forj*")'

# Reload forj.el
emacsclient --socket-name "/path/to/socket" -e '(load-file "forj.el")'

# Start forj
emacsclient --socket-name "/path/to/socket" -e '(forj-start)'
```

**Submit Prompts Programmatically:**
```bash
# Create prompt buffer and submit (works around interactive limitations)
emacsclient --socket-name "/path/to/socket" -e '(let ((prompt-text "What directory am I in")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))'
```

**Read Conversation Results:**
```bash
# Get conversation buffer contents
emacsclient --socket-name "/path/to/socket" -e '(with-current-buffer "*forj*" (buffer-substring-no-properties (point-min) (point-max)))'

# Check buffer status and size
emacsclient --socket-name "/path/to/socket" -e '(with-current-buffer "*forj*" (list (buffer-name) (buffer-size) (if (buffer-modified-p) "MODIFIED" "saved")))'
```

### Testing Interactive Features

#### Project-Specific Testing Workflow
1. **Setup Test Environment:**
   ```bash
   # Switch to test project directory
   emacsclient --socket-name "/path/to/socket" -e '(cd "/path/to/test/project")'
   
   # Clean slate - kill existing forj session
   emacsclient --socket-name "/path/to/socket" -e '(kill-buffer "*forj*")'
   
   # Start fresh forj instance
   emacsclient --socket-name "/path/to/socket" -e '(forj-start)'
   ```

2. **Test Specific Features:**
   ```bash
   # Test directory queries (reproduces troubleshooting scenario)
   emacsclient --socket-name "/path/to/socket" -e '(let ((prompt-text "What directory am I in")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))'
   
   # Test file operations
   emacsclient --socket-name "/path/to/socket" -e '(let ((prompt-text "List the files in this project")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))'
   ```

3. **Validate Results:**
   ```bash
   # Check if tools were triggered (expected behavior)
   emacsclient --socket-name "/path/to/socket" -e '(with-current-buffer "*forj*" (save-excursion (goto-char (point-min)) (search-forward "tool-call" nil t)))'
   
   # Extract conversation for analysis
   emacsclient --socket-name "/path/to/socket" -e '(with-current-buffer "*forj*" (buffer-substring-no-properties (point-min) (point-max)))'
   ```

### Test Automation Patterns

#### Regression Testing Script Template
```bash
#!/bin/bash
SOCKET="/var/folders/k7/9pkqzxmj2j9cfzv5nclrn_l80000gn/T/emacs501/server"

# Function to run emacsclient commands
run_emacs() {
    emacsclient --socket-name "$SOCKET" -e "$1"
}

# Test setup
run_emacs '(kill-buffer "*forj*")' > /dev/null 2>&1
run_emacs '(load-file "forj.el")'
run_emacs '(cd "/path/to/test/project")'
run_emacs '(forj-start)'

# Test case 1: Directory query
echo "Testing directory query..."
run_emacs '(let ((prompt-text "What directory am I in")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))'

# Wait for response
sleep 3

# Validate result
RESULT=$(run_emacs '(with-current-buffer "*forj*" (buffer-substring-no-properties (point-min) (point-max)))')
if [[ $RESULT == *"tool-call"* ]]; then
    echo "✅ PASS: Tool call triggered"
else
    echo "❌ FAIL: No tool call triggered"
    echo "Response: $RESULT"
fi
```

#### Error Reproduction Testing
```bash
# Reproduce specific issues for troubleshooting
run_emacs '(let ((prompt-text "show me the current directory")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))'

# Document exact response for specification writing
RESPONSE=$(run_emacs '(with-current-buffer "*forj*" (buffer-substring-no-properties (point-min) (point-max)))')
echo "Actual Response: $RESPONSE" >> troubleshooting-log.txt
```

### Best Practices

1. **Socket Path Management**: Always verify socket path before testing
2. **Buffer Cleanup**: Kill buffers between tests to ensure clean state
3. **Response Timing**: Allow sufficient time for API calls to complete (3-5 seconds)
4. **Error Handling**: Check for error messages in response content
5. **State Isolation**: Use different project directories for independent tests
6. **Documentation**: Record exact commands and responses for debugging

### Integration with Development Workflow

- **Specification Validation**: Use TCP testing to validate specification requirements match actual behavior
- **Regression Prevention**: Automate critical user journey testing
- **Performance Monitoring**: Measure response times for interactive features
- **Edge Case Testing**: Programmatically test error conditions and recovery

This TCP/emacsclient approach enables systematic testing of Emacs interactive features that would be difficult to test through traditional unit testing frameworks alone.
