# GEMINI.md - Readme-Driven Development Workflow

## Overview

Readme-Driven Development (RDD) is a development methodology where you write the README file first, before writing any code. This approach helps clarify the project's purpose, API design, and user experience before implementation begins.

## Readme-Driven Workflow Best Practices

### 1. Start with the README

**Before writing any code:**

- Write the complete README as if the project already exists
- Include installation instructions, usage examples, and API documentation
- Define the user experience and interface design
- Describe features as if they're already implemented

### 2. README Structure for Forj.el

```markdown
# Forj.el - AI Co-pilot for Emacs

## Installation

[Package manager instructions]

## Quick Start

[Minimal usage example]

## Features

[List of capabilities with examples]

## Configuration

[Customization options]

## API Reference

[Function documentation]

## Contributing

[Development setup instructions]
```

### 3. Benefits of RDD for Forj.el

**Clarity of Vision**:

- Forces you to think about the user experience first
- Helps identify unclear or complex APIs before implementation
- Provides a concrete target for what you're building

**Better API Design**:

- Writing usage examples reveals awkward interfaces
- Documents the happy path before edge cases complicate design
- Ensures the API is actually usable by real users

**Documentation-First Culture**:

- README becomes living documentation
- Examples serve as integration tests
- Reduces documentation debt after implementation

**Stakeholder Communication**:

- README serves as project specification
- Non-technical stakeholders can understand the vision
- Facilitates early feedback before significant investment

### 4. RDD Workflow Steps

#### Step 1: Write the README

```elisp
;; Example: Document the API you want to exist
(forj-prompt "Refactor this function to use modern Emacs Lisp patterns")
;; => AI analyzes current buffer and suggests improvements
```

#### Step 2: Validate with Stakeholders

- Share README with potential users
- Gather feedback on proposed interface
- Iterate on design before coding

#### Step 3: Implementation Planning

- Break down README features into development tasks
- Identify technical challenges from usage examples
- Plan implementation to match documented behavior

#### Step 4: Test-Driven Implementation

- Use README examples as acceptance criteria
- Write tests that verify documented behavior
- Implement features to pass both tests and match README

### 5. README-First Design Questions

**For each feature in the README, ask:**

- Is this the simplest possible interface?
- Would I actually want to use this API?
- Are the examples realistic and compelling?
- Does this fit naturally into Emacs workflows?

**For Forj.el specifically:**

- Does the command feel native to Emacs?
- Is the configuration discoverable via `customize`?
- Are error messages helpful for typical use cases?
- Does it integrate well with existing Emacs packages?

### 6. RDD Anti-Patterns to Avoid

**Over-Documentation**: Don't document features you're unsure about implementing
**Feature Creep**: Resist adding "nice-to-have" features to README
**Implementation Leaking**: Don't let technical constraints dictate user experience
**Vague Examples**: Use specific, realistic usage scenarios

### 7. Integration with TDD

**RDD � TDD Bridge**:

1. README examples become integration test scenarios
2. API documentation becomes unit test specifications
3. **IMPORTANT**: All subsequent `forj.el` functions must pass `forj-paren-checker` validation before running tests.
4. Error handling documentation becomes edge case tests
5. Configuration examples become validation tests

**Example Integration**:

```elisp
;; README Example:
;; (forj-prompt "Fix syntax errors in this buffer")
;; => Returns structured error report with suggestions

;; Corresponding Test:
(ert-deftest forj-test-syntax-error-detection ()
  "Test that forj-prompt detects and reports syntax errors."
  (with-temp-buffer
    (insert "(defun broken-func () (let ((x 1)")
    (let ((result (forj-prompt "Fix syntax errors in this buffer")))
      (should (plist-get result :errors))
      (should (string-match "unbalanced" (plist-get result :message))))))
```

### 8. Maintaining README Quality

**Regular Reviews**:

- Update README as features evolve
- Ensure examples remain current and functional
- Verify installation instructions work for new users

**User Feedback Integration**:

- Use actual user questions to improve documentation
- Add FAQ section based on common issues
- Include troubleshooting for typical problems

**Version Alignment**:

- Tag README versions with code releases
- Maintain backward compatibility in documented APIs
- Clearly mark deprecated features and migration paths

## Forj.el RDD Implementation

### Current Status

- [x] Product Requirements Document (PRD) defines vision
- [x] Architecture document outlines technical approach
- [ ] **NEXT**: Write comprehensive README with usage examples
- [ ] Validate README with potential Emacs users
- [ ] Convert README examples to integration tests
- [ ] Implement features following TDD methodology

### README Template for Forj.el

````markdown
# Forj.el - AI Co-pilot for Emacs Lisp Development

Transform your Emacs into an intelligent coding assistant specialized for Emacs Lisp development.

## Quick Start

```elisp
M-x forj-prompt RET "Refactor this function for better readability"
```
````

## Features

- **Smart Refactoring**: AI-powered code improvements
- **Syntax Validation**: Real-time parentheses and syntax checking
- **Context-Aware Help**: Understands your entire project
- **Emacs Integration**: Native keyboard-driven workflow

[Continue with detailed examples...]

```

This README-first approach ensures Forj.el will have excellent user experience and clear documentation from day one.
```
