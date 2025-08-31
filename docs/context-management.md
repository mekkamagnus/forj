# Context Management System

The Forj.el Context Management System provides intelligent, contextual AI assistance by leveraging native Emacs concepts such as buffers, regions, marks, compilation results, and project files.

## Overview

The context management system automatically collects and suggests relevant context for your AI interactions, making responses more accurate and helpful by providing the AI with the information it needs to understand your current work.

## Key Features

- **Automatic Context Suggestions**: Analyzes your prompts and current Emacs state to suggest relevant context
- **Multiple Context Sources**: Supports buffers, files, compilation results, and project-wide context
- **Smart Relevance Scoring**: Prioritizes context based on relevance to your current task
- **Performance Optimized**: Includes caching and size optimization for responsive operation
- **Seamless Integration**: Works with existing Forj.el API and conversation system

## Getting Started

### Basic Usage

1. **Launch Forj Application**:
   ```elisp
   M-x forj-start
   ```
   This opens the main Forj dashboard with available commands.

2. **Open Prompt Interface**:
   ```elisp
   M-x forj-prompt
   ```
   Or press `p` from the main dashboard.

3. **Use Context Features**:
   - Type `@` to select and include files in your prompt
   - Type `/` to insert common commands
   - The system automatically suggests relevant context based on your prompt

### Entry Points

- **`M-x forj-start`**: Main application launcher - opens front page/dashboard
- **`M-x forj-prompt`**: Direct access to interactive prompt interface with context management

## Context Sources

### Buffer Context

The system can collect context from Emacs buffers:

- **Current Buffer**: Automatically suggested when relevant
- **Multiple Buffers**: Collect from several buffers simultaneously
- **Region Context**: Use selected text regions with surrounding context
- **Modified Buffers**: Prioritize buffers with unsaved changes

### File Context

Collect context from project files:

- **Individual Files**: Manually select specific files with `@`
- **Project Files**: Automatically include relevant project files
- **File Relationships**: Include imported/required files and dependencies
- **File Type Detection**: Smart handling based on file type (elisp, markdown, etc.)

### Compilation Context

Analyze compilation results and errors:

- **Error Analysis**: Automatically include compilation errors and warnings
- **Build Output**: Context from recent build/compile operations
- **Location Information**: Link errors to specific file locations

### Project Context

Understand your entire project:

- **Project Structure**: Directory layout and organization
- **File Discovery**: Automatic scanning of relevant project files
- **Dependency Mapping**: Understanding of file relationships

## Prompt Interface

### Key Bindings

In the prompt interface (`*Forj Prompt*` buffer):

| Key Binding | Action |
|-------------|--------|
| `@` | Insert file path and add to context |
| `/` | Insert command |
| `C-c C-c` | Submit prompt |
| `C-c C-k` | Cancel prompt |
| `C-c C-s` | Show context suggestions |
| `C-c C-l` | List selected context sources |
| `C-c C-r` | Clear context sources |

### Context Selection Workflow

1. **Type Your Prompt**: Start typing your request
2. **Review Suggestions**: System automatically suggests relevant context
3. **Add Context**: Use `@` to manually add files or rely on automatic suggestions
4. **Submit**: Press `C-c C-c` to send your prompt with context to the AI

## Advanced Features

### Smart Context Suggestions

The system analyzes your prompts for:

- **File References**: Detects mentions of specific files
- **Error Keywords**: Identifies debugging and troubleshooting requests
- **Code Review Patterns**: Recognizes code analysis requests
- **Project Scope**: Understands project-wide operations

### Relevance Scoring

Context sources are scored based on:

- **Content Relevance**: How closely related to your prompt
- **File Type**: Programming files scored higher for code tasks
- **Recency**: Recently modified files get higher scores
- **Size**: Appropriately sized files preferred
- **Activity**: Current/modified buffers prioritized

### Performance Optimization

- **Caching**: Frequently accessed content cached for speed
- **Size Limits**: Automatic truncation to stay within API limits
- **Lazy Loading**: Content loaded only when needed
- **Async Operations**: Non-blocking context collection where possible

## Configuration

### Customization Variables

```elisp
;; Maximum size of context in characters
(setq forj-context-max-size 50000)

;; Enable automatic context suggestions
(setq forj-context-auto-suggestions t)

;; Enable context content caching
(setq forj-context-cache-enabled t)

;; Enable performance monitoring
(setq forj-context-performance-monitoring t)
```

### Integration Settings

The context system integrates with existing Forj.el settings:

```elisp
;; File extensions to consider for project context
(setq forj-supported-extensions '("el" "md" "txt" "org" "lisp" "py" "js" "ts"))

;; Patterns for files/directories to exclude
(setq forj-excluded-patterns '(".git" ".DS_Store" "*.elc" "*.log" "*~"))

;; Maximum file size to read
(setq forj-max-file-size 50000)
```

## API Integration

### For Developers

The context system provides several APIs for extension:

#### Context Collection

```elisp
;; Collect context from a buffer
(forj-collect-buffer-context buffer-or-name &optional region)

;; Collect context from a file
(forj-collect-file-context file-path)

;; Collect compilation context
(forj-collect-compilation-context &optional include-warnings)

;; Collect project-wide context
(forj-collect-project-context project-root &optional file-patterns)
```

#### Context Suggestions

```elisp
;; Analyze prompt for context needs
(forj-analyze-prompt-for-context prompt)

;; Get context suggestions based on current state
(forj-suggest-context-sources &optional prompt)

;; Collect context from suggested sources
(forj-collect-context &optional sources)
```

#### Processing

```elisp
;; Process prompt with context (new API)
(forj-process-prompt-with-context prompt context-data)

;; Format context for API consumption
(forj-format-context-for-api context-list)
```

## Troubleshooting

### Common Issues

1. **Context Not Being Collected**
   - Check file permissions
   - Verify files exist and are readable
   - Check `forj-max-file-size` setting

2. **Performance Issues**
   - Reduce `forj-context-max-size` if needed
   - Enable caching with `forj-context-cache-enabled`
   - Check performance metrics with `(forj-context-get-performance-metrics)`

3. **API Integration Problems**
   - Ensure `forj-api.el` is loaded
   - Check API key configuration
   - Verify network connectivity

### Debug Commands

```elisp
;; Check context suggestions for a prompt
(forj-analyze-prompt-for-context "your prompt here")

;; View current Emacs state analysis
(forj-analyze-emacs-state)

;; Check performance metrics
(forj-context-get-performance-metrics)

;; Clear cache if needed
(forj-context-clear-cache)
```

## Examples

### Basic File Review

```
Prompt: "Please review authentication.el for security issues"
Context: Automatically includes authentication.el and related files
Result: AI analyzes the specific file with security focus
```

### Error Debugging

```
Prompt: "Fix the compilation errors"
Context: Automatically includes compilation buffer and error locations
Result: AI understands specific errors and provides targeted fixes
```

### Project Analysis

```
Prompt: "Analyze the overall architecture of this project"
Context: Includes project structure, main files, and relationships
Result: AI provides comprehensive architectural analysis
```

### Code Review with Multiple Files

```
1. Type: "Review the API implementation"
2. Use @ to select: api.el, routes.el, handlers.el
3. Submit with C-c C-c
Result: AI reviews all selected files as a cohesive system
```

## Architecture

The context management system consists of three main modules:

1. **`forj-context.el`**: Core context collection functions
2. **`forj-context-suggestions.el`**: Smart context suggestion engine
3. **`forj-prompt-interface.el`**: Interactive prompt interface

These modules integrate seamlessly with the existing Forj.el architecture while providing enhanced contextual awareness for AI interactions.

## Performance Metrics

The system includes built-in performance monitoring:

- Context collection times
- Cache hit rates
- Memory usage optimization
- API request sizes

Monitor performance with:
```elisp
M-x forj-context-get-performance-metrics
```

## Future Enhancements

Planned features include:

- **Semantic Context**: Use LSP and treesitter for semantic code context
- **Context Learning**: ML-based relevance scoring based on user feedback
- **Remote Context**: Support for remote files and distributed development
- **Visual Context**: Screenshots and diagrams as context sources
- **Org-mode Integration**: Use Org files and agenda as context sources