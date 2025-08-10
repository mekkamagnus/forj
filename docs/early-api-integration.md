# Early API Integration Guide

## Overview

This guide documents the implementation of Phase 1.4.5 - Minimal API Integration, which enables immediate testing of all implemented features with real AI interaction.

## Quick Start

### Prerequisites

1. Set your Gemini API key: `export GEMINI_API_KEY=your_key_here`
2. Load forj.el in Emacs: `M-x load-file RET forj.el RET`
   - API integration loads automatically!

### Basic Usage

```elisp
;; Start a conversation with AI
M-x forj-prompt RET "Analyze this project" RET

;; The AI will respond and you can choose to apply suggestions
```

## Architecture

### Core Components

- **forj-api.el**: Main API integration module
- **test/forj-api-test.el**: Comprehensive test suite
- **forj-get-api-key**: Secure credential management
- **forj-api-request**: HTTP communication with Gemini
- **forj-prompt**: User interface for AI interaction

### Security Features

- API keys only from environment variables (never stored in code)
- Response validation with forj-paren-checker
- Safe file operations with confirmation prompts
- No plain-text credential exposure

## Testing Benefits

### Immediate Validation

✅ **File Operations**: Test reading/writing with real AI analysis  
✅ **Conversation System**: Validate with actual AI responses  
✅ **Project Context**: Get real AI feedback on project structure  
✅ **Security**: Test validation with AI-generated code  
✅ **Error Handling**: Real-world error scenarios

### Development Workflow

1. **RED**: Write failing tests in `test/forj-api-test.el`
2. **GREEN**: Implement minimal API functions
3. **REFACTOR**: Enhance based on real usage

## Implementation Timeline

### Phase 1.4.5: 4-6 hours total

- **Hour 1**: Credential management and basic setup
- **Hours 2-3**: HTTP client and API communication
- **Hour 4**: User interface and integration
- **Hours 5-6**: Testing and refinement

## Usage Examples

### Basic Prompting

```elisp
(forj-prompt "Create a new function to validate email addresses")
```

### Context-Aware Requests

```elisp
(forj-prompt "Refactor the file-reading functions to handle binary files")
```

### Integration Testing

```elisp
;; Test with existing file operations
(forj-prompt "Read the current buffer and suggest improvements")
```

## Next Steps

After Phase 1.4.5 completion:

1. **Phase 1.5**: Enhanced cross-buffer editing
2. **Phase 1.6**: Development quality gates
3. **Phase 1.7**: Advanced agent interface
4. **Phase 1.8**: Full API integration with streaming

## Troubleshooting

### Common Issues

- **"GEMINI_API_KEY not set"**: Export your API key before starting Emacs
- **"No response received"**: Check internet connection and API key validity
- **"Invalid response"**: AI response failed validation - review the response

### Debug Commands

```elisp
;; Check API key availability
(forj-get-api-key)

;; Test API connection
(forj-api-request "Hello, world!")

;; Validate a response
(forj-validate-response "(defun test () \"hello\")")
```
