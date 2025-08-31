# Forj.el Architecture

## 1. Overview

- **Purpose:** This document provides a comprehensive overview of the Forj.el architecture. It is intended to be the primary map for developers navigating the codebase, explaining the major components, their interactions, and the design principles that guide the project.
- **Target Audience:** This document is for new developers, system architects, and future contributors to the Forj.el project.

## 2. Guiding Principles & Architectural Goals

Our architecture is guided by a core philosophy: to be **quintessentially Emacs**. The following principles inform our design decisions:

- **Deep Integration:** The architecture must be deeply integrated with the Emacs environment, using its introspective and interactive capabilities as a primary feature, not just a hosting platform.
- **Modularity:** Components are designed to be loosely coupled, with distinct responsibilities. This allows for independent development and testing.
- **Developer Experience:** The codebase should be easy to understand, and the development workflow simple to set up for new contributors.
- **Testability:** Every part of the system is designed with automated testing in mind, using the built-in `ert` framework.
- **User-Centric Configuration:** All user-facing configuration must use the standard Emacs `customize` system (`defcustom`) for a familiar and consistent user experience.
- **Robust Error Handling:** Errors are handled consistently across all components with comprehensive context, recovery mechanisms, and dual-format output to support both human developers and automated AI agents. All error conditions provide actionable feedback and clear resolution paths.

## 3. High-Level Architecture

### System Diagram

The following diagram illustrates the major components and the primary flow of data from user input to the final response.

```
+-----------+       +----------------+       +-------------+
| User      |------>| forj-ui.el     |------>| forj-core.el|
| (M-x ...) |       | (Minibuffer,   |       | (Orchestrator)|
+-----------+       |  *forj-convo*) |       +-------------+
                    +----------------+              |
                                                    |
                           +------------------------+------------------------+
                           |                        |                        |
                           v                        v                        v
                    +--------------+      +-----------------+      +----------------+
                    | forj-api.el  |      | forj-tools.el   |      | Emacs Built-ins|
                    | (To LLM)     |      | (File, Shell)   |      | (Buffer fns)   |
                    +--------------+      +-----------------+      +----------------+
                           |                        |                        |
                           +------------------------+------------------------+
                                                    |
                                                    v
                                          +-------------------+
                                          | forj-error-system |
                                          | (Centralized      |
                                          |  Error Handling)  |
                                          +-------------------+
```

### Component Legend

- **`forj-ui.el`:** Manages all user interface elements.
- **`forj-core.el`:** The central orchestrator that manages the conversation loop and state.
- **`forj-api.el`:** Handles all communication with the external LLM API.
- **`forj-tools.el`:** Defines the tool system and the individual tools the agent can use.
- **`forj-error-system.el`:** Centralized error handling, classification, logging, and recovery system.
- **Emacs Built-ins:** Standard Emacs functions for buffer manipulation, process control, etc.

## 4. Component Breakdown

### Core System (Implemented)

- **`forj.el` (Main Entry Point) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Package definition, component loading, and user-facing commands. Provides `forj-start`, `forj-prompt`, and core conversation management functions. Handles fallback modes when advanced features are unavailable.
  - **Key Features:** Activity tracking, conversation history, file operations, syntax validation (`forj-paren-check`), Git integration, project scanning.

- **`forj-api.el` (API Client) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Google Gemini API integration with secure key management, request/response handling, context building, and response validation. Includes comprehensive error handling and multibyte character cleaning.
  - **Key Features:** Enhanced context building, code-specific context for reviews, conversation logging, response validation with syntax checking.

- **`forj-error-system.el` (Centralized Error Handling) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Unified error classification, context enrichment, dual-format output (human + machine readable), comprehensive logging, and recovery mechanisms.
  - **Integration:** Used throughout all components for consistent error handling patterns.

### Advanced Features (Implemented)

- **`forj-context.el` (Context Management Base) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Foundation for intelligent context collection from buffers, files, and compilation results.
  
- **`forj-context-suggestions.el` (AI Context Analysis) - ✅ IMPLEMENTED:**
  - **Responsibilities:** AI-powered context suggestion engine that analyzes prompts and recommends relevant context sources.

- **`forj-prompt-interface.el` (Enhanced UI) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Interactive prompt interface with @ file selection, / command insertion, and intelligent context integration.

### UI Enhancement System (Implemented)

- **`forj-ui-integration.el` (Main UI Coordinator) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Coordinates all UI components and provides enhanced conversation experience.

- **`forj-ui-components.el` (Reusable Components) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Core UI building blocks and interactive elements.

- **`forj-buffer-layout.el` (Layout Management) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Advanced buffer layouts and window management.

- **`forj-theme.el` (Visual Theming) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Consistent visual styling and theme integration.

- **`forj-syntax-highlight.el` (Code Highlighting) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Syntax highlighting for code blocks in conversations.

- **`forj-markdown.el` (Markdown Rendering) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Rich text formatting for AI responses.

- **`forj-progress.el` (Progress Indicators) - ✅ IMPLEMENTED:**
  - **Responsibilities:** User feedback for long-running operations.

### Specialized Features (Implemented)

- **`forj-texinfo.el` (Documentation Integration) - ✅ IMPLEMENTED:**
  - **Responsibilities:** Integration with Emacs documentation system for context-aware help.

## 5. Data Flow & Interaction Scenarios

### Current Implementation Data Flow

The system supports multiple interaction patterns with sophisticated context management:

#### Enhanced Context-Aware Flow (forj-start)

1.  The user invokes `M-x forj-start` which loads the context management system.
2.  `forj-prompt-interface.el` presents an enhanced prompt buffer with @ and / command support.
3.  The user enters their prompt, optionally using @ to select files or / to insert commands.
4.  `forj-context-suggestions.el` analyzes the prompt and suggests relevant context sources.
5.  `forj-context.el` collects context from selected sources (buffers, files, compilation results).
6.  `forj-api.el` processes the prompt with collected context and sends to Google Gemini API.
7.  The API response is validated using `forj-paren-check` for syntax correctness.
8.  The response is displayed in the conversation buffer with syntax highlighting and formatting.

#### Legacy Direct Flow (forj-prompt)

1.  The user invokes `M-x forj-prompt` for direct AI interaction.
2.  `forj-api.el` builds basic context from project structure and conversation history.
3.  The request is sent directly to the Gemini API with existing context.
4.  Responses are validated and displayed in the conversation buffer.

#### File Operations Flow

1.  User selects files via `forj-browse-files` or uses automated project scanning.
2.  `forj.el` provides comprehensive file operations with Git integration.
3.  Operations include reading, writing, editing regions, backup/restore with atomic file operations.
4.  All operations are logged to the conversation buffer and tracked in history.

## 6. Cross-Cutting Concerns

- **Configuration Management:** Comprehensive `defcustom` system with 15+ configuration variables covering API settings, file operations, Git integration, and UI preferences. Full Emacs `customize` interface support.

- **Error Handling:** Advanced error management through `forj-error-system.el` with:
  - **Centralized Classification:** Structured error types (api-error, validation-error, file-error, user-error)
  - **Context Enrichment:** Detailed error context with recovery suggestions
  - **Dual Output:** Human-readable messages + machine-readable data for testing
  - **Multi-target Logging:** _Messages_, conversation buffer, files, stdout with configurable levels
  - **Recovery Mechanisms:** Interactive error resolution and automatic retry logic

- **Security:** Multi-layered security approach:
  - **API Keys:** Environment variable (`GEMINI_API_KEY`) with fallback validation
  - **File Operations:** Atomic writes, path validation, permission checks
  - **Input Sanitization:** Multibyte character cleaning for API requests
  - **Git Integration:** Safe staging and commit checking before operations

- **Performance Optimization:** 
  - **Context Caching:** Intelligent caching of file contents and project structure
  - **Size Limits:** Configurable file size limits and context truncation
  - **Async Operations:** Non-blocking API requests with timeout handling
  - **Resource Management:** Memory-efficient file scanning with depth/count limits

## 7. Design Decisions & Rationale (ADRs)

- **Decision:** We chose to build a native Emacs Lisp application instead of wrapping a web-based UI in an Emacs frame.

  - **Context:** The goal is to create an AI agent that is deeply integrated with the Emacs environment.
  - **Alternatives Considered:** Using a simple client to talk to a local web server; embedding a browser in Emacs.
  - **Rationale:** A native Elisp implementation allows the agent to directly inspect and interact with the full state of the editor (buffers, modes, functions, etc.), which is the core of our vision. This provides a level of integration and power that is impossible with other approaches.

- **Decision:** We implemented centralized error handling through `forj-error-system.el` instead of distributed error handling across components.
  - **Context:** The existing codebase had inconsistent error handling patterns, mixing `error` and `user-error` calls, with silent error suppression and limited context for debugging.
  - **Alternatives Considered:** Enhancing error handling incrementally in each component; using Emacs built-in error handling only; implementing error handling as part of the UI layer.
  - **Rationale:** Centralized error handling provides consistency across the entire system, enables comprehensive error context enrichment, supports both human and machine-readable error formats for automated testing, and allows for systematic error recovery mechanisms. This approach reduces cognitive load for developers and improves debuggability for both humans and AI agents.

- **Decision:** We chose Google Gemini API as the primary AI provider instead of supporting multiple providers initially.
  - **Context:** Need for reliable AI integration with manageable complexity during initial development.
  - **Alternatives Considered:** OpenAI GPT API, Anthropic Claude API, local models, multiple provider support from start.
  - **Rationale:** Google Gemini provides excellent code generation capabilities, reasonable pricing, and good API stability. Focusing on a single provider initially allows for better optimization and testing. The architecture supports adding more providers in the future through the existing `forj-api-provider` abstraction.

- **Decision:** We implemented advanced context management system instead of simple prompt-response interaction.
  - **Context:** Initial implementation showed that context-aware AI assistance produces significantly better results than isolated prompts.
  - **Alternatives Considered:** Simple prompt system, manual context inclusion, file-based context loading.
  - **Rationale:** Intelligent context collection and management enables the AI to provide more accurate, relevant, and actionable responses. The @ and / command system provides intuitive user control while automated suggestions reduce cognitive overhead.

## 8. Implementation Status & Future Considerations

### Current Status (Alpha)

**✅ Completed Features:**
- Complete core functionality (conversation, file operations, Git integration)
- Advanced context management system with intelligent suggestions
- Comprehensive UI enhancement system with theming and syntax highlighting
- Robust error handling and validation system
- Full Google Gemini API integration with security measures
- Extensive test coverage with 33+ automated tests

**🔬 In Development:**
- Performance optimizations for large projects
- Additional AI provider integrations
- Enhanced documentation and user guides

**📋 Planned for Beta:**
- MELPA package submission
- Community feedback integration
- Advanced refactoring capabilities
- Multi-language support beyond Emacs Lisp

### Out of Scope for v1.0

- Voice input integration
- Custom GUI implementations outside Emacs
- Official support for LLM APIs other than Gemini (experimental support may be added)
- Real-time collaborative editing features

## 9. Glossary

- **ERT:** Emacs Lisp Regression Testing. The built-in testing framework for Emacs.
- **MELPA:** The most popular community-driven package archive for Emacs.
- **`defcustom`:** A macro used to define a user-configurable variable in Emacs.

## 10. Further Reading

- **[Product Requirements Document (PRD)](./prd.md):** Outlines the product's purpose, features, and target audience.
- **[Contributing Guide](./CONTRIBUTING.md):** Provides instructions for how to contribute to the project, including how to run tests and submit pull requests.
