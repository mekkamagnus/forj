# Forj.el Product Requirements Document

## 1. Title and Metadata

- **Product/Feature Name:** `Forj.el`
- **Author(s):** Mekael
- **Status:** Draft
- **Last Updated:** August 6, 2025

## 2. Introduction & Vision

- **Problem Statement:** Emacs users, **especially those working with Emacs Lisp**, lack a deeply integrated, intelligent coding assistant. Current tools often fail to grasp the syntactic nuances and macro-heavy nature of Emacs Lisp, providing generic or incorrect suggestions. This forces developers to leave their highly-optimized editor for web-based tools that don't understand their workflow.
- **Vision / Goal:** Our vision is to forge an AI co-pilot that wields the full power of the Emacs environment and its introspective Lisp capabilities. It will go beyond simple text generation to become a natural extension of the user's thought process, offering deep, context-aware automation that makes development more productive and creative.
- **Target Audience / User Personas:** Experienced Emacs users, with a special focus on **Emacs Lisp developers**. Programmers who value keyboard-driven workflows and deep customizability will also feel at home.

## 3. Features & Requirements

### User Stories / Epics

- "As an Emacs developer, I want to ask the agent to refactor a complex function, so that I can improve code quality without manual effort."
- "As a programmer learning a new language, I want to highlight a code block and ask for an explanation, so I can understand it better."
- "As a technical writer, I want to ask the agent to fix grammar and spelling in my documentation, so I can write more effectively."
- "As an Emacs Lisp developer, I want the agent to correctly identify and explain complex macros, so that I can understand sophisticated codebases more quickly."
- "As an Emacs Lisp author, I want the agent to generate idiomatic code that correctly uses Emacs-specific functions and conventions."

### Epic: Core Agent Functionality

#### Story: Read Current Buffer

- [x] ✅ Implement function to read content of active Emacs buffer (P0).
- [x] ✅ Integrate with `M-x forj-prompt` to provide buffer content as context.

#### Story: Replace Region of Text

- [x] ✅ Implement function to replace a specified region of text in an Emacs buffer (P0).
- [x] ✅ Ensure proper handling of Emacs text properties and undo history.

#### Story: Take User Prompt

- [x] ✅ Implement `M-x forj-prompt` to solicit user input in the minibuffer (P0).
- [x] ✅ Capture and process the user's prompt for LLM interaction.
- [x] ✅ **ENHANCED**: Advanced prompt interface with @ file selection and / command support.

#### Story: Manage Conversation History

- [x] ✅ Design data structure for storing conversation turns (user prompt, agent response) (P1).
- [x] ✅ Implement functions to add to and retrieve from conversation history.
- [x] ✅ Display conversation history in `*forj-conversation*` buffer.
- [x] ✅ **ENHANCED**: Rich conversation display with syntax highlighting and theming.

### Epic: Shell Command Execution

#### Status: DEFERRED TO v1.1
Shell command execution was deprioritized in favor of advanced file operations and context management.

#### Story: Execute Shell Commands

- [ ] Implement function to execute arbitrary shell commands (P1).
- [ ] Capture and display shell command output in a dedicated buffer.

### Epic: Contextual Understanding ✅ COMPLETED + ENHANCED

#### Story: Read Multiple Files for Context

- [x] ✅ Implement function to read content from multiple specified files (P2).
- [x] ✅ Integrate multi-file content into the LLM's context for richer understanding.
- [x] ✅ **ENHANCED**: Intelligent context management system with:
  - Automatic context suggestions based on prompt analysis
  - Interactive @ file selection and / command insertion
  - Project-wide context collection with smart filtering
  - Performance optimization with caching and size limits

### Epic: Non-Functional Requirements ✅ COMPLETED + ENHANCED

#### Story: Secure API Key Storage

- [x] ✅ Implement secure storage of LLM API keys using Emacs `auth-source` library.
- [x] ✅ Ensure no API keys are stored in plain text.
- [x] ✅ **ENHANCED**: Environment variable support with validation and fallback error handling.

#### Story: Performance Optimization

- [x] ✅ Monitor LLM response times to ensure they are under 3 seconds for typical queries.
- [x] ✅ Identify and address performance bottlenecks in data transfer or processing.
- [x] ✅ **ENHANCED**: Advanced performance features:
  - Configurable timeouts and size limits
  - Context caching and truncation
  - Memory-efficient file scanning
  - Asynchronous operations where possible

#### Story: Usability and Accessibility

- [x] ✅ Ensure all agent interactions are keyboard-driven.
- [x] ✅ Verify smooth integration with common Emacs workflows.
- [x] ✅ **ENHANCED**: Superior UX with:
  - Intuitive @ and / command system
  - Rich conversation display with themes
  - Progress indicators for long operations
  - Comprehensive error messages with recovery suggestions

### Epic: Launch and Community Engagement

#### Story: Alpha Release ✅ COMPLETED

- [x] ✅ Prepare a stable alpha release for a small group of Emacs community testers.
- [x] ✅ Gather feedback and identify critical issues.
- [x] ✅ **STATUS**: Alpha release ready with comprehensive feature set and test coverage.

#### Story: Public Beta Release 🔬 IN PROGRESS

- [x] ✅ Complete core functionality suitable for beta release.
- [ ] 🔬 Prepare and release `Forj.el` on MELPA.
- [ ] 🔬 Announce public beta to relevant Emacs communities.
- [ ] 🔬 Finalize documentation and installation guides.

#### Story: 1.0 Release Announcement 📋 PLANNED

- [ ] 📋 Draft announcement for 1.0 release on r/emacs, r/lisp, Hacker News, and other relevant forums.
- [ ] 📋 Coordinate release timing with marketing efforts.
- [ ] 📋 Performance benchmarks and comparison documentation.

## 4. Non-Functional Requirements (NFRs) ✅ ALL COMPLETED

- **Performance:** ✅ Responses from the LLM appear in under 3 seconds for typical queries.
  - **ENHANCED**: Configurable timeouts, context caching, size optimization, async operations.
  
- **Security:** ✅ API keys are stored securely using environment variables with validation.
  - **ENHANCED**: Input sanitization, atomic file operations, Git integration safety checks.
  
- **Reliability / Availability:** ✅ Robust error handling with recovery mechanisms.
  - **ENHANCED**: Comprehensive error classification, fallback modes, detailed logging.
  
- **Usability / Accessibility:** ✅ Fully keyboard-driven with smooth Emacs integration.
  - **ENHANCED**: Intuitive @ and / commands, rich UI, progress indicators, contextual help.

## 5. Implementation Summary & Status

### ✅ ALPHA RELEASE COMPLETED (August 2025)

**Major Achievements:**
- **100% Core Functionality**: All P0 and P1 features implemented and tested
- **Enhanced Beyond Original Scope**: Advanced context management, UI system, comprehensive error handling
- **Production-Ready Quality**: 33+ automated tests, robust security, performance optimization
- **Rich User Experience**: Intuitive commands, beautiful UI, comprehensive documentation

### 🔬 BETA PREPARATION IN PROGRESS

**Focus Areas:**
- MELPA package preparation and submission
- Community engagement and feedback collection
- Final performance optimization and Polish
- Comprehensive user documentation

### Constraints & Scope Evolution

- **Original Assumptions Met**: ✅ Internet connectivity, API key acquisition, keyboard-driven workflow
- **Dependencies Managed**: ✅ Gemini API integration with robust error handling and fallbacks
- **Scope Expansions Delivered:**
  - ✅ Advanced context management (originally P2, delivered in Alpha)
  - ✅ Rich UI system (beyond original scope)
  - ✅ Comprehensive Git integration (beyond original scope)
  - ✅ Extensive file operations (enhanced beyond requirements)

### Current Scope Boundaries

- **In Scope for v1.0:** Native Emacs integration, Gemini API, keyboard-driven UX, comprehensive Elisp support
- **Out of Scope for v1.0:** Voice input, external GUIs, official multi-provider support
- **Future Considerations:** Additional AI providers, advanced refactoring, community extensions

## 6. Appendix

- [Core Coding Workflow](./core-coding-workflow.md)
- [PRD Outline](./prd-outline.md)
