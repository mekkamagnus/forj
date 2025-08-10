# Forj.el Development Roadmap

## Overview

Development roadmap for Forj.el - AI co-pilot for Emacs with deep Emacs Lisp integration. Following TDD methodology and focusing on MVP features first.

## 🎨 UI/UX Features Quick Reference

**PRIORITY: Phase 1.6 UI/UX Enhancement** - Comprehensive interface overhaul
- Full markdown rendering, syntax highlighting, themes, interactive components, modern layout
- Complete visual redesign before moving to Phase 1.7

**Technical Implementation Areas**:
- **Syntax Highlighting**: `forj-syntax-highlight` using `font-lock-mode` integration
- **Markdown Rendering**: `forj-markdown` with code fence support and rich formatting
- **Color Schemes**: `forj-theme` with custom faces and theme integration  
- **Interactive Elements**: `forj-ui-components` with clickable buttons and tooltips
- **Buffer Design**: `forj-buffer-design` with split-window support and layout management

## Phase 1: Coding Agent Foundation (MVP)

**Timeline**: 2-3 weeks  
**Goal**: Claude Code-style AI coding agent integrated into Emacs with conversation buffer and file operations

**Current Status (as of implementation)**: 
- ✅ **COMPLETED**: Sections 1.2, 1.3, 1.4, 1.4.5, 1.5 - All core file operations, conversation system, validation foundation, minimal API integration, and cross-buffer editing
- 🎨 **NEXT PRIORITY**: Section 1.6 - UI/UX Enhancement & Quality Gates (comprehensive interface overhaul)
- 🔄 **REMAINING**: Sections 1.7, 1.8 - Agent interface and advanced API features
- 📊 **Test Coverage**: 30+ passing tests covering all implemented functionality including API integration
- 🎯 **MILESTONE ACHIEVED**: Working AI co-pilot ready for real-world testing and development
- 🚀 **FOCUS**: Interface enhancement before advancing to additional features

### 1.1 Project Setup & Infrastructure

- [x] Initialize Emacs package structure
- [x] Set up test framework with ERT
- [x] Configure package headers and metadata
- [x] Create basic project documentation

### 1.2 Code Validation Foundation (P0)

- [x] **forj-paren-checker**: Custom parentheses balance checker (FIRST PRIORITY - MUST BE BUILT BEFORE ALL OTHER CODE)
  - [ ] **Purpose**: Enable AI and developers to validate ALL Emacs Lisp code before execution
  - [ ] **Critical**: This function validates ALL other forj.el code during development
  - [x] Write failing test for unbalanced parentheses detection
  - [x] Implement detailed paren analysis with line/column reporting
  - [x] Return structured data: line number, column, paren type, error description
  - [x] Handle nested structures: (), [], {}, strings, comments
  - [x] Provide machine-readable validation results for AI consumption
  - [x] Include syntax error recovery suggestions for AI correction
  - [x] Test with AI-generated code examples and edge cases
  - [x] Integration point for AI self-validation workflow
  - [x] **Mandatory**: All subsequent forj.el functions must pass forj-paren-checker validation

### 1.3 Conversation & Activity System (P0)

- [x] **forj-conversation-buffer**: Main interaction buffer (BUILT AFTER forj-paren-checker)
  - [x] **Purpose**: Claude Code-style conversation interface showing AI activity
  - [x] Write failing test for conversation buffer creation
  - [x] **MANDATORY**: Validate all buffer code with forj-paren-checker before implementation
  - [x] Implement _forj_ buffer with conversation history
  - [x] Display user prompts and AI responses in structured format
  - [x] Show AI "thinking" indicators and progress updates
  - [x] Handle buffer persistence across sessions
  - [x] Support markdown rendering for AI responses
  - [x] Add copy/paste functionality for code blocks
- [x] **forj-activity-tracking**: Real-time activity display
  - [x] Write failing test for activity status updates
  - [x] **MANDATORY**: Validate all activity tracking code with forj-paren-checker
  - [x] Show current AI operation status ("Reading file...", "Analyzing code..")
  - [x] Display file operations and modifications in real-time
  - [x] Track conversation context and token usage
  - [x] Provide clear success/error indicators

### 1.4 File System Operations (P0)

- [x] **forj-file-reader**: Read files from current directory
  - [x] Write failing test for file content reading
  - [x] **MANDATORY**: Validate all file reader code with forj-paren-checker
  - [x] Implement secure file reading with permission checks
  - [x] Support common text file formats (elisp, md, txt, etc.)
  - [x] Handle large files with content truncation/summarization
  - [x] Provide file tree browsing capabilities
- [x] **forj-file-writer**: Write and edit files
  - [x] Write failing test for file creation and modification
  - [x] **MANDATORY**: Validate all file writer code with forj-paren-checker
  - [x] **CRITICAL**: Use forj-paren-checker to validate ALL generated .el files before writing
  - [x] Implement safe file writing with backup creation
  - [x] Support atomic file operations
  - [x] Handle file permissions and ownership
  - [x] Integrate with version control awareness
- [x] **forj-directory-operations**: Current directory context
  - [x] Write failing test for directory scanning
  - [x] **MANDATORY**: Validate directory operations code with forj-paren-checker
  - [x] Scan current working directory for project files
  - [x] Build project context from file structure
  - [x] Detect project type (elisp package, general project)
  - [x] Ignore common non-essential files (.git, .DS_Store, etc.)

#### 1.4.5 Minimal API Integration (P0) ⭐ COMPLETED ✅

**Timeline**: 4-6 hours ✅ COMPLETED  
**Goal**: Enable basic AI interaction for immediate testing of all implemented features ✅ ACHIEVED  
**Strategic Value**: Transforms forj.el from foundation to working AI co-pilot ✅ COMPLETE

- [x] **forj-auth**: Secure credential management
  - [x] Write failing test for API key retrieval
  - [x] **MANDATORY**: Validate all authentication code with forj-paren-checker  
  - [x] Implement secure `forj-get-api-key` from `GEMINI_API_KEY` environment variable
  - [x] Add credential validation and error handling
  - [x] Ensure no plain-text key exposure or logging

- [x] **forj-api-call**: Basic HTTP communication
  - [x] Write failing test for API request structure
  - [x] **MANDATORY**: Validate all API communication code with forj-paren-checker
  - [x] Implement `forj-api-request` using url.el for Gemini API
  - [x] Add JSON request/response handling
  - [x] Implement basic error handling and timeouts
  - [x] Support conversation context in API calls

- [x] **forj-prompt**: Simple user interface
  - [x] Write failing test for conversational prompting
  - [x] **MANDATORY**: Validate all prompt interface code with forj-paren-checker
  - [x] Implement `M-x forj-prompt` command with conversation context
  - [x] Support natural language coding requests
  - [x] **CRITICAL**: All AI-generated code responses must pass forj-paren-checker validation
  - [x] Integration with existing file operations and conversation system

- [x] **forj-integration**: Response processing
  - [x] Write failing test for AI response handling
  - [x] **MANDATORY**: Validate all response processing code with forj-paren-checker
  - [x] Parse and apply AI suggestions to files using existing file operations
  - [x] Integrate with conversation history and activity tracking
  - [x] Provide confirmation prompts for file modifications
  - [x] Handle API errors gracefully with user feedback

**Acceptance Criteria**:
- [x] Can send prompts to Gemini API and receive responses
- [x] AI responses integrated with existing conversation system
- [x] File operations work with AI-generated content and pass validation
- [x] Secure credential management without exposure
- [x] Error handling provides clear user guidance
- [x] All generated code passes forj-paren-checker validation

**Testing Benefits**:
- ✅ Real-world testing of all implemented file operations
- ✅ Validation of conversation system with actual AI responses  
- ✅ Security testing with AI-generated code
- ✅ Project context validation with real AI analysis
- ✅ End-to-end workflow testing

**🎉 PHASE 1.4.5 COMPLETION SUMMARY**:
- ✅ **API Integration**: Full Gemini API communication with UTF-8 encoding, error handling, and context size management
- ✅ **Security**: Environment-based API key management with no plaintext exposure
- ✅ **User Interface**: `M-x forj-prompt` command with automatic conversation buffer display
- ✅ **Validation**: All AI responses validated with forj-paren-checker integration
- ✅ **Error Handling**: Robust multibyte text handling, buffer error protection, graceful API failure recovery
- ✅ **Testing**: Comprehensive syntax validation, multibyte character tests, and end-to-end workflow verification
- ✅ **Strategic Achievement**: Successfully transformed forj.el from development foundation to working AI co-pilot

### 1.5 Cross-Buffer Editing (P0)

- [x] **forj-buffer-manager**: Multi-buffer operations
  - [x] Write failing test for cross-buffer editing
  - [x] **MANDATORY**: Validate all buffer management code with forj-paren-checker
  - [x] Edit files from conversation buffer commands
  - [x] Open files mentioned in AI responses
  - [x] Switch between conversation and file buffers seamlessly
  - [x] Track which files are being modified by AI
- [x] **forj-edit-integration**: Smart editing operations
  - [x] Write failing test for AI-directed editing
  - [x] **MANDATORY**: Validate all editing code with forj-paren-checker
  - [x] **CRITICAL**: Use forj-paren-checker to validate ALL AI-generated code before applying to buffers
  - [x] Apply AI suggestions directly to open buffers
  - [ ] Show diff previews before applying changes
  - [ ] Support undo/redo for AI modifications
  - [ ] Handle multiple simultaneous file edits

### 1.6 UI/UX Enhancement & Quality Gates (P0) 🎨

**Goal**: Create a modern, visually appealing, and highly functional user interface with robust quality assurance

- [ ] **forj-syntax-highlight**: Enhanced syntax highlighting
  - [ ] Write failing test for code block syntax highlighting
  - [ ] **MANDATORY**: Validate all highlighting code with forj-paren-checker
  - [ ] Implement syntax highlighting for code blocks in conversation buffer
  - [ ] Support multiple programming languages (elisp, python, javascript, etc.)
  - [ ] Add proper indentation and formatting for AI-generated code
  - [ ] Integration with `font-lock-mode` for consistent highlighting
  - [ ] Custom highlighting for AI responses vs user input

- [ ] **forj-markdown**: Rich markdown rendering
  - [ ] Write failing test for markdown formatting
  - [ ] **MANDATORY**: Validate all markdown rendering code with forj-paren-checker
  - [ ] Render markdown headers, lists, and emphasis in conversation buffer
  - [ ] Support code fences with language-specific highlighting
  - [ ] Add clickable links and references
  - [ ] Proper line spacing and typography for readability
  - [ ] Support for tables and structured data display

- [ ] **forj-theme**: Color scheme and visual design
  - [ ] Write failing test for theme application
  - [ ] **MANDATORY**: Validate all theming code with forj-paren-checker
  - [ ] Implement color-coded conversation roles (user, AI, system)
  - [ ] Add status indicators with appropriate colors (success: green, error: red, warning: yellow)
  - [ ] Create custom faces for different types of content
  - [ ] Support both light and dark themes
  - [ ] Integration with popular Emacs themes (doom, spacemacs, etc.)
  - [ ] Custom icons and visual separators for better readability

- [ ] **forj-ui-components**: Interactive UI elements
  - [ ] Write failing test for interactive components
  - [ ] **MANDATORY**: Validate all UI component code with forj-paren-checker
  - [ ] Clickable code blocks for easy copying
  - [ ] Expandable/collapsible sections for long responses
  - [ ] Progress bars for long-running operations
  - [ ] Interactive buttons for common actions (apply, reject, copy)
  - [ ] Tooltip support for explanatory text
  - [ ] Context menus for quick actions

- [ ] **forj-buffer-design**: Enhanced buffer layout
  - [ ] Write failing test for buffer layout
  - [ ] **MANDATORY**: Validate all buffer design code with forj-paren-checker
  - [ ] Improved conversation buffer layout with clear message boundaries
  - [ ] Split-window support for side-by-side conversation and code editing
  - [ ] Customizable buffer width and font sizes
  - [ ] Header/footer information bars showing context and status
  - [ ] Smooth scrolling and better cursor management
  - [ ] Auto-scroll to new messages option

- [ ] **forj-code-validator**: Integration layer for all code validation
  - [ ] **Purpose**: Ensure ALL forj.el code passes validation before execution
  - [ ] Write failing test for code validation pipeline
  - [ ] **MANDATORY**: Every function must pass forj-paren-checker before being saved
  - [ ] Integrate forj-paren-checker into development workflow
  - [ ] Auto-validate on save for all .el files in forj.el project
  - [ ] Block compilation/loading of invalid code
  - [ ] Provide detailed error reports for validation failures
  - [ ] Create validation hooks for TDD workflow
  - [ ] Ensure AI-generated code passes validation before application

### 1.7 Coding Agent Interface (P0)

- [ ] **forj-prompt**: Main coding agent interface
  - [ ] Write failing test for conversational prompting
  - [ ] **MANDATORY**: Validate all prompt interface code with forj-paren-checker
  - [ ] Implement M-x forj-prompt command with conversation context
  - [ ] Support natural language coding requests
  - [ ] Add prompt history and autocompletion
  - [ ] Handle multi-turn conversations with context retention
- [ ] **forj-agent-commands**: Agent-style command processing
  - [ ] Write failing test for command interpretation
  - [ ] **MANDATORY**: Validate all command processing code with forj-paren-checker
  - [ ] **CRITICAL**: All AI-generated code responses must pass forj-paren-checker validation
  - [ ] Parse user intents ("create file", "edit function", "analyze project")
  - [ ] Execute file operations based on conversational commands
  - [ ] Provide confirmation prompts for destructive operations
  - [ ] Support command chaining and workflows

### 1.8 API Integration (P0)

- [ ] **forj-api-call**: Secure API communication
  - [ ] Write failing test for API request structure
  - [ ] **MANDATORY**: Validate all API communication code with forj-paren-checker
  - [ ] **CRITICAL**: Validate ALL AI-generated code responses with forj-paren-checker before processing
  - [ ] Implement streaming API responses for real-time feedback
  - [ ] Add error handling and timeouts
  - [ ] Support conversation context in API calls
- [ ] **forj-auth**: Secure credential management
  - [ ] Write failing test for auth-source integration
  - [ ] **MANDATORY**: Validate all authentication code with forj-paren-checker
  - [ ] Implement API key storage and retrieval
  - [ ] Ensure no plain-text key exposure

## Phase 2: Enhanced Functionality (P1)

**Timeline**: 2-3 weeks  
**Goal**: Rich interaction and context awareness

### 2.1 Conversation Management (P1)

- [ ] **forj-history**: Conversation history
  - [ ] Write failing test for history data structure
  - [ ] Implement conversation storage and retrieval
  - [ ] Create _forj-conversation_ buffer display
- [ ] **forj-context**: Context building
  - [ ] Write failing test for context aggregation
  - [ ] Combine buffer content with conversation history
  - [ ] Optimize context for token limits

### 2.2 Shell Command Integration (P1)

- [ ] **forj-shell**: Execute shell commands
  - [ ] Write failing test for command execution
  - [ ] Implement safe shell command runner
  - [ ] Capture and display output in dedicated buffer
- [ ] **forj-shell-integration**: AI-driven shell commands
  - [ ] Write failing test for AI command suggestion
  - [ ] Allow AI to suggest and execute shell commands
  - [ ] Add user confirmation for potentially dangerous commands

### 2.3 Enhanced User Experience (P1)

- [ ] **forj-region**: Work with selected regions
  - [ ] Write failing test for region-based operations
  - [ ] Allow AI to operate on selected text
  - [ ] Support multiple region operations
- [ ] **forj-eldoc**: Integration with eldoc
  - [ ] Write failing test for eldoc support
  - [ ] Provide contextual help for Emacs Lisp symbols
  - [ ] Show AI-generated explanations in eldoc


## Phase 3: Advanced Features (P2)

**Timeline**: 3-4 weeks  
**Goal**: Multi-file context and advanced AI capabilities

### 3.1 Multi-File Context (P2)

- [ ] **forj-file-reader**: Read multiple files
  - [ ] Write failing test for multi-file reading
  - [ ] Implement selective file content inclusion
  - [ ] Add file type detection and filtering
- [ ] **forj-project**: Project-aware context
  - [ ] Write failing test for project detection
  - [ ] Integration with project.el or projectile
  - [ ] Build project-wide context maps

### 3.2 Advanced AI Features (P2)

- [ ] **forj-refactor**: AI-powered refactoring
  - [ ] Write failing test for refactoring operations
  - [ ] Implement function and variable renaming
  - [ ] Add code structure improvements
- [ ] **forj-explain**: Code explanation
  - [ ] Write failing test for explanation generation
  - [ ] Explain complex Emacs Lisp constructs
  - [ ] Generate documentation for functions

### 3.3 Performance & Optimization (P2)

- [ ] **forj-cache**: Response caching
  - [ ] Write failing test for cache functionality
  - [ ] Cache frequent AI responses
  - [ ] Implement cache invalidation strategies
- [ ] **forj-async**: Asynchronous operations
  - [ ] Write failing test for async API calls
  - [ ] Non-blocking API requests
  - [ ] Progress indicators for long operations

## Phase 4: Polish & Release (P3)

**Timeline**: 2-3 weeks  
**Goal**: Production-ready package

### 4.1 Testing & Quality Assurance

- [ ] Comprehensive test suite (>90% coverage)
- [ ] Integration tests with real API
- [ ] Performance benchmarking
- [ ] Memory usage profiling
- [ ] Multi-Emacs version testing (27.1+)

### 4.2 Documentation & Examples

- [ ] Complete function documentation
- [ ] User manual with examples
- [ ] Configuration guide
- [ ] Troubleshooting guide
- [ ] Video demonstrations

### 4.3 Package Distribution

- [ ] Prepare MELPA package submission requirements
  - [ ] Create package recipe file for MELPA
  - [ ] Ensure package headers are complete and valid
  - [ ] Test package installation from local repository
  - [ ] Validate package metadata and dependencies
- [ ] MELPA submission and approval process
  - [ ] Submit pull request to melpa/melpa repository
  - [ ] Address any reviewer feedback
  - [ ] Monitor build process and fix issues
  - [ ] Celebrate MELPA acceptance!
- [ ] GitHub releases with proper tagging
- [ ] Changelog maintenance
- [ ] Issue template creation
- [ ] Contributing guidelines

### 4.4 Community & Feedback

- [ ] Alpha testing with select users
- [ ] Gather and incorporate feedback
- [ ] Beta release announcement
- [ ] r/emacs community engagement
- [ ] Hacker News submission preparation

## Long-term Vision (Future)

**Timeline**: Ongoing  
**Goal**: Ecosystem integration and advanced AI features

### Future Enhancements

- [ ] Support for multiple LLM providers (OpenAI, Anthropic, etc.)
- [ ] Voice input integration
- [ ] GUI components for complex interactions
- [ ] Plugin system for extensibility
- [ ] Integration with popular Emacs packages
- [ ] Collaborative AI features
- [ ] Code generation templates
- [ ] Advanced debugging assistance

## Success Metrics

- **Technical**: <3s response times, <100MB memory usage
- **Quality**: >90% test coverage, zero critical bugs
- **Adoption**: >100 GitHub stars, >50 MELPA downloads/week
- **Community**: >10 contributors, active issue discussions

## Risk Mitigation

- **API Dependency**: Plan for multiple provider support
- **Performance**: Regular benchmarking and optimization
- **Security**: Regular security audits and key management reviews
- **Compatibility**: Automated testing across Emacs versions
