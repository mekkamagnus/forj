# Forj.el Architecture

## 1. Overview

*   **Purpose:** This document provides a comprehensive overview of the Forj.el architecture. It is intended to be the primary map for developers navigating the codebase, explaining the major components, their interactions, and the design principles that guide the project.
*   **Target Audience:** This document is for new developers, system architects, and future contributors to the Forj.el project.

## 2. Guiding Principles & Architectural Goals

Our architecture is guided by a core philosophy: to be **quintessentially Emacs**. The following principles inform our design decisions:

*   **Deep Integration:** The architecture must be deeply integrated with the Emacs environment, using its introspective and interactive capabilities as a primary feature, not just a hosting platform.
*   **Modularity:** Components are designed to be loosely coupled, with distinct responsibilities. This allows for independent development and testing.
*   **Developer Experience:** The codebase should be easy to understand, and the development workflow simple to set up for new contributors.
*   **Testability:** Every part of the system is designed with automated testing in mind, using the built-in `ert` framework.
*   **User-Centric Configuration:** All user-facing configuration must use the standard Emacs `customize` system (`defcustom`) for a familiar and consistent user experience.

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
```

### Component Legend

*   **`forj-ui.el`:** Manages all user interface elements.
*   **`forj-core.el`:** The central orchestrator that manages the conversation loop and state.
*   **`forj-api.el`:** Handles all communication with the external LLM API.
*   **`forj-tools.el`:** Defines the tool system and the individual tools the agent can use.
*   **Emacs Built-ins:** Standard Emacs functions for buffer manipulation, process control, etc.

## 4. Component Breakdown

*   **`forj.el` (Main Entry Point):**
    *   **Responsibilities:** Defines the package, its dependencies (`Package-Requires`), and version. Provides the user-facing interactive commands (e.g., `M-x forj-prompt`). Handles loading the other components of the system.
*   **`forj-core.el` (The Orchestrator):**
    *   **Responsibilities:** Manages the state of the conversation. Receives user prompts from the UI, constructs the appropriate context (including history), and decides whether to call the LLM API or execute a tool. It is the "brain" of the agent.
*   **`forj-api.el` (API Client):**
    *   **Responsibilities:** Formats requests into the specific JSON structure required by the target LLM. Sends the request and parses the JSON response, handling potential network errors. It is the sole component responsible for external communication.
*   **`forj-tools.el` (Tool System):**
    *   **Responsibilities:** Defines the framework for creating and registering tools. Contains the implementation for all built-in tools, such as reading files, executing shell commands, or interacting with the Emacs environment.
*   **`forj-ui.el` (User Interface):**
    *   **Responsibilities:** Creates and manages the `*forj-conversation*` buffer. Handles getting input from the user via the minibuffer. Formats and displays the final output from the agent.

## 5. Data Flow & Interaction Scenarios

A typical user interaction follows this data flow:

1.  The user invokes an interactive command (e.g., `M-x forj-prompt`).
2.  `forj-ui.el` captures the user's input from the minibuffer.
3.  The prompt is sent to `forj-core.el` for processing.
4.  `forj-core.el` constructs a request, including conversation history and system prompts, and passes it to `forj-api.el`.
5.  `forj-api.el` sends the request to the external LLM.
6.  The LLM responds, either with a final answer or a request to use a tool.
7.  If a tool is requested, `forj-core.el` executes the relevant tool from `forj-tools.el`.
8.  The tool's output is sent back to the LLM via the same API path.
9.  The LLM generates a final response, which is sent back through `forj-core.el` to `forj-ui.el` to be displayed to the user.

## 6. Cross-Cutting Concerns

*   **Configuration Management:** All user-facing configuration is handled via `defcustom` variables, allowing users to modify settings through the standard Emacs `customize` interface.
*   **Error Handling:** Errors (e.g., network failures, invalid API keys) are caught at the boundary (e.g., in `forj-api.el`) and reported to the user gracefully in the UI.
*   **Security:** Sensitive data like API keys are handled by the `auth-source` library, avoiding plain-text storage.

## 7. Design Decisions & Rationale (ADRs)

*   **Decision:** We chose to build a native Emacs Lisp application instead of wrapping a web-based UI in an Emacs frame.
    *   **Context:** The goal is to create an AI agent that is deeply integrated with the Emacs environment.
    *   **Alternatives Considered:** Using a simple client to talk to a local web server; embedding a browser in Emacs.
    *   **Rationale:** A native Elisp implementation allows the agent to directly inspect and interact with the full state of the editor (buffers, modes, functions, etc.), which is the core of our vision. This provides a level of integration and power that is impossible with other approaches.

## 8. Future Considerations & Out of Scope

This section is defined by the project's Product Requirements Document.

*   **Out of Scope for v1.0:** Voice input, custom GUIs, and official support for LLM APIs other than Gemini.
*   **Future Work:** See the official PRD for potential future features.

## 9. Glossary

*   **ERT:** Emacs Lisp Regression Testing. The built-in testing framework for Emacs.
*   **MELPA:** The most popular community-driven package archive for Emacs.
*   **`defcustom`:** A macro used to define a user-configurable variable in Emacs.

## 10. Further Reading

*   **[Product Requirements Document (PRD)](./prd.md):** Outlines the product's purpose, features, and target audience.
*   **[Contributing Guide](./CONTRIBUTING.md):** Provides instructions for how to contribute to the project, including how to run tests and submit pull requests.