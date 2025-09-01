# Specification 004: Natural Language Query Interpretation Layer

## High Level Objectives

**Natural Language Query Processing:**
As a Forj user, I want to ask natural language questions like "what is in this directory?" and have the system automatically trigger appropriate tool calls, so that I can interact with the AI assistant more intuitively without needing to know specific tool names.

**Intelligent Tool Mapping:**
As a developer using Forj, I want natural language queries to be automatically mapped to the correct tool calls with appropriate parameters, so that common tasks are completed efficiently without manual tool invocation.

## Low-level Objectives

- **Query Interpretation Engine:**
  - Parse natural language queries and extract intent and entities
  - Map recognized patterns to appropriate tool calls from forj-tools registry
  - Handle context-aware parameter resolution (e.g., "this directory" → current working directory)
- **Tool Integration:**
  - Seamlessly integrate with existing forj-tools.el dispatcher
  - Maintain all existing safety, validation, and approval mechanisms
  - Preserve tool result formats and error handling contracts
- **Testing:**
  - Comprehensive pattern matching tests for various query styles
  - Integration tests with actual tool execution
  - Performance tests for query interpretation speed

## 1. Overview

This specification defines a natural language query interpretation layer that sits between user input and the existing forj-tools system. The layer will analyze natural language queries, identify user intent, extract relevant parameters, and automatically trigger appropriate tool calls. This addresses the issue where users asking questions like "what is in this directory?" receive requests for clarification instead of automatic directory listing.

## 2. Core Concepts

### 2.1 User Experience

- **Natural Query Processing:** Users can ask questions in natural language and receive direct answers through automated tool execution
- **Context Awareness:** The system understands contextual references like "this directory", "current file", "these files"
- **Fallback Handling:** When queries cannot be mapped to tools, provide helpful suggestions or fall back to normal conversation mode

### 2.2 Backend Logic

- **AI Intent + Tool Planning (Primary):** Use an AI API call (Gemini) to classify whether the query requires tool calls and, if so, which tools and arguments to use. The AI emits a deterministic JSON “plan.”
- **Parameter Extraction (AI-guided):** The AI returns proposed arguments; the interpreter validates and normalizes them (paths → project-root, caps, defaults).
- **Tool Delegation:** Convert AI plan into one or more `tool-call` JSON objects and delegate to `forj-tools-dispatch` with full safety checks and approval gates.
- **Rule-based Fallback:** If the AI API is unavailable or returns invalid output, fall back to lightweight pattern matching for common intents (list files, read file, search).

## 3. Implementation Details

### 3.1 Frontend (forj-query-interpreter.el)

- AI-first interpretation flow using Gemini via `forj-api.el`.
- Deterministic JSON plan schema with confidence scoring and rationale.
- Validation/normalization of AI-suggested arguments (paths, limits, flags).
- Fallback to pattern registry when AI disabled/unavailable.
- User preferences (`defcustom`) for enabling AI interpretation and setting latency budgets/timeouts.

### 3.2 Backend (forj-tools.el integration)

- New `forj-tools-interpret-query` entrypoint that:
  - Calls AI to obtain an execution plan for the user query
  - Verifies plan against registry and sandbox constraints
  - Produces one or more `tool-call` JSONs for `forj-tools-dispatch`
- Dispatcher remains unchanged; interpretation is a preprocessing step in the orchestrator.
- All existing safety mechanisms and approval gates remain intact.
- All tool result/error contracts preserved.

### 3.3 AI Prompt + Fallback Patterns

- AI System Prompt includes: tool registry summary, sandbox rules, approval gates, and required JSON schema.
- Fallback Pattern Database (used only when AI is unavailable):
  - Directory/file listing ("what's in", "list files", "show contents")
  - Search ("find", "search for", "look for")
  - File operations ("read file", "show me", "open")
  - Context resolution ("this directory", "current file", "here")

AI Plan JSON Schema (returned by Gemini):
```
{
  "needs_tools": true|false,
  "tools": [
    {"name": "list_files", "args": {"directory": "."}, "confidence": 0.92},
    {"name": "search", "args": {"query": "TODO", "max_results": 100}, "confidence": 0.88}
  ],
  "rationale": "Short explanation of choices",
  "notes": "Any assumptions or ambiguities"
}
```

## 4. Testing Strategy

- **Unit Tests (test/test-forj-query-interpreter.el):**
  - AI plan parsing and validation (mock AI responses)
  - Argument normalization (paths, limits) and sandbox enforcement
  - Fallback pattern recognition when AI disabled/unavailable
  - Error handling for malformed/ambiguous AI outputs
- **Integration Tests (test/test-forj-tools-query-integration.el):**
  - End-to-end: query → AI plan → multiple tool executions
  - Preservation of approval gates and safety constraints
  - Tool result format consistency
- **Performance Tests (test/test-forj-query-performance.el):**
  - Interpretation latency budget (e.g., < 300ms average with AI; < 100ms fallback)
  - Memory usage for plan handling
  - Behavior under API timeouts/retries

## 5. Benefits

- **Improved User Experience:** Users can interact naturally without learning specific tool syntax or command structures
- **Reduced Friction:** Common tasks like directory listing, file reading, and searching become instant and intuitive
- **Maintains Safety:** AI proposes, but Forj validates and enforces sandboxing and approval gates before execution
- **Backward Compatibility:** Existing tool call workflows continue to work unchanged

## 6. File Structure

```
.
├── forj-query-interpreter.el    # New - Main query interpretation engine (AI + fallback)
├── specs/
│   └── 004-query-interpretation-layer-specs.md  # This document
├── forj-tools.el                # Modified - Add query preprocessing
├── forj.el                      # Modified - Integrate query interpreter
└── test/
    ├── test-forj-query-interpreter.el       # New - Unit tests
    ├── test-forj-tools-query-integration.el # New - Integration tests
    └── test-forj-query-performance.el       # New - Performance tests
```

## 7. Affected Files

- **New Files:**
  - `forj-query-interpreter.el` - Main query interpretation engine (Gemini-backed)
  - `test/test-forj-query-interpreter.el` - Unit tests
  - `test/test-forj-tools-query-integration.el` - Integration tests
  - `test/test-forj-query-performance.el` - Performance tests
- **Modified Files:**
  - `forj-tools.el` - (Optional) helper to surface registry to interpreter
  - `forj.el` - Integrate query interpreter with main conversation flow and AI call plumbing

## 8. Implementation Context

### Previous Specification Insights
- Specification 003 established comprehensive tool system with safety mechanisms and JSON-based protocol
- Existing forj-tools.el provides robust dispatcher and validation framework that must be preserved
- Error handling system from Specification 001 provides consistent error contracts that must be maintained

### Data Models [Source: docs/architecture.md#4-component-breakdown]
- AI Plan: `{needs_tools: bool, tools: [{name, args, confidence}], rationale: string, notes?: string}`
- Normalized Tool Call JSON: same as Spec 003 `tool-call` blocks
- Fallback Pattern: `{pattern: regex, intent: tool-name, arg-extractors: [fn]}`

### API Specifications [Source: docs/architecture.md#4-component-breakdown]
- `forj-query-interpret` interface: `(query &optional context) → (ai-plan | nil)`
- `forj-query-build-tool-calls` interface: `(ai-plan) → (list of tool-call json)`
- `forj-query-extract-parameters` (fallback): `(query pattern) → (parameters-alist)`
- `forj-query-resolve-context`: `(context-references) → (resolved-parameters)`
- Uses `forj-api.el` (Gemini) with model `gemini-2.0-flash-exp` and key from `GEMINI_API_KEY`
- All tool execution via `forj-tools-dispatch` (Spec 003 protocol)

AI Prompt Contract (Outline):
```
System: You are Forj’s query interpreter. Decide if tools are needed and produce a JSON plan.
Context: Project sandbox rules, allowed tools, path constraints, approval gates.
User: <natural language query>

Respond ONLY with compact JSON per schema:
{"needs_tools": bool, "tools": [{"name": string, "args": object, "confidence": number}], "rationale": string}
```

### Component Specifications [Source: docs/architecture.md#4-component-breakdown]
- Query interpreter integrates with forj-core.el orchestrator for conversation flow
- Uses forj-api.el (Gemini) for AI planning with strict JSON response handling
- Uses forj-error-system.el for consistent error handling and recovery
- Maintains forj-tools.el safety mechanisms including sandboxing and approval gates; AI plan is advisory, not authoritative

### File Locations [Source: docs/architecture.md#4-component-breakdown]
- Main implementation: `forj-query-interpreter.el` (new module alongside existing components)
- Integration point: `forj-tools.el` (enhanced dispatcher)
- Test files: `test/test-forj-query-*.el` following established testing patterns

### Testing Requirements [Source: docs/architecture.md#2-guiding-principles--architectural-goals]
- Use built-in `ert` framework following established testing patterns
- Mock AI responses for deterministic tests; include malformed outputs
- Integration tests verify safety, sandbox, and approval preservation
- Performance requirements: AI call under practical budget (< 300ms avg dev); fallback < 100ms; <50MB memory usage

### Technical Constraints [Source: docs/architecture.md#2-guiding-principles--architectural-goals]
- Must maintain deep Emacs integration and use standard Emacs conventions
- All user-facing configuration via `defcustom` for consistency
- Preserve modularity and loose coupling with existing components
- Maintain testability and developer experience standards

### Project Structure Notes
- New module follows existing naming convention with `forj-` prefix
- Integrates with established error handling and tool dispatch patterns
- Maintains architectural separation between UI, core logic, and external API
- No conflicts with existing component responsibilities or data flow

## Usage Instructions

1. Implement `forj-query-interpreter.el` with AI-first interpretation using `forj-api.el`; add strict JSON parser and schema validation
2. Provide fallback pattern registry for AI-unavailable paths
3. Integrate in `forj.el` orchestrator: query → AI plan → validated tool-calls → `forj-tools-dispatch`
4. Create comprehensive test suite: mocked AI plans, integration with tools, and fallbacks
5. Preserve safety mechanisms and approval gates; never execute unvalidated paths
6. Validate performance (latency budgets) and add timeouts/retries for AI calls
7. Update user docs with natural language examples and safety notes
8. Test with real queries and record problems in `docs/troubleshoot.md`
