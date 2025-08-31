# Specification 003: Emacs-Native Coding Agent Tool Calls

## 1. Overview

### Purpose

Define a minimal but complete set of AI-invokable tools and a
lightweight invocation protocol so Forj can act as a coding agent
inside Emacs with safe file operations, testing, and shell execution.

### Goals

- Emacs-native tooling mapped to concrete Elisp functions
- Safety-first: validation, backups, error context, and undo
- Simple, deterministic tool-call protocol the model can follow
- Clear error contracts compatible with `forj-error-system`
- Works with current Google Gemini integration

### Success Criteria

- Tools cover read, write, search, edit-region, list-files, tests,
  and basic shell
- All write/edit operations validate `.el` via `forj-paren-check`
- Every tool returns machine-readable results and errors
- Tool usage is logged to the conversation buffer
- Round-trip examples work via `forj-api` + conversation flow

## 2. Architecture Alignment

- Tooling layer lives in a new `forj-tools.el` module referenced by
  the orchestrator and UI.
  [Source: docs/architecture.md#3-high-level-architecture]

- `forj-tools.el` exposes discrete, tested functions for each tool and
  a registry/dispatcher for model-initiated calls.
  [Source: docs/architecture.md#4-component-breakdown]

- All errors flow through centralized error handling with dual-format
  outputs for humans and machines.
  [Source: docs/architecture.md#6-cross-cutting-concerns]

- File operations must reuse the Phase 1.4 capabilities and patterns
  (backups, atomic writes, path validation, truncation rules).
  [Source: docs/phase-1-4-file-operations-workflow.md#overview--strategic-approach]

## 3. Invocation Protocol

The model requests actions by emitting a fenced JSON object with a
single tool call. Forj parses, validates, executes, and replies with a
structured result block.

### 3.1 Tool Call JSON (from model)

```
```tool-call
{
  "id": "uuid-or-counter",
  "name": "read_file",
  "args": {
    "path": "src/foo.el",
    "max_bytes": 65536
  },
  "meta": {
    "cwd": ".",            // must be project root or a subdirectory
    "timeout": 10
  }
}
```
```

Notes:
- Exactly one call per block. Deterministic parsing > flexibility.
- Unknown fields are ignored. Missing required fields cause errors.

### 3.2 Tool Result JSON (from Forj)

```
```tool-result
{
  "id": "uuid-or-counter",
  "name": "read_file",
  "ok": true,
  "result": {
    "path": "src/foo.el",
    "encoding": "utf-8",
    "content": "...possibly truncated...",
    "truncated": false,
    "size": 1234
  }
}
```
```

On failure:

```
```tool-result
{
  "id": "uuid-or-counter",
  "name": "read_file",
  "ok": false,
  "error": {
    "type": "file-error",
    "message": "Permission denied",
    "details": {"path": "src/foo.el"},
    "recovery": ["Check file permissions", "Try a different file"]
  }
}
```
```

### 3.3 Conversation Integration

- Forj appends the `tool-result` block into the conversation buffer.
- The model continues with a normal message that uses the result.
- UI annotates tool calls and results with concise, clickable headers.
  [Source: docs/architecture.md#3-high-level-architecture]

### 3.4 Project Sandbox and Working Directory

- Project Root: All tools operate relative to the current project root
  (workspace root), mirroring Claude Code’s workflow.
- Path Restriction: Any `path` arguments must resolve inside the project
  root. Attempts to access parent directories are rejected.
- `cwd` Restriction: Shell `cwd` must be the project root or one of its
  subdirectories; otherwise the call fails validation.
- Normalization: Forj normalizes and validates paths before execution.

## 4. Core Tools (MVP)

Each tool defines: name, intent, args, returns, errors, safety, and
its Emacs mapping. All write/edit tools must validate `.el` content via
`forj-paren-check` before persisting.

### 4.1 read_file (Claude Code-compatible)

- Intent: Safely read a text file with size limits, optional line-range,
  and encoding detection, matching Claude Code’s ReadFile semantics.
- Args:
  - `path` (string, required)
  - `max_bytes` (integer, optional; default 100_000)
  - `start_line` (integer, optional; 1-based)
  - `end_line` (integer, optional; inclusive)
  - `include_context` (integer, optional; lines of context around range)
- Returns: `:path`, `:encoding`, `:content`, `:truncated`, `:size`,
  `:range` (when line-range supplied)
- Errors: `file-error`, `validation-error`
- Safety: Detect binary; truncate large files; sanitize path; respect
  ignore patterns for private/secret files when configured.
- Mapping: `forj-read-file`
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-141-file-reader-system-priority-p0]

### 4.2 list_files (Claude Code-compatible)

- Intent: List files in a directory with depth limits and metadata; pairs
  with `glob` and follows IDE ignore semantics.
- Args:
  - `directory` (string, optional; default project root)
  - `max_depth` (integer, optional; default 5)
  - `include_hidden` (boolean, optional; default false)
  - `follow_symlinks` (boolean, optional; default false)
- Returns: Array of file plists `{path, size, mtime, type}`
- Errors: `file-error`
- Safety: Respect ignore patterns; avoid huge traversals; enforce caps.
- Mapping: `forj-list-project-files`
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-141-file-reader-system-priority-p0]

### 4.3 search (Claude Code-compatible)

- Intent: Project-wide text search with regex support and path filters.
- Args:
  - `query` (string, required)
  - `is_regex` (boolean, optional; default false)
  - `case_sensitive` (boolean, optional; default false)
  - `include_paths` (array<string>, optional)
  - `exclude_paths` (array<string>, optional)
  - `max_results` (integer, optional; default 200)
- Returns: Array of matches
  `{path, line, column, match_text, before, after}`
- Errors: `validation-error`
- Safety: Enforce caps; skip binary/large files; respect ignore patterns.
- Mapping: New function `forj-search` using Emacs primitives or
  external `ripgrep` if configured.

### 4.4 write_file (Claude Code-compatible)

- Intent: Safe write with backup and atomic persistence; supports append
  and dry-run to mirror Claude Code write semantics.
- Args:
  - `path` (string, required)
  - `content` (string, required)
  - `no_backup` (boolean, optional; default false)
  - `append` (boolean, optional; default false)
  - `create_if_missing` (boolean, optional; default true)
  - `dry_run` (boolean, optional; default false)
- Returns: `{path, backup_path, bytes_written, dry_run, preview?}`
- Errors: `validation-error`, `file-error`
 - Safety: Backups, atomic write, `.el` validation via `forj-paren-check`;
  no writes when `dry_run` is true; present preview summary. Requires
  explicit user approval when `dry_run` is false. Paths must be within
  the project root.
- Mapping: `forj-write-file`
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-142-file-writer-system-priority-p0]

### 4.5 edit_region (Claude Code-compatible)

- Intent: Replace a line range in a file with optional optimistic
  concurrency hints, akin to Claude Code’s EditBlock.
- Args:
  - `path` (string, required)
  - `start_line` (integer, required, 1-based)
  - `end_line` (integer, required, inclusive)
  - `new_content` (string, required)
  - `expected` (string, optional; original text to match)
  - `dry_run` (boolean, optional; default false)
- Returns: `{path, start_line, end_line, backup_path, dry_run, changed}`
- Errors: `validation-error`, `file-error`
 - Safety: Backup; validate `.el` on save; if `expected` provided and
  mismatch occurs, abort and return a clear error with a small diff.
  Requires explicit user approval when `dry_run` is false. Paths must be
  within the project root.
- Mapping: `forj-edit-file-region`
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-142-file-writer-system-priority-p0]

### 4.6 run_shell (Claude Code-compatible, safe)

- Intent: Run a local shell command with timeouts and environment vars.
- Args:
  - `cmd` (array<string>, required)
  - `cwd` (string, optional)
  - `timeout` (integer, optional; seconds)
  - `env` (object, optional; key/value strings)
  - `stdin` (string, optional)
- Returns: `{exit_code, stdout, stderr, duration_ms}`
- Errors: `process-error`, `validation-error`
 - Safety: No network by default; redact secrets; enforce timeouts and
  output caps; prompt before destructive commands when interactive.
  Requires explicit user approval for potentially destructive commands
  (e.g., modifying filesystem/process state). `cwd` must be the project
  root or a subdirectory.
- Mapping: New `forj-run-shell` using `call-process`/`make-process`.
  [Source: docs/architecture.md#3-high-level-architecture]

### 4.7 run_tests (Claude Code-compatible, ERT)

- Intent: Run tests and return structured results; defaults to ERT but
  allows custom command when configured.
- Args:
  - `selector` (string, optional; ERT selector or regex)
  - `command` (array<string>, optional; alternate test runner)
- Returns: `{total, passed, failed, errors, skipped, report}`
- Errors: `validation-error`, `process-error`
- Safety: Load test files in batch; cap log sizes.
- Mapping: New `forj-run-tests` (batch Emacs invocation) or in-proc
  ERT run depending on user config.
  [Source: docs/architecture.md#6-cross-cutting-concerns]

### 4.8 multi_edit (Claude Code-compatible)

- Intent: Apply multiple surgical edits, optionally with per-edit
  optimistic concurrency hints.
- Args:
  - `edits` (array, required) items:
    `{path, start_line, end_line, new_content, expected?}`
  - `stop_on_error` (boolean, optional; default true)
  - `dry_run` (boolean, optional; default false)
- Returns: `{applied: integer, skipped: integer, dry_run, details:
  [ {path, start_line, end_line, backup_path?, changed, error?} ]}`
- Errors: `validation-error`, `file-error`
 - Safety: Per-file backups; `.el` validation after edits; rollback on
  failure; respect `stop_on_error` for best-effort. Requires explicit
  user approval when `dry_run` is false. All paths must be within the
  project root.
- Mapping: New `forj-edit-multiple-regions` that iterates `forj-edit-file-region`
  with transactional semantics per file.

### 4.9 glob (Claude Code-compatible)

- Intent: Expand glob patterns with IDE-like ignore behavior.
- Args:
  - `patterns` (array<string>, required)
  - `max_results` (integer, optional; default 1000)
  - `include_hidden` (boolean, optional; default false)
- Returns: `{files: [ {path, size, mtime, type} ]}`
- Errors: `validation-error`
- Safety: Apply ignore patterns (e.g., `.git`, `node_modules`, binaries),
  cap results, keep results inside project root.
- Mapping: New `forj-glob` using `file-expand-wildcards` plus filters; or
  integrate with `forj-list-project-files` for metadata consistency.

### 4.10 todo_write (Claude Code-compatible)

- Intent: Create and manage lightweight TODO tasks during coding sessions
  without modifying source files by default (Claude Code TodoWrite parity).
- Capabilities:
  - Create tasks with title and optional note
  - Link to code location (`path`, `line`/`range`) for quick navigation
  - Update status: complete, reopen; edit title/note/labels
  - List and filter tasks by status/label/file
  - Optional export to file (e.g., `docs/TODO.md`) on explicit request
- Args:
  - `action` (string, required; one of `create`, `update`, `complete`,
    `reopen`, `delete`, `list`, `export`)
  - `title` (string, required for `create`)
  - `note` (string, optional)
  - `id` (string, optional; required for `update`/`complete`/`reopen`/`delete`)
  - `location` (object, optional) `{path: string, line: int, end_line: int?}`
  - `labels` (array<string>, optional)
  - `filter` (object, optional; for `list`) `{status?: string, label?: string, path?: string}`
  - `export_path` (string, optional; for `export`, default `docs/TODO.md`)
- Returns:
  - For `create`/`update`/`complete`/`reopen`: `{task}`
  - For `delete`: `{deleted: true, id}`
  - For `list`: `{tasks: [task]}`
  - For `export`: `{path, count}`
- Task shape:
  - `{id, title, note, status: "open"|"completed", location?, labels?, created_at, updated_at}`
- Errors: `validation-error` (bad args), `file-error` (on export only)
- Safety:
  - No file writes unless `action=export`
  - Validate `location.path` exists before navigation
  - Redact secrets in notes on export
  - `export` requires explicit user approval; `export_path` must be
    within the project root
- Mapping: New `forj-todo-write` module maintaining an in-memory task
  store with optional export to file; UI renders a Tasks panel and links.

### 4.11 task (Claude Code-compatible)

- Intent: Manage higher-level work items with lifecycle, subtasks,
  acceptance criteria, labels, priority, and links. Non-destructive by
  default; optional export to roadmap/TODO files.
- Args:
  - `action` (string, required; `create`|`update`|`set_status`|
    `complete`|`reopen`|`add_subtask`|`update_subtask`|
    `add_note`|`list`|`delete`|`export`)
  - `title` (string, required for `create`)
  - `description` (string, optional)
  - `acceptance` (array<string>, optional)
  - `links` (array<object>, optional; `{path, line?, end_line?}`)
  - `labels` (array<string>, optional)
  - `priority` (string, optional; `low`|`medium`|`high`|`critical`)
  - `id` (string, optional; required for updates)
  - `status` (string, for `set_status`; `open`|`in_progress`|`blocked`|`completed`)
  - `subtask` (object, for subtask ops; `{id?, title?, done?}`)
  - `note` (string, for `add_note`)
  - `filter` (object, for `list`; `{status?, label?, path?}`)
  - `export_path` (string, optional)
- Returns:
  - For create/update/set_status/complete/reopen/add_subtask/update_subtask/add_note:
    `{task}`
  - For list: `{tasks: [task]}`
  - For delete: `{deleted: true, id}`
  - For export: `{path, count}`
- Task shape: `{id, title, description?, acceptance?: [string],
  subtasks?: [{id, title, done}], status, labels?, priority?, links?,
  created_at, updated_at, history?: [note]}`
- Errors: `validation-error`, `file-error` (on export only)
 - Safety: No code edits; validate links before navigation; redact
  secrets on export. `export` requires explicit user approval; export
  path must be within the project root.
- Mapping: `forj-task` in-memory store with optional export to
  `docs/roadmap.md`/`docs/TODO.md` using safe write flow.

## 5. Safety, Validation, and Errors

- Validation: All `.el` content from write/edit tools must pass
  `forj-paren-check` before saving. On failure, return
  `validation-error` with checker details and do not persist.
  [Source: docs/forj-paren-checker.md:1]

- Backups and Atomicity: Always create timestamped backups and use
  atomic rename for writes/edits. Provide `backup_path` in results.
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-142-file-writer-system-priority-p0]

- Centralized Errors: Use `forj-error-system.el` constructors to
  produce dual-format errors with `:context`, `:details`, `:recovery`.
  [Source: docs/architecture.md#6-cross-cutting-concerns]

- Read-only and Git Awareness: Respect `buffer-read-only` and warn on
  uncommitted changes; optionally auto-stage on success (configurable).
  [Source: docs/phase-1-4-file-operations-workflow.md#sub-phase-142-file-writer-system-priority-p0]

- Project Sandbox (Mandatory): All file paths and operations are
  restricted to the current project root. Any attempt to read/write
  outside the workspace is rejected with `validation-error`.

- Approval Gate for Destructive Actions (Mandatory): Any destructive or
  mutating operation requires explicit user approval before execution.
  This includes: `write_file` (when not `dry_run`), `edit_region`
  (when not `dry_run`), `multi_edit` (when not `dry_run`), `run_shell`
  (when command is classified as destructive), `todo_write export`, and
  `task export`. The UI presents a summary/diff preview and the action is
  only applied after approval.

- Dry-Run First: For write/edit tools, a preflight `dry_run` is
  recommended to generate previews used in the approval dialog. Tools do
  nothing irreversible when `dry_run` is true.

## 6. UI and Experience

- Conversation Log: Render a compact header per tool call and attach a
  collapsible JSON result. Provide quick actions (open file, jump to
  line).

- Interactive Prompts: For destructive shell commands or edits outside
  the project root, prompt with clear diff/summary and recovery steps.

- Streaming: For long commands, stream partial output into a dedicated
  buffer and summarize into the final `tool-result`.

## 7. Implementation Plan

1) Create `forj-tools.el` with:
   - Registry: `(forj-tools-register name fn arg-spec)`
   - Dispatcher: `(forj-tools-dispatch json)` → result/error plist
   - Helpers: parsing, redaction, size caps, timeouts

 2) Implement core tools mapping to existing file ops:
   - `read_file` → `forj-read-file`
   - `list_files` → `forj-list-project-files`
   - `write_file` → `forj-write-file`
   - `edit_region` → `forj-edit-file-region`
   - `search` → `forj-search`
   - `run_shell` → `forj-run-shell`
   - `run_tests` → `forj-run-tests`
   - `multi_edit` → `forj-edit-multiple-regions`
   - `glob` → `forj-glob`
   - `todo_write` → `forj-todo-write`
   - `task` → `forj-task`

3) Wire into orchestrator:
   - Parse ```tool-call blocks in AI messages
   - Execute via dispatcher and append ```tool-result
   - Guardrails: size limits, timeouts, confirmation flows
   - Approval Gate: For destructive actions, show diff/summary and block
     until the user explicitly approves
   - Project Sandbox: Normalize and validate all paths/cwd to ensure
     they’re within the project root before calling any tool

4) Surface UX in `forj-ui`:
   - Clickable headers, open-file actions, jump-to-line
   - Stream buffers for long-running commands

## 8. Testing Strategy

- ERT unit tests per tool function (happy path + errors):
  - `forj-test-tools-read-file-basic`
  - `forj-test-tools-write-file-validation-fails`
  - `forj-test-tools-edit-region-bounds`
  - `forj-test-tools-search-matches-capped`
  - `forj-test-tools-run-shell-timeout`
  - `forj-test-tools-run-tests-selector`

- Dispatcher tests:
  - Rejects malformed JSON / missing fields
  - Routes to correct function with validated args
  - Produces machine-readable error contracts

- Integration tests (conversation loop):
  - Model emits a `tool-call`; Forj returns `tool-result`; model uses it

## 9. Non-Goals (MVP)

- Git write operations (stage/commit/revert); read-only status only
- Network-enabled shell commands
- Patch-application tools; focus on region edits for determinism
- Multi-call batches; one call per block keeps flow simple

## 10. Project Structure Notes

- New file: `forj-tools.el` alongside `forj-api.el` and orchestrator
  modules.
- Registry-only exposure; internal functions use `forj--` prefix.
- Customization group entries for tool caps (timeouts, size limits).
  [Source: docs/architecture.md#6-cross-cutting-concerns]

## Appendix A: Tool Schemas

### read_file (Claude Code-compatible)

Args:
```
{"path": string, "max_bytes": integer?, "start_line": integer?,
 "end_line": integer?, "include_context": integer?}
```

Result:
```
{"path": string, "encoding": string, "content": string,
 "truncated": boolean, "size": integer, "range": {"start_line": integer,
 "end_line": integer}?}
```

### list_files (Claude Code-compatible)

Args:
```
{"directory": string?, "max_depth": integer?, "include_hidden": boolean?,
 "follow_symlinks": boolean?}
```

Result:
```
[{"path": string, "size": integer, "mtime": integer, "type": string}]
```

### search (Claude Code-compatible)

Args:
```
{"query": string, "is_regex": boolean?, "case_sensitive": boolean?,
 "include_paths": [string]?, "exclude_paths": [string]?,
 "max_results": integer?}
```

Result:
```
[{"path": string, "line": integer, "column": integer,
  "match_text": string, "before": string, "after": string}]
```

### write_file (Claude Code-compatible)

Args:
```
{"path": string, "content": string, "no_backup": boolean?,
 "append": boolean?, "create_if_missing": boolean?, "dry_run": boolean?}
```

Result:
```
{"path": string, "backup_path": string?, "bytes_written": integer,
 "dry_run": boolean, "preview": string?}
```

### edit_region (Claude Code-compatible)

Args:
```
{"path": string, "start_line": integer, "end_line": integer,
 "new_content": string, "expected": string?, "dry_run": boolean?}
```

Result:
```
{"path": string, "start_line": integer, "end_line": integer,
 "backup_path": string?, "dry_run": boolean, "changed": boolean}
```

### run_shell (Claude Code-compatible)

Args:
```
{"cmd": [string], "cwd": string?, "timeout": integer?,
 "env": {string: string}?, "stdin": string?}
```

Result:
```
{"exit_code": integer, "stdout": string, "stderr": string,
 "duration_ms": integer}
```

### run_tests (Claude Code-compatible)

Args:
```
{"selector": string?, "command": [string]?}
```

Result:
```
 {"total": integer, "passed": integer, "failed": integer,
 "errors": integer, "skipped": integer, "report": string}
 ```

### multi_edit (Claude Code-compatible)

Args:
```
{"edits": [{"path": string, "start_line": integer,
            "end_line": integer, "new_content": string,
            "expected": string?}],
 "stop_on_error": boolean?, "dry_run": boolean?}
```

Result:
```
{"applied": integer, "skipped": integer, "dry_run": boolean,
 "details": [{"path": string, "start_line": integer,
               "end_line": integer, "backup_path": string?,
               "changed": boolean, "error": string?}]}
```

### glob (Claude Code-compatible)

Args:
```
{"patterns": [string], "max_results": integer?, "include_hidden": boolean?}
```

Result:
```
{"files": [{"path": string, "size": integer,
            "mtime": integer, "type": string}]}
```

### todo_write (Claude Code-compatible)

Args:
```
{"action": "create"|"update"|"complete"|"reopen"|"delete"|"list"|"export",
 "title": string?, "note": string?, "id": string?,
 "location": {"path": string, "line": integer, "end_line": integer?}?,
 "labels": [string]?,
 "filter": {"status": string?, "label": string?, "path": string?}?,
 "export_path": string?}
```

Result:
```
// create/update/complete/reopen
{"task": {"id": string, "title": string, "note": string?,
          "status": "open"|"completed",
          "location": {"path": string, "line": integer, "end_line": integer?}?,
          "labels": [string]?,
          "created_at": integer, "updated_at": integer}}

// delete
{"deleted": true, "id": string}

// list
{"tasks": [{"id": string, "title": string, "status": string,
            "location": {"path": string, "line": integer}?,
            "labels": [string]?}]}

// export
{"path": string, "count": integer}
```

### task (Claude Code-compatible)

Args:
```
{"action": "create"|"update"|"set_status"|"complete"|"reopen"|
 "add_subtask"|"update_subtask"|"add_note"|"list"|"delete"|"export",
 "title": string?, "description": string?, "acceptance": [string]?,
 "links": [{"path": string, "line": integer?, "end_line": integer?}]?,
 "labels": [string]?, "priority": "low"|"medium"|"high"|"critical"?,
 "id": string?, "status": "open"|"in_progress"|"blocked"|"completed"?,
 "subtask": {"id": string?, "title": string?, "done": boolean?}?,
 "note": string?, "filter": {"status": string?, "label": string?, "path": string?}?,
 "export_path": string?}
```

Result:
```
// returns either a task, a list, a deletion ack, or an export summary as described above
```
