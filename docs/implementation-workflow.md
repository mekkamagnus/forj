# Detailed Implementation Workflow for Forj.el

## Task: Implement Core Agent Functionality

### Story: Read Current Buffer

**Persona**: Emacs Lisp Developer
**Estimated Time**: 2 hours
**Dependencies**: None
**MCP Context**: Emacs Lisp best practices for buffer manipulation.

#### Implementation Steps:

1.  **Define function `forj-get-buffer-content`** (1 hour)
    - Use `buffer-string` to get the entire content of the current buffer.
    - Add error handling for non-file buffers.
2.  **Integrate with `forj-prompt`** (1 hour)
    - Modify `forj-prompt` to call `forj-get-buffer-content`.
    - Pass the buffer content as part of the context to the LLM.

#### Acceptance Criteria:

- [ ] `forj-get-buffer-content` returns the full content of the active buffer.
- [ ] `M-x forj-prompt` sends the current buffer's content to the LLM.

### Story: Replace Region of Text

**Persona**: Emacs Lisp Developer
**Estimated Time**: 3 hours
**Dependencies**: None
**MCP Context**: Emacs Lisp text properties, undo handling.

#### Implementation Steps:

1.  **Define function `forj-replace-region`** (2 hours)
    - Take `start`, `end`, and `new-text` as arguments.
    - Use `delete-region` and `insert` to replace the text.
    - Wrap the operation in `with-undo-amalgamate` to ensure it's a single undo step.
2.  **Handle Text Properties** (1 hour)
    - Ensure that text properties from `new-text` are preserved.

#### Acceptance Criteria:

- [ ] `forj-replace-region` correctly replaces text within a specified region.
- [ ] The replacement action can be undone with a single `undo` command.

---

## Task: Implement Shell Command Execution

### Story: Execute Shell Commands

**Persona**: Emacs Lisp Developer
**Estimated Time**: 4 hours
**Dependencies**: None
**MCP Context**: Emacs asynchronous process handling.

#### Implementation Steps:

1.  **Define function `forj-execute-shell-command`** (2 hours)
    - Use `start-process` to run a shell command asynchronously.
    - Create a dedicated buffer for the output.
2.  **Capture and Display Output** (2 hours)
    - Set up a process sentinel to handle the output.
    - Insert the command's stdout and stderr into the output buffer.
    - Ensure the output buffer is automatically displayed.

#### Acceptance Criteria:

- [ ] `forj-execute-shell-command` can run arbitrary shell commands.
- [ ] Command output is displayed in a separate buffer (e.g., `*forj-shell-output*`).

---

## Task: Implement Contextual Understanding

### Story: Read Multiple Files for Context

**Persona**: Emacs Lisp Developer
**Estimated Time**: 3 hours
**Dependencies**: None
**MCP Context**: File I/O operations in Emacs Lisp.

#### Implementation Steps:

1.  **Define function `forj-read-files-for-context`** (2 hours)
    - Take a list of file paths as input.
    - Use `find-file-noselect` and `buffer-string` for each file.
    - Concatenate the contents.
2.  **Integrate with `forj-prompt`** (1 hour)
    - Add an option to `forj-prompt` to specify additional files for context.
    - Include the content of these files in the LLM prompt.

#### Acceptance Criteria:

- [ ] `forj-read-files-for-context` can read and combine content from multiple files.
- [ ] The combined content can be passed to the LLM as context.
