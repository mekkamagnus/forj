# Scripts Directory Structure

This document describes the organization of scripts in the Forj project.

## Directory Structure

```
scripts/
├── forj-scripts/           # Forj workflow and testing scripts
│   ├── forj.sh            # Main workflow orchestrator  
│   ├── forj-dev-workflow.sh
│   ├── forj-regression-tests.sh
│   ├── forj-test-setup.sh
│   ├── forj-test-tools.sh
│   └── forj-query.sh      # Standalone query script (advanced)
├── emacsclient-core/      # Enhanced emacsclient wrapper
│   └── execute-on-emacsclient.sh
├── check-parens.sh        # Parentheses validation
├── check-parens.ts
├── dev-load.sh           # Development loader
└── test.sh              # Basic test runner
```

## Main Entry Points

### forj.sh - Primary Interface
```bash
./scripts/forj-scripts/forj.sh [command] [options]

Commands:
  setup                 - Initial environment setup
  status                - Check system health  
  dev [action]          - Development workflows
  test [type]           - Testing workflows
  query "text"          - Execute natural language query
  tool [name] [args]    - Direct tool execution
  ci                    - Full CI/CD pipeline
  watch                 - File change monitoring
```

### Enhanced Emacsclient Wrapper
```bash
./scripts/emacsclient-core/execute-on-emacsclient.sh [command]

Built-in commands:
  discover              - Find and test Emacs socket
  status                - System status check
  eval "expression"     - Evaluate Elisp
  call function args    - Function calls
  forj-query "text"     - Submit Forj query
  forj-tool name args   - Execute Forj tool
  forj-reload           - Reload Forj modules
```

## Claude Code Integration

### /forj:query Command
The `/forj:query` command in Claude Code is implemented as:
```bash
./scripts/forj-scripts/forj.sh query "$1"
```

**Key Features**:
- **Works with existing Emacs**: Automatically detects and connects to running Emacs instances
- **No configuration required**: Automatic socket discovery and connection
- **Simple query interface**: Just submits natural language queries to Forj
- **Forj handles processing**: All tool execution and AI processing happens within Forj itself
- **Formatted output**: Clean, readable responses from Forj conversation
- **Robust error handling**: Clear diagnostics for connection and processing issues

**Workflow**:
1. Detects existing Emacs instance with Forj loaded
2. Submits query to Forj prompt system using emacsclient
3. Forj processes query internally (handles tools, AI, etc.)
4. Retrieves and formats response from Forj conversation buffer

## Usage Examples

```bash
# Basic query via forj.sh
./scripts/forj-scripts/forj.sh query "What directory am I in?"

# Development workflow
./scripts/forj-scripts/forj.sh dev reload
./scripts/forj-scripts/forj.sh test regression

# Direct emacsclient operations
./scripts/emacsclient-core/execute-on-emacsclient.sh discover
./scripts/emacsclient-core/execute-on-emacsclient.sh forj-query "List files"

# Advanced query with specific options
./scripts/forj-scripts/forj-query.sh "Complex query" --socket /path/to/socket
```

## Script Coordination

Scripts are designed to work together:
- `forj.sh` orchestrates workflows and calls other scripts
- Individual scripts handle specific functions
- All scripts use consistent error handling and logging
- Path references are relative to project root
- Socket discovery is automated where possible

## Development Notes

- All scripts changed directory to project root for consistent behavior
- Path calculations account for nested directory structure
- Environment variables loaded from `.test-env` when available
- Scripts can be run from any directory
- Comprehensive logging and error reporting throughout