# ADR 0002 — Claude Code /forj:query Integration

<!-- Metadata -->

**Title:** 0002 — Claude Code /forj:query Integration with Existing Emacs Instances  
**Status:** accepted  
**Date:** 2025-09-01  
**Authors:** Claude (Anthropic Assistant)

## Context

Claude Code needed a `/forj:query` command to enable users to submit natural language queries to the Forj system from within Claude Code. The original implementation required complex test environment setup and failed when no test configuration existed. Users wanted to query their existing, running Emacs instances with Forj loaded, not set up isolated test environments.

## Decision

Implement `/forj:query` as a simple wrapper around `./scripts/forj-scripts/forj.sh query` that automatically detects and connects to existing Emacs instances, submits queries directly to the Forj prompt system, and returns formatted responses.

## Consequences

**Positive:**
- Zero configuration required - works immediately with any running Emacs + Forj
- Simple, reliable workflow that matches user expectations
- Leverages existing emacsclient infrastructure for robust socket discovery
- Maintains separation of concerns - script handles connection, Forj handles processing
- Clean, readable output format suitable for Claude Code integration

**Negative:**
- Requires Emacs server to be enabled (`server-start`) 
- Depends on Forj being properly loaded in the target Emacs instance
- No isolation - queries run in user's actual Emacs environment

## Alternatives Considered

**Option A** — Complex TCP/socket management with manual configuration
- Pros: Full control over connection parameters
- Cons: Required extensive configuration, prone to connection failures

**Option B** — Test environment setup and isolation
- Pros: Clean test environment, reproducible results
- Cons: Failed when no test config existed, didn't work with user's actual Emacs

**Option C** — Direct emacsclient calls without wrapper (chosen)
- Pros: Simple, works with existing instances, no configuration
- Cons: Less control over error handling, dependent on socket availability

## Rationale

Users wanted to query their existing Emacs instances where they already have Forj configured and working. Creating isolated test environments was unnecessary complexity that didn't match the use case. The chosen approach prioritizes simplicity and user expectations over theoretical isolation benefits.

## Related ADRs

- 0001 — Native Emacs Lisp Architecture (established the foundation for emacsclient integration)

## References

- `.claude/commands/forj/query.md` — Command specification
- `SCRIPTS.md` — Script organization documentation  
- `scripts/forj-scripts/forj.sh` — Main implementation
- `scripts/emacsclient-core/execute-on-emacsclient.sh` — Underlying emacsclient wrapper

## Implementation Notes

1. Modified `interactive_query()` function in `forj.sh` to use emacsclient wrapper
2. Updated script to call `execute-on-emacsclient.sh forj-query` instead of test environment setup
3. Reorganized scripts into `forj-scripts/` and `emacsclient-core/` directories
4. Created comprehensive documentation for the simplified workflow
5. Verified functionality with directory, file listing, and search queries

## Rollback / Migration Plan

To revert this decision:
1. Restore original `interactive_query()` function that uses `forj-dev-workflow.sh test-query`
2. Update documentation to reflect test environment requirements
3. Ensure `.test-env` configuration is available for environment setup
4. Consider maintaining both approaches with a flag to choose between them

---

## Technical Details

**Implementation Path:**
- `Claude Code /forj:query "query"` 
- → `./scripts/forj-scripts/forj.sh query "query"`
- → `./scripts/emacsclient-core/execute-on-emacsclient.sh forj-query "query"`
- → Emacs `(forj-prompt-submit)` with query in `*forj-prompt*` buffer
- → Forj processes query internally (AI + tools as needed)
- → Response retrieved from `*forj*` conversation buffer

**Key Architectural Insight:** The script is purely an interface layer - it doesn't execute tools or process natural language. All intelligence remains within Forj itself, maintaining clean separation of concerns.