#!/usr/bin/env bash
set -euo pipefail

# forj-query.sh — Run a Forj query via emacsclient (TCP)
#
# Atomic CLI helper for submitting a natural-language query to a running
# Emacs server hosting Forj, and retrieving the conversation buffer.
#
# Dependencies: emacsclient; a running Emacs server (socket/server-file/host:port);
# project directory containing forj.el.
#
# Usage:
#   scripts/forj-query.sh "Query text" \
#     [--socket /path/to/server | --server-file /path/to/server | --host HOST --port PORT] \
#     [--project-dir /abs/path/to/project] [--timeout 8] [--restart] [--session-id id] \
#     [--strict] [--expect-tool | --expect-answer] [--approve] [--dry-run-only] \
#     [--json] [--raw] [--quiet] [--verbose] \
#     [--pre-elisp "(setq var t)"] [--post-elisp "(message \"done\")"]
#
# Options summary:
#   --socket|--server-file|--host/--port  Target Emacs server connection
#   --project-dir PATH                    Repo root containing forj.el
#   --timeout SECONDS                     Max wait time (default 6)
#   --restart                             Kill *forj* and reload forj.el
#   --session-id ID                       Use isolated buffer *forj-ID*
#   --strict                              Enable assertion mode
#   --expect-tool|--expect-answer         Expected outcome in strict mode
#   --approve                             Best-effort: disable approval gate
#   --dry-run-only                        Best-effort: force dry-run
#   --json                                Emit minimal JSON summary
#   --raw                                 Print full buffer content
#   --quiet|--verbose                     Control log verbosity
#   --pre-elisp/--post-elisp FORM         Eval Elisp before/after run
#
# Exit codes:
#   0  success; 2/3 strict expectation failed; >0 other errors
#
# Notes:
# - Credentials are not managed here; Emacs server environment is used as-is.
# - Script is self-contained and reusable across projects using Forj.

die() { echo "Error: $*" >&2; exit 1; }
need() { command -v "$1" >/dev/null 2>&1 || die "$1 not found in PATH"; }

# Defaults
QUERY=""
SOCKET="${FORJ_SOCKET:-}"
SERVER_FILE=""
HOST=""
PORT=""
PROJECT_DIR="${FORJ_PROJECT_DIR:-$(pwd)}"
TIMEOUT=6
RESTART=0
RAW=0
JSON_OUT=0
STRICT=0
EXPECT_TOOL=0
EXPECT_ANSWER=0
APPROVE=0
DRY_RUN_ONLY=0
QUIET=0
VERBOSE=0
SESSION_ID=""
PRE_ELISP=""
POST_ELISP=""

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    --socket) SOCKET="$2"; shift 2 ;;
    --server-file) SERVER_FILE="$2"; shift 2 ;;
    --host) HOST="$2"; shift 2 ;;
    --port) PORT="$2"; shift 2 ;;
    --project-dir) PROJECT_DIR="$2"; shift 2 ;;
    --timeout) TIMEOUT="$2"; shift 2 ;;
    --restart) RESTART=1; shift ;;
    --session-id) SESSION_ID="$2"; shift 2 ;;
    --strict) STRICT=1; shift ;;
    --expect-tool) EXPECT_TOOL=1; shift ;;
    --expect-answer) EXPECT_ANSWER=1; shift ;;
    --approve) APPROVE=1; shift ;;
    --dry-run-only) DRY_RUN_ONLY=1; shift ;;
    --json) JSON_OUT=1; shift ;;
    --raw) RAW=1; shift ;;
    --quiet) QUIET=1; shift ;;
    --verbose) VERBOSE=1; shift ;;
    --pre-elisp) PRE_ELISP="$2"; shift 2 ;;
    --post-elisp) POST_ELISP="$2"; shift 2 ;;
    --help|-h) sed -n '1,120p' "$0"; exit 0 ;;
    *)
      if [[ -z "$QUERY" ]]; then QUERY="$1"; shift; else die "Unexpected arg: $1"; fi ;;
  esac
done

[[ -n "$QUERY" ]] || die "Missing query string (positional)"
need emacsclient

# Resolve connection target
EC_ARGS=()
if [[ -n "$SOCKET" ]]; then
  [ -S "$SOCKET" ] || die "Server socket not found: $SOCKET"
  EC_ARGS=(--socket-name "$SOCKET")
elif [[ -n "$SERVER_FILE" ]]; then
  [ -e "$SERVER_FILE" ] || die "Server file not found: $SERVER_FILE"
  EC_ARGS=(--server-file "$SERVER_FILE")
elif [[ -n "$HOST" && -n "$PORT" ]]; then
  EC_ARGS=(--server-file "$HOST:$PORT")
else
  die "Missing connection: provide --socket or --server-file or --host/--port"
fi

# Helper to eval elisp
ec() {
  local form="$1"; shift || true
  if [[ "$VERBOSE" -eq 1 ]]; then echo "> emacsclient ${EC_ARGS[*]} -e '$form'" >&2; fi
  emacsclient "${EC_ARGS[@]}" -e "$form" "$@"
}

# Ensure project dir and forj.el exists
[ -d "$PROJECT_DIR" ] || die "Project dir not found: $PROJECT_DIR"
[ -f "$PROJECT_DIR/forj.el" ] || die "forj.el not found in project dir: $PROJECT_DIR"

# Check running Emacs server
if ! PING_OUT=$(ec '(emacs-pid)' 2>/dev/null); then
  die "No running Emacs server. Start Emacs with server enabled and try again."
fi
[[ "$PING_OUT" != "nil" ]] || die "Emacs server did not respond with a PID."

# Optional restart: clean conversation buffer and reload forj.el
if [[ "$RESTART" -eq 1 ]]; then
  ec "(ignore-errors (kill-buffer \"*forj*\"))" >/dev/null || true
  ec "(load-file \"$PROJECT_DIR/forj.el\")" >/dev/null
fi

# Project safety: ensure repo root (if git present)
if command -v git >/dev/null 2>&1; then
  if ROOT=$(git -C "$PROJECT_DIR" rev-parse --show-toplevel 2>/dev/null); then
    if [[ "$ROOT" != "$PROJECT_DIR" ]]; then
      die "--project-dir must be the repo root ($ROOT), got $PROJECT_DIR"
    fi
  fi
fi

# cd to project, optional session isolation
ec "(cd \"$PROJECT_DIR\")" >/dev/null
BUFNAME="*forj*"
if [[ -n "$SESSION_ID" ]]; then
  BUFNAME="*forj-$SESSION_ID*"
  ec "(when (boundp 'forj-conversation-buffer) (setq forj-conversation-buffer \"$BUFNAME\"))" >/dev/null || true
fi

# Preflight toggles and hooks
[[ -n "$PRE_ELISP" ]] && ec "$PRE_ELISP" >/dev/null || true
if [[ "$APPROVE" -eq 1 ]]; then
  ec "(when (boundp 'forj-tools-approve-destructive) (setq forj-tools-approve-destructive nil))" >/dev/null || true
fi
if [[ "$DRY_RUN_ONLY" -eq 1 ]]; then
  ec "(when (boundp 'forj-force-dry-run) (setq forj-force-dry-run t))" >/dev/null || true
fi

# Start Forj and verify required functions
ec '(forj-start)' >/dev/null
if [[ "$(ec "(and (fboundp 'forj-start) (fboundp 'forj-prompt-submit))")" != "t" ]]; then
  die "Missing required Forj functions (forj-start/forj-prompt-submit)."
fi

# Prepare prompt (base64 for safe quoting if available)
if command -v base64 >/dev/null 2>&1; then
  B64=$(printf %s "$QUERY" | base64 | tr -d '\n')
  PROMPT_FORM="(decode-coding-string (base64-decode-string \"$B64\") 'utf-8)"
else
  ESCQ=${QUERY//"/\"}
  PROMPT_FORM="\"$ESCQ\""
fi

# Submit the query
ec "(let ((prompt-text $PROMPT_FORM)) (with-current-buffer (get-buffer-create \"*forj-prompt*\") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))" >/dev/null

# Poll for results up to TIMEOUT seconds
SECONDS=0
HAS_TOOL_CALL=0
HAS_TOOL_RESULT=0
ERR_MARK=0
while (( SECONDS < TIMEOUT )); do
  BUF=$(ec "(when (get-buffer \"$BUFNAME\") (with-current-buffer \"$BUFNAME\" (buffer-substring-no-properties (point-min) (point-max))))" 2>/dev/null || true)
  [[ -n "$BUF" ]] || { sleep 0.25; continue; }
  if echo "$BUF" | grep -q "tool-call"; then HAS_TOOL_CALL=1; fi
  if echo "$BUF" | grep -q "tool-result"; then HAS_TOOL_RESULT=1; fi
  if echo "$BUF" | grep -qi "forj error\|error:"; then ERR_MARK=1; fi
  if [[ $HAS_TOOL_CALL -eq 1 || $HAS_TOOL_RESULT -eq 1 ]]; then break; fi
  sleep 0.25
done
TOTAL_S=$SECONDS

# Fetch final buffer
RESULT=$(ec "(with-current-buffer \"$BUFNAME\" (buffer-substring-no-properties (point-min) (point-max)))") || die "Failed to fetch $BUFNAME"

# Strict mode checks
rc=0
if [[ "$STRICT" -eq 1 ]]; then
  if [[ "$EXPECT_TOOL" -eq 1 && $HAS_TOOL_CALL -eq 0 ]]; then rc=2; fi
  if [[ "$EXPECT_ANSWER" -eq 1 && $HAS_TOOL_CALL -eq 1 ]]; then rc=3; fi
fi

# Output
if [[ "$JSON_OUT" -eq 1 ]]; then
  printf '{"query":%q,"project":%q,' "$QUERY" "$PROJECT_DIR"
  if [[ -n "$SOCKET" ]]; then printf '"connection":%q,' "socket:$SOCKET"; 
  elif [[ -n "$SERVER_FILE" ]]; then printf '"connection":%q,' "server-file:$SERVER_FILE"; 
  else printf '"connection":%q,' "$HOST:$PORT"; fi
  printf '"timings":{"total_s":%d},' "$TOTAL_S"
  printf '"status":{"tool_call":%s,"tool_result":%s,"errors":%s}}\n' \
    "$([[ $HAS_TOOL_CALL -eq 1 ]] && echo true || echo false)" \
    "$([[ $HAS_TOOL_RESULT -eq 1 ]] && echo true || echo false)" \
    "$([[ $ERR_MARK -eq 1 ]] && echo true || echo false)"
else
  [[ "$QUIET" -eq 1 ]] || {
    echo "Query: $QUERY"
    if [[ -n "$SOCKET" ]]; then echo "Socket: $SOCKET"; 
    elif [[ -n "$SERVER_FILE" ]]; then echo "Server-file: $SERVER_FILE"; 
    else echo "Host: $HOST Port: $PORT"; fi
    echo "Project: $PROJECT_DIR"
    echo "Timing: ${TOTAL_S}s"
    SUMMARY=""
    [[ $HAS_TOOL_CALL -eq 1 ]] && SUMMARY+="tool-call present; "
    [[ $HAS_TOOL_RESULT -eq 1 ]] && SUMMARY+="tool-result present; "
    [[ $ERR_MARK -eq 1 ]] && SUMMARY+="possible error; "
    echo "Summary: ${SUMMARY:-no markers found}"
  }
fi

if [[ "$RAW" -eq 1 ]]; then
  echo "----- $BUFNAME (raw Lisp string repr) -----"
  echo "$RESULT"
fi

# Post hook
[[ -n "$POST_ELISP" ]] && ec "$POST_ELISP" >/dev/null || true

exit "$rc"
