#!/bin/bash
# execute-on-emacsclient.sh - Enhanced emacsclient wrapper for Forj development

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

# Default configuration
SOCKET=""
TIMEOUT=30
VERBOSE=false
DEBUG=false
AUTO_FORMAT=true

show_help() {
    cat << EOF
${BOLD}📡 Execute on Emacsclient${NC}

A wrapper around emacsclient with automatic socket discovery, error handling,
and Forj-specific convenience functions.

${BOLD}Usage:${NC}
    $0 [options] [command] [args...]
    $0 [options] -e "elisp-expression"

${BOLD}Options:${NC}
    -s, --socket PATH       Specify socket path (auto-detected if not provided)
    -t, --timeout SECONDS   Timeout for operations (default: 30)
    -v, --verbose          Verbose output
    -d, --debug            Debug mode with detailed logging
    -q, --quiet            Suppress socket discovery messages
    -f, --format           Auto-format JSON output (default: on)
    -r, --raw              Raw output, no formatting
    -h, --help             Show this help

${BOLD}Built-in Commands:${NC}
    discover               Discover and test Emacs socket
    status                 Check Emacs and Forj status
    eval "expression"      Evaluate Elisp expression
    call function args     Call Elisp function with arguments
    buffer name [range]    Get buffer contents
    forj-start            Start Forj system
    forj-query "text"     Submit query to Forj
    forj-tool name args   Execute Forj tool directly
    forj-reload           Reload Forj modules

${BOLD}Examples:${NC}
    $0 discover                              # Find Emacs socket
    $0 status                                # Check system status
    $0 eval "(+ 1 2)"                       # Evaluate expression
    $0 call forj-tools-list                 # Call function
    $0 buffer "*forj*" -200                 # Get last 200 chars of buffer
    $0 forj-query "what directory am I in?" # Submit Forj query
    $0 forj-tool get_current_directory '{}'  # Execute tool directly

${BOLD}Environment:${NC}
    FORJ_SOCKET           Override socket path
    FORJ_EMACS_TIMEOUT    Override timeout (seconds)
    FORJ_DEBUG            Enable debug mode (any non-empty value)

EOF
}

log_debug() {
    if $DEBUG; then
        echo -e "${BLUE}[DEBUG]${NC} $*" >&2
    fi
}

log_verbose() {
    if $VERBOSE || $DEBUG; then
        echo -e "${YELLOW}[INFO]${NC} $*" >&2
    fi
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" >&2
}

# Discover Emacs server socket
discover_socket() {
    if [ -n "$SOCKET" ]; then
        log_debug "Using provided socket: $SOCKET"
        return 0
    fi

    if [ -n "$FORJ_SOCKET" ]; then
        SOCKET="$FORJ_SOCKET"
        log_debug "Using FORJ_SOCKET: $SOCKET"
        return 0
    fi

    # Load from .test-env if available
    if [ -f "$PROJECT_ROOT/.test-env" ]; then
        source "$PROJECT_ROOT/.test-env"
        if [ -n "$FORJ_SOCKET" ]; then
            SOCKET="$FORJ_SOCKET"
            log_debug "Loaded socket from .test-env: $SOCKET"
            return 0
        fi
    fi

    # Auto-discover socket
    log_verbose "Auto-discovering Emacs server socket..."
    local found_socket=$(find /var/folders -name server 2>/dev/null | head -1)
    
    if [ -z "$found_socket" ]; then
        log_error "No Emacs server socket found"
        echo "Start Emacs with: (server-start)" >&2
        return 1
    fi

    SOCKET="$found_socket"
    log_verbose "Discovered socket: $SOCKET"
    return 0
}

# Test socket connectivity
test_socket() {
    log_debug "Testing socket connectivity: $SOCKET"
    
    if ! timeout "$TIMEOUT" emacsclient --socket-name "$SOCKET" -e '(+ 1 2)' >/dev/null 2>&1; then
        log_error "Cannot connect to Emacs server"
        log_error "Socket: $SOCKET"
        return 1
    fi
    
    log_debug "Socket connection successful"
    return 0
}

# Execute emacsclient with error handling
execute_emacsclient() {
    local expression="$1"
    
    log_debug "Executing: $expression"
    
    if ! discover_socket; then
        return 1
    fi
    
    if ! test_socket; then
        return 1
    fi
    
    local result
    if ! result=$(timeout "$TIMEOUT" emacsclient --socket-name "$SOCKET" -e "$expression" 2>&1); then
        log_error "Emacsclient execution failed"
        log_error "Expression: $expression"
        log_error "Result: $result"
        return 1
    fi
    
    # Format JSON output if requested and detected
    if $AUTO_FORMAT && echo "$result" | grep -q '^{.*}$\|^\[.*\]$'; then
        if command -v python3 >/dev/null 2>&1; then
            echo "$result" | python3 -m json.tool 2>/dev/null || echo "$result"
        elif command -v jq >/dev/null 2>&1; then
            echo "$result" | jq . 2>/dev/null || echo "$result"
        else
            echo "$result"
        fi
    else
        echo "$result"
    fi
    
    return 0
}

# Built-in command: discover
cmd_discover() {
    if discover_socket; then
        echo "Socket: $SOCKET"
        if test_socket; then
            log_success "Emacs server connection successful"
            
            # Test Forj availability
            if execute_emacsclient "(fboundp 'forj-start)" | grep -q "t"; then
                log_success "Forj system available"
            else
                echo -e "${YELLOW}⚠️ Forj system not loaded${NC}"
            fi
        fi
    else
        return 1
    fi
}

# Built-in command: status
cmd_status() {
    if ! discover_socket || ! test_socket; then
        return 1
    fi
    
    echo -e "${BOLD}📊 Emacs & Forj Status${NC}"
    echo "Socket: $SOCKET"
    echo ""
    
    # Emacs version
    echo -n "Emacs version: "
    execute_emacsclient "(emacs-version)" | sed 's/"//g'
    
    # Current directory
    echo -n "Working directory: "
    execute_emacsclient "(pwd)" | sed 's/"//g'
    
    # Forj status
    echo ""
    echo -e "${BOLD}Forj System:${NC}"
    
    local forj_functions=(
        "forj-start"
        "forj-tools-dispatch"
        "forj-tools-list"
        "forj-check-for-tool-trigger"
    )
    
    for func in "${forj_functions[@]}"; do
        echo -n "  $func: "
        if execute_emacsclient "(fboundp '$func)" | grep -q "t"; then
            echo -e "${GREEN}✅${NC}"
        else
            echo -e "${RED}❌${NC}"
        fi
    done
    
    # Tool count
    echo -n "  Registered tools: "
    local tool_count=$(execute_emacsclient "(length (forj-tools-list))" 2>/dev/null || echo "0")
    echo "$tool_count"
    
    # Active buffers
    echo ""
    echo -e "${BOLD}Active Buffers:${NC}"
    if execute_emacsclient "(get-buffer \"*forj*\")" | grep -q "buffer"; then
        echo -e "  *forj*: ${GREEN}✅ Active${NC}"
    else
        echo -e "  *forj*: ${YELLOW}⚠️ Not active${NC}"
    fi
}

# Built-in command: eval
cmd_eval() {
    local expression="$1"
    if [ -z "$expression" ]; then
        log_error "Expression required"
        echo "Usage: $0 eval \"(+ 1 2)\""
        return 1
    fi
    
    execute_emacsclient "$expression"
}

# Built-in command: call
cmd_call() {
    local function_name="$1"
    shift
    
    if [ -z "$function_name" ]; then
        log_error "Function name required"
        echo "Usage: $0 call function-name [arg1] [arg2] ..."
        return 1
    fi
    
    # Build function call expression
    local expression="($function_name"
    for arg in "$@"; do
        if [[ "$arg" =~ ^[0-9]+$ ]]; then
            expression="$expression $arg"
        elif [[ "$arg" =~ ^[0-9]*\.[0-9]+$ ]]; then
            expression="$expression $arg"
        elif [ "$arg" = "t" ] || [ "$arg" = "nil" ]; then
            expression="$expression $arg"
        else
            expression="$expression \"$arg\""
        fi
    done
    expression="$expression)"
    
    log_debug "Built function call: $expression"
    execute_emacsclient "$expression"
}

# Built-in command: buffer
cmd_buffer() {
    local buffer_name="$1"
    local range="$2"
    
    if [ -z "$buffer_name" ]; then
        log_error "Buffer name required"
        echo "Usage: $0 buffer buffer-name [offset]"
        return 1
    fi
    
    local expression
    if [ -n "$range" ] && [[ "$range" =~ ^-?[0-9]+$ ]]; then
        if [ "$range" -lt 0 ]; then
            # Negative range: last N characters
            expression="(with-current-buffer \"$buffer_name\" (buffer-substring-no-properties (max (point-min) (+ (point-max) $range)) (point-max)))"
        else
            # Positive range: first N characters
            expression="(with-current-buffer \"$buffer_name\" (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) $range))))"
        fi
    else
        # Entire buffer
        expression="(with-current-buffer \"$buffer_name\" (buffer-substring-no-properties (point-min) (point-max)))"
    fi
    
    execute_emacsclient "$expression"
}

# Built-in command: forj-start
cmd_forj_start() {
    log_verbose "Starting Forj system..."
    execute_emacsclient "(forj-start)"
    log_success "Forj system started"
}

# Built-in command: forj-query
cmd_forj_query() {
    local query="$1"
    if [ -z "$query" ]; then
        log_error "Query required"
        echo "Usage: $0 forj-query \"What directory am I in?\""
        return 1
    fi
    
    log_verbose "Submitting query: $query"
    
    # Submit query and wait for response
    execute_emacsclient "(let ((prompt-text \"$query\")) (with-current-buffer (get-buffer-create \"*forj-prompt*\") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))" >/dev/null
    
    # Wait a moment for processing
    sleep 3
    
    # Get response from forj buffer
    echo -e "${BOLD}Query:${NC} $query"
    echo -e "${BOLD}Response:${NC}"
    echo "----------"
    cmd_buffer "*forj*" -1000 | sed 's/\\n/\n/g' | tail -20
}

# Built-in command: forj-tool
cmd_forj_tool() {
    local tool_name="$1"
    local args="$2"
    
    if [ -z "$tool_name" ]; then
        log_error "Tool name required"
        echo "Usage: $0 forj-tool tool-name '{\"arg\": \"value\"}'"
        return 1
    fi
    
    if [ -z "$args" ]; then
        args="{}"
    fi
    
    log_verbose "Executing tool: $tool_name with args: $args"
    
    local json_call="{\\\"id\\\": \\\"script-test\\\", \\\"name\\\": \\\"$tool_name\\\", \\\"args\\\": $args, \\\"meta\\\": {}}"
    execute_emacsclient "(forj-tools-dispatch \"$json_call\")"
}

# Built-in command: forj-reload
cmd_forj_reload() {
    log_verbose "Reloading Forj modules..."
    
    local modules=("forj-tools.el" "forj-api.el" "forj.el")
    
    for module in "${modules[@]}"; do
        if [ -f "$PROJECT_ROOT/$module" ]; then
            log_verbose "Loading $module..."
            if ! execute_emacsclient "(load-file \"$module\")" >/dev/null; then
                log_error "Failed to load $module"
                return 1
            fi
        fi
    done
    
    # Restart forj
    execute_emacsclient "(progn (ignore-errors (kill-buffer \"*forj*\")) (forj-start))" >/dev/null
    log_success "Forj modules reloaded and restarted"
}

# Parse command line arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -s|--socket)
                SOCKET="$2"
                shift 2
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -d|--debug)
                DEBUG=true
                VERBOSE=true
                shift
                ;;
            -q|--quiet)
                VERBOSE=false
                DEBUG=false
                shift
                ;;
            -f|--format)
                AUTO_FORMAT=true
                shift
                ;;
            -r|--raw)
                AUTO_FORMAT=false
                shift
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            -e)
                # Direct elisp expression
                execute_emacsclient "$2"
                exit $?
                ;;
            discover)
                shift
                cmd_discover "$@"
                exit $?
                ;;
            status)
                shift
                cmd_status "$@"
                exit $?
                ;;
            eval)
                shift
                cmd_eval "$@"
                exit $?
                ;;
            call)
                shift
                cmd_call "$@"
                exit $?
                ;;
            buffer)
                shift
                cmd_buffer "$@"
                exit $?
                ;;
            forj-start)
                shift
                cmd_forj_start "$@"
                exit $?
                ;;
            forj-query)
                shift
                cmd_forj_query "$@"
                exit $?
                ;;
            forj-tool)
                shift
                cmd_forj_tool "$@"
                exit $?
                ;;
            forj-reload)
                shift
                cmd_forj_reload "$@"
                exit $?
                ;;
            *)
                # Unknown argument - pass through to emacsclient
                break
                ;;
        esac
    done
    
    # If we get here, pass remaining arguments to emacsclient
    if [ $# -eq 0 ]; then
        show_help
        exit 0
    fi
    
    # Pass through to original emacsclient
    if ! discover_socket || ! test_socket; then
        exit 1
    fi
    
    exec emacsclient --socket-name "$SOCKET" "$@"
}

# Main execution
main() {
    cd "$PROJECT_ROOT"
    
    # Load environment variables
    if [ -n "$FORJ_EMACS_TIMEOUT" ]; then
        TIMEOUT="$FORJ_EMACS_TIMEOUT"
    fi
    
    if [ -n "$FORJ_DEBUG" ]; then
        DEBUG=true
        VERBOSE=true
    fi
    
    parse_arguments "$@"
}

main "$@"