#!/bin/bash

# test-forj-query-tcp.sh - Interactive TCP/emacsclient testing script for query interpreter
# Based on examples from CLAUDE.md for testing interactive features

set -euo pipefail

# Configuration
SOCKET_PATH=""
TEST_PROJECT_DIR="/tmp/forj-test-project"
TIMEOUT=10

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

# Function to find Emacs socket
find_emacs_socket() {
    log_info "Searching for Emacs server socket..."
    
    # Common socket locations
    local socket_dirs=(
        "/var/folders/*/T/emacs*/server"
        "/tmp/emacs*/server"
        "$HOME/.emacs.d/server/server"
    )
    
    for pattern in "${socket_dirs[@]}"; do
        for socket in $pattern; do
            if [[ -S "$socket" ]]; then
                echo "$socket"
                return 0
            fi
        done
    done
    
    return 1
}

# Function to run emacsclient commands
run_emacs() {
    if [[ -z "$SOCKET_PATH" ]]; then
        log_error "Socket path not set"
        return 1
    fi
    
    local cmd="$1"
    log_info "Running: $cmd"
    
    timeout "$TIMEOUT" emacsclient --socket-name "$SOCKET_PATH" -e "$cmd" 2>/dev/null || {
        log_error "Command failed or timed out: $cmd"
        return 1
    }
}

# Function to create test project
create_test_project() {
    log_info "Creating test project at $TEST_PROJECT_DIR"
    
    mkdir -p "$TEST_PROJECT_DIR"
    cd "$TEST_PROJECT_DIR"
    
    # Create test files
    cat > "test.el" << 'EOF'
;;; test.el --- Test file for query interpreter

(defun test-function ()
  "A test function for query interpretation testing."
  (message "Hello from test function"))

(defvar test-variable "test value"
  "A test variable.")

;; TODO: Add more test functions

(provide 'test)
;;; test.el ends here
EOF

    cat > "README.md" << 'EOF'
# Test Project

This is a test project for forj query interpreter validation.

## Features

- Query interpretation
- Tool execution
- Interactive testing

## TODO

- [ ] Test directory listing
- [ ] Test file reading
- [ ] Test search functionality
EOF

    cat > "notes.txt" << 'EOF'
This is a simple text file for testing.
It contains some sample content.
TODO: Add more comprehensive test data.
FIXME: This might need updating.
EOF

    mkdir -p "subdir"
    echo "File in subdirectory" > "subdir/subfile.txt"
    
    log_success "Test project created with files: test.el, README.md, notes.txt, subdir/subfile.txt"
}

# Function to setup test environment
setup_test_environment() {
    log_info "Setting up test environment in Emacs..."
    
    # Switch to test project directory
    run_emacs "(cd \"$TEST_PROJECT_DIR\")" || return 1
    
    # Kill existing forj buffer to start fresh
    run_emacs '(kill-buffer "*forj*")' 2>/dev/null || true
    
    # Load forj.el (assuming it's in the current working directory when script is run)
    local forj_path="$(pwd)/forj.el"
    run_emacs "(load-file \"$forj_path\")" || {
        log_error "Failed to load forj.el from $forj_path"
        return 1
    }
    
    # Start forj
    run_emacs '(forj-start)' || {
        log_error "Failed to start forj"
        return 1
    }
    
    log_success "Test environment setup complete"
    return 0
}

# Test query interpretation with natural language
test_query_interpretation() {
    local query="$1"
    local expected_pattern="$2"
    local test_name="$3"
    
    log_info "Testing query interpretation: $test_name"
    log_info "Query: '$query'"
    log_info "Expected pattern: '$expected_pattern'"
    
    # Submit query using the new query interpreter
    local submit_cmd="(let ((prompt-text \"$query\")) (when (fboundp 'forj-process-query-with-interpretation) (forj-process-query-with-interpretation prompt-text)))"
    
    if ! run_emacs "$submit_cmd"; then
        log_error "Failed to submit query: $query"
        return 1
    fi
    
    # Wait for processing
    sleep 3
    
    # Get conversation buffer contents
    local result
    result=$(run_emacs '(with-current-buffer "*forj*" (buffer-substring-no-properties (point-min) (point-max)))')
    
    if [[ -z "$result" ]]; then
        log_error "No response received for query: $query"
        return 1
    fi
    
    # Check if expected pattern is found
    if echo "$result" | grep -q "$expected_pattern"; then
        log_success "✅ PASS: $test_name"
        echo "Response contains expected pattern: $expected_pattern"
        return 0
    else
        log_error "❌ FAIL: $test_name"
        echo "Response does not contain expected pattern: $expected_pattern"
        echo "Actual response (last 500 chars):"
        echo "$result" | tail -c 500
        return 1
    fi
}

# Test fallback pattern matching
test_fallback_patterns() {
    log_info "Testing fallback pattern matching (AI disabled)"
    
    # Disable AI for predictable fallback behavior
    run_emacs '(setq forj-query-ai-enabled nil)' || {
        log_warning "Could not disable AI - tests may be unpredictable"
    }
    
    # Test directory listing patterns
    test_query_interpretation \
        "what's in this directory" \
        "tool-call\|list_files\|files" \
        "Directory listing query"
    
    # Test search patterns
    test_query_interpretation \
        "find TODO comments" \
        "tool-call\|search\|TODO" \
        "Search query"
    
    # Test file reading patterns
    test_query_interpretation \
        "read test.el file" \
        "tool-call\|read_file\|test.el" \
        "File reading query"
    
    # Re-enable AI
    run_emacs '(setq forj-query-ai-enabled t)' || {
        log_warning "Could not re-enable AI"
    }
}

# Test tool execution validation
test_tool_execution() {
    log_info "Testing tool execution through query interpreter"
    
    # Test that tools are actually executed (not just interpreted)
    test_query_interpretation \
        "list files in this directory" \
        "test.el\|README.md\|notes.txt" \
        "Tool execution with real results"
}

# Test error handling
test_error_handling() {
    log_info "Testing error handling and recovery"
    
    # Test nonsensical query
    test_query_interpretation \
        "purple monkey dishwasher quantum" \
        "interpretation\|fallback\|regular" \
        "Nonsensical query handling"
}

# Test AI integration (if available)
test_ai_integration() {
    log_info "Testing AI integration (if available)"
    
    # Check if AI is enabled and working
    local ai_check
    ai_check=$(run_emacs '(and (boundp '\''forj-query-ai-enabled) forj-query-ai-enabled (fboundp '\''forj-query--classify-intent) t)')
    
    if [[ "$ai_check" == "t" ]]; then
        log_info "AI query interpretation is available - testing..."
        
        # Test question classification
        test_query_interpretation \
            "what is machine learning" \
            "question\|answer\|AI" \
            "Question classification"
        
        # Test request classification
        test_query_interpretation \
            "show me project files" \
            "tool-call\|list_files\|project" \
            "Request classification with AI"
    else
        log_warning "AI query interpretation not available - skipping AI tests"
    fi
}

# Test performance and timing
test_performance() {
    log_info "Testing query interpretation performance"
    
    local start_time
    local end_time
    local duration
    
    start_time=$(date +%s)
    
    # Run a simple query
    test_query_interpretation \
        "list files" \
        "tool-call\|list_files" \
        "Performance test query"
    
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    
    if [[ $duration -lt 10 ]]; then
        log_success "Performance test passed: ${duration}s (< 10s threshold)"
    else
        log_warning "Performance test slow: ${duration}s (>= 10s)"
    fi
}

# Cleanup function
cleanup_test_environment() {
    log_info "Cleaning up test environment..."
    
    # Clear forj conversation
    run_emacs '(forj-clear-conversation)' 2>/dev/null || true
    
    # Remove test project
    if [[ -d "$TEST_PROJECT_DIR" ]]; then
        rm -rf "$TEST_PROJECT_DIR"
        log_info "Removed test project directory"
    fi
}

# Main test runner
run_tests() {
    local tests_passed=0
    local tests_failed=0
    
    log_info "Starting forj query interpreter TCP/emacsclient tests"
    
    # Setup
    create_test_project || {
        log_error "Failed to create test project"
        return 1
    }
    
    setup_test_environment || {
        log_error "Failed to setup test environment"
        return 1
    }
    
    # Run test suites
    log_info "Running test suites..."
    
    if test_fallback_patterns; then
        ((tests_passed++))
    else
        ((tests_failed++))
    fi
    
    if test_tool_execution; then
        ((tests_passed++))
    else
        ((tests_failed++))
    fi
    
    if test_error_handling; then
        ((tests_passed++))
    else
        ((tests_failed++))
    fi
    
    if test_ai_integration; then
        ((tests_passed++))
    else
        ((tests_failed++))
    fi
    
    if test_performance; then
        ((tests_passed++))
    else
        ((tests_failed++))
    fi
    
    # Summary
    log_info "Test Results Summary:"
    log_success "Tests passed: $tests_passed"
    if [[ $tests_failed -gt 0 ]]; then
        log_error "Tests failed: $tests_failed"
    else
        log_info "Tests failed: $tests_failed"
    fi
    
    # Cleanup
    cleanup_test_environment
    
    return $tests_failed
}

# Usage information
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Test script for forj query interpreter using TCP/emacsclient.

OPTIONS:
    -s, --socket PATH     Specify Emacs server socket path
    -p, --project DIR     Test project directory (default: $TEST_PROJECT_DIR)
    -t, --timeout SEC     Command timeout in seconds (default: $TIMEOUT)
    -h, --help           Show this help message

REQUIREMENTS:
    - Running Emacs instance with server enabled
    - forj.el loaded and working
    - emacsclient available in PATH

EXAMPLES:
    $0                                          # Auto-detect socket
    $0 -s /tmp/emacs501/server                 # Specific socket
    $0 -p /tmp/my-test-project                 # Custom test directory

Based on examples from CLAUDE.md for testing interactive Emacs features.
EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -s|--socket)
            SOCKET_PATH="$2"
            shift 2
            ;;
        -p|--project)
            TEST_PROJECT_DIR="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Main execution
main() {
    log_info "Forj Query Interpreter TCP Test Suite"
    log_info "======================================"
    
    # Find socket if not specified
    if [[ -z "$SOCKET_PATH" ]]; then
        SOCKET_PATH=$(find_emacs_socket)
        if [[ $? -ne 0 ]]; then
            log_error "Could not find Emacs server socket. Please ensure Emacs server is running."
            log_info "Try: M-x server-start in Emacs"
            exit 1
        fi
        log_info "Found Emacs socket: $SOCKET_PATH"
    fi
    
    # Validate socket
    if [[ ! -S "$SOCKET_PATH" ]]; then
        log_error "Socket path is not a valid socket: $SOCKET_PATH"
        exit 1
    fi
    
    # Test basic connectivity
    if ! run_emacs '(+ 1 2)' >/dev/null; then
        log_error "Cannot connect to Emacs via socket: $SOCKET_PATH"
        exit 1
    fi
    log_success "Successfully connected to Emacs"
    
    # Run tests
    if run_tests; then
        log_success "All tests completed successfully!"
        exit 0
    else
        log_error "Some tests failed"
        exit 1
    fi
}

# Run main function
main "$@"