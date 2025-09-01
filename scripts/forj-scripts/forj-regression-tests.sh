#!/bin/bash
# forj-regression-tests.sh - Comprehensive regression testing suite

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Test results tracking
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
FAILED_TEST_NAMES=()

# Load environment
if [ -f .test-env ]; then
    source .test-env
else
    echo -e "${YELLOW}⚠️ Setting up test environment...${NC}"
    ./scripts/forj-scripts/forj-test-setup.sh
    source .test-env
fi

# Helper functions
emacs_eval() {
    emacsclient --socket-name "$FORJ_SOCKET" -e "$1" 2>/dev/null
}

run_test() {
    local test_name="$1"
    local test_function="$2"
    
    echo -n "  $test_name... "
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if $test_function; then
        echo -e "${GREEN}✅ PASS${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}❌ FAIL${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        FAILED_TEST_NAMES+=("$test_name")
    fi
}

setup_clean_test() {
    emacs_eval '(kill-buffer "*forj*")' 2>/dev/null || true
    emacs_eval '(forj-start)' >/dev/null 2>&1
    sleep 0.5
}

# Test: System is properly loaded
test_system_loaded() {
    emacs_eval "(and (fboundp 'forj-start) (fboundp 'forj-tools-dispatch) (fboundp 'forj-check-for-tool-trigger))" | grep -q "t"
}

# Test: Tools are registered
test_tools_registered() {
    local tools_count=$(emacs_eval "(length (forj-tools-list))")
    [ "$tools_count" -gt "5" ]  # Should have at least 6 tools
}

# Test: get_current_directory tool works
test_get_current_directory() {
    setup_clean_test
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-1\", \"name\": \"get_current_directory\", \"args\": {}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    echo "$result" | grep -q '"ok":true' && echo "$result" | grep -q '"directory":'
}

# Test: read_file tool works
test_read_file() {
    setup_clean_test
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-2\", \"name\": \"read_file\", \"args\": {\"path\": \"README.md\", \"max_bytes\": 100}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    echo "$result" | grep -q '"ok":true' && echo "$result" | grep -q '"content":'
}

# Test: list_files tool works
test_list_files() {
    setup_clean_test
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-3\", \"name\": \"list_files\", \"args\": {\"directory\": \".\", \"max_depth\": 1}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    echo "$result" | grep -q '"ok":true'
}

# Test: search tool works
test_search_tool() {
    setup_clean_test
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-4\", \"name\": \"search\", \"args\": {\"query\": \"forj\", \"max_results\": 5}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    echo "$result" | grep -q '"ok":true'
}

# Test: Directory query natural language trigger
test_directory_nl_trigger() {
    setup_clean_test
    local trigger_result=$(emacs_eval '(forj-check-for-tool-trigger "What directory am I in?")')
    [ "$trigger_result" != "nil" ] && echo "$trigger_result" | grep -q "📁 Current directory:"
}

# Test: File listing natural language trigger  
test_files_nl_trigger() {
    setup_clean_test
    local trigger_result=$(emacs_eval '(forj-check-for-tool-trigger "List files")')
    [ "$trigger_result" != "nil" ] && echo "$trigger_result" | grep -q "📋 Files in directory:"
}

# Test: Search natural language trigger
test_search_nl_trigger() {
    setup_clean_test
    local trigger_result=$(emacs_eval '(forj-check-for-tool-trigger "Search for test")')
    [ "$trigger_result" != "nil" ]
}

# Test: Conversation flow with directory query
test_conversation_directory() {
    setup_clean_test
    
    # Submit query
    emacs_eval '(let ((prompt-text "What directory am I in?")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))' >/dev/null
    sleep 3
    
    # Check response
    local response=$(emacs_eval '(with-current-buffer "*forj*" (buffer-substring-no-properties (max (point-min) (- (point-max) 500)) (point-max)))')
    echo "$response" | grep -q "📁 Current directory:"
}

# Test: Conversation flow with file listing
test_conversation_files() {
    setup_clean_test
    
    # Submit query
    emacs_eval '(let ((prompt-text "List files in this directory")) (with-current-buffer (get-buffer-create "*forj-prompt*") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))' >/dev/null
    sleep 3
    
    # Check response
    local response=$(emacs_eval '(with-current-buffer "*forj*" (buffer-substring-no-properties (max (point-min) (- (point-max) 800)) (point-max)))')
    echo "$response" | grep -q "📋 Files in directory:"
}

# Test: Path validation works
test_path_validation() {
    setup_clean_test
    # This should fail due to path outside project root
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-5\", \"name\": \"read_file\", \"args\": {\"path\": \"../../../etc/passwd\"}, \"meta\": {}}")) (forj-tools-dispatch json-call)' 2>/dev/null || echo "validation-error")
    echo "$result" | grep -q "validation-error"
}

# Test: Tool error handling
test_error_handling() {
    setup_clean_test
    # Try to read non-existent file
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"reg-test-6\", \"name\": \"read_file\", \"args\": {\"path\": \"nonexistent-file-12345.txt\"}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    echo "$result" | grep -q '"ok":false'
}

# Test: Performance within reasonable bounds
test_performance_bounds() {
    setup_clean_test
    
    local start_time=$(date +%s.%N)
    emacs_eval '(forj-check-for-tool-trigger "What directory am I in?")' >/dev/null
    local end_time=$(date +%s.%N)
    
    local duration=$(echo "$end_time - $start_time" | bc -l)
    # Should complete within 2 seconds
    (( $(echo "$duration < 2.0" | bc -l) ))
}

# Main test suite
run_test_suite() {
    echo -e "${BOLD}🧪 Forj Regression Test Suite${NC}"
    echo "============================="
    echo "Socket: $FORJ_SOCKET"
    echo ""
    
    echo -e "${BLUE}📋 System Tests${NC}"
    run_test "System functions loaded" test_system_loaded
    run_test "Tools registered" test_tools_registered
    
    echo ""
    echo -e "${BLUE}🔧 Direct Tool Tests${NC}"  
    run_test "get_current_directory tool" test_get_current_directory
    run_test "read_file tool" test_read_file
    run_test "list_files tool" test_list_files
    run_test "search tool" test_search_tool
    
    echo ""
    echo -e "${BLUE}🗣️ Natural Language Trigger Tests${NC}"
    run_test "Directory NL trigger" test_directory_nl_trigger
    run_test "File listing NL trigger" test_files_nl_trigger  
    run_test "Search NL trigger" test_search_nl_trigger
    
    echo ""
    echo -e "${BLUE}💬 Conversation Flow Tests${NC}"
    run_test "Directory conversation" test_conversation_directory
    run_test "File listing conversation" test_conversation_files
    
    echo ""
    echo -e "${BLUE}🛡️ Security & Error Tests${NC}"
    run_test "Path validation" test_path_validation
    run_test "Error handling" test_error_handling
    
    echo ""
    echo -e "${BLUE}⚡ Performance Tests${NC}"
    run_test "Performance bounds" test_performance_bounds
    
    # Summary
    echo ""
    echo -e "${BOLD}📊 Test Results${NC}"
    echo "==============="
    echo -e "Total tests: ${BOLD}$TOTAL_TESTS${NC}"
    echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
    echo -e "Failed: ${RED}$FAILED_TESTS${NC}"
    
    if [ $FAILED_TESTS -gt 0 ]; then
        echo ""
        echo -e "${RED}❌ Failed tests:${NC}"
        for test_name in "${FAILED_TEST_NAMES[@]}"; do
            echo -e "  • $test_name"
        done
        echo ""
        echo -e "${RED}🚨 REGRESSION DETECTED${NC}"
        exit 1
    else
        echo ""
        echo -e "${GREEN}🎉 ALL TESTS PASSED${NC}"
        echo -e "${GREEN}✅ No regressions detected${NC}"
    fi
}

# Main execution
main() {
    cd "$(dirname "$0")/../.."  # Go to project root
    
    # Check dependencies
    if ! command -v bc &> /dev/null; then
        echo -e "${RED}❌ 'bc' command required for performance tests${NC}"
        exit 1
    fi
    
    run_test_suite
}

main "$@"