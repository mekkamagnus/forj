#!/bin/bash
# forj-test-tools.sh - Automated tool testing for Forj

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Load test environment
if [ -f .test-env ]; then
    source .test-env
else
    echo -e "${RED}❌ Run forj-test-setup.sh first${NC}"
    exit 1
fi

if [ -z "$FORJ_SOCKET" ]; then
    echo -e "${RED}❌ FORJ_SOCKET not set${NC}"
    exit 1
fi

# Helper function to run emacsclient
emacs_eval() {
    emacsclient --socket-name "$FORJ_SOCKET" -e "$1" 2>/dev/null
}

# Helper function to clean and setup for each test
setup_test() {
    local test_name="$1"
    echo -e "${BLUE}🧪 Testing: $test_name${NC}"
    
    # Kill buffer and restart forj
    emacs_eval '(kill-buffer "*forj*")' 2>/dev/null || true
    emacs_eval '(forj-start)' >/dev/null
    sleep 0.5
}

# Helper function to submit prompt and get response
submit_prompt() {
    local prompt="$1"
    local timeout="${2:-5}"
    
    emacs_eval "(let ((prompt-text \"$prompt\")) (with-current-buffer (get-buffer-create \"*forj-prompt*\") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))" >/dev/null
    
    # Wait for response
    sleep "$timeout"
    
    # Get response
    emacs_eval '(with-current-buffer "*forj*" (buffer-substring-no-properties (max (point-min) (- (point-max) 1000)) (point-max)))'
}

# Test direct tool calls
test_direct_tools() {
    echo -e "${YELLOW}📋 Testing Direct Tool Calls${NC}"
    
    # Test get_current_directory
    setup_test "get_current_directory tool"
    local result=$(emacs_eval 'let ((json-call "{\"id\": \"test-1\", \"name\": \"get_current_directory\", \"args\": {}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    
    if echo "$result" | grep -q '"ok":true'; then
        echo -e "${GREEN}✅ get_current_directory works${NC}"
    else
        echo -e "${RED}❌ get_current_directory failed${NC}"
        echo "Response: $result"
    fi
    
    # Test read_file
    setup_test "read_file tool"
    result=$(emacs_eval 'let ((json-call "{\"id\": \"test-2\", \"name\": \"read_file\", \"args\": {\"path\": \"README.md\", \"max_bytes\": 200}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    
    if echo "$result" | grep -q '"ok":true'; then
        echo -e "${GREEN}✅ read_file works${NC}"
    else
        echo -e "${RED}❌ read_file failed${NC}"
        echo "Response: $result"
    fi
    
    # Test list_files
    setup_test "list_files tool"
    result=$(emacs_eval 'let ((json-call "{\"id\": \"test-3\", \"name\": \"list_files\", \"args\": {\"directory\": \".\", \"max_depth\": 1}, \"meta\": {}}")) (forj-tools-dispatch json-call)')
    
    if echo "$result" | grep -q '"ok":true'; then
        echo -e "${GREEN}✅ list_files works${NC}"
    else
        echo -e "${RED}❌ list_files failed${NC}"
        echo "Response: $result"
    fi
}

# Test natural language triggers
test_natural_language() {
    echo -e "${YELLOW}🗣️ Testing Natural Language Triggers${NC}"
    
    # Test directory query
    setup_test "Directory query natural language"
    local response=$(submit_prompt "What directory am I in?" 3)
    
    if echo "$response" | grep -q "📁 Current directory:"; then
        echo -e "${GREEN}✅ Directory query triggers tool${NC}"
    else
        echo -e "${RED}❌ Directory query doesn't trigger tool${NC}"
        echo "Response snippet: $(echo "$response" | tail -c 200)"
    fi
    
    # Test file listing query
    setup_test "File listing natural language"
    response=$(submit_prompt "List files in this directory" 3)
    
    if echo "$response" | grep -q "📋 Files in directory:"; then
        echo -e "${GREEN}✅ File listing triggers tool${NC}"
    else
        echo -e "${RED}❌ File listing doesn't trigger tool${NC}"
        echo "Response snippet: $(echo "$response" | tail -c 200)"
    fi
    
    # Test search query
    setup_test "Search natural language"
    response=$(submit_prompt "Search for defun" 4)
    
    if echo "$response" | grep -q "🔍 Search results:"; then
        echo -e "${GREEN}✅ Search query triggers tool${NC}"
    else
        echo -e "${RED}❌ Search query doesn't trigger tool${NC}"
        echo "Response snippet: $(echo "$response" | tail -c 200)"
    fi
}

# Test tool trigger detection
test_trigger_detection() {
    echo -e "${YELLOW}🎯 Testing Tool Trigger Detection${NC}"
    
    local test_queries=(
        "what directory am i in"
        "where am i"
        "pwd"
        "current directory"
        "list files"
        "show files"
        "search for test"
        "find something"
    )
    
    for query in "${test_queries[@]}"; do
        local result=$(emacs_eval "(forj-check-for-tool-trigger \"$query\")")
        
        if [ "$result" != "nil" ]; then
            echo -e "${GREEN}✅ '$query' → tool triggered${NC}"
        else
            echo -e "${YELLOW}⚠️ '$query' → no tool trigger${NC}"
        fi
    done
}

# Performance testing
test_performance() {
    echo -e "${YELLOW}⚡ Performance Testing${NC}"
    
    setup_test "Performance test"
    
    local start_time=$(date +%s.%N)
    submit_prompt "What directory am I in?" 2 >/dev/null
    local end_time=$(date +%s.%N)
    
    local duration=$(echo "$end_time - $start_time" | bc -l)
    echo -e "${BLUE}📊 Directory query response time: ${duration}s${NC}"
    
    if (( $(echo "$duration < 3.0" | bc -l) )); then
        echo -e "${GREEN}✅ Performance within acceptable range${NC}"
    else
        echo -e "${YELLOW}⚠️ Performance slower than expected${NC}"
    fi
}

# Main test runner
main() {
    cd "$(dirname "$0")/../.."  # Go to project root
    
    echo -e "${BLUE}🧪 Forj Tool Testing Suite${NC}"
    echo "=========================="
    echo "Using socket: $FORJ_SOCKET"
    echo ""
    
    test_direct_tools
    echo ""
    
    test_natural_language
    echo ""
    
    test_trigger_detection
    echo ""
    
    test_performance
    echo ""
    
    echo -e "${GREEN}🎉 Testing Complete!${NC}"
}

main "$@"