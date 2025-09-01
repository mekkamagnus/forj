#!/bin/bash
# forj-dev-workflow.sh - Development workflow automation for Forj

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
    echo -e "${YELLOW}⚠️ No .test-env found, running setup...${NC}"
    ./scripts/forj-scripts/forj-test-setup.sh
    source .test-env
fi

# Helper functions
emacs_eval() {
    emacsclient --socket-name "$FORJ_SOCKET" -e "$1" 2>/dev/null
}

show_help() {
    cat << EOF
🛠️ Forj Development Workflow Helper

Usage: $0 [command]

Commands:
    reload          - Reload all Forj modules
    check           - Check system status and function availability
    debug           - Debug current state (buffer contents, errors)
    test-query      - Interactive query testing
    validate-code   - Validate elisp code (parentheses, syntax)
    clean-start     - Clean restart of Forj system
    performance     - Run performance benchmarks
    regression      - Run full regression test suite
    
Examples:
    $0 reload                    # Reload after code changes
    $0 test-query "list files"   # Test specific query
    $0 debug                     # Show current state
EOF
}

# Reload all Forj modules
reload_forj() {
    echo -e "${BLUE}🔄 Reloading Forj modules...${NC}"
    
    local modules=("forj-tools.el" "forj-api.el" "forj.el")
    
    for module in "${modules[@]}"; do
        if [ -f "$module" ]; then
            echo -n "Loading $module... "
            if emacs_eval "(load-file \"$module\")" >/dev/null 2>&1; then
                echo -e "${GREEN}✅${NC}"
            else
                echo -e "${RED}❌${NC}"
                echo -e "${RED}Failed to load $module${NC}"
                return 1
            fi
        fi
    done
    
    # Restart forj
    echo -n "Restarting Forj... "
    emacs_eval '(kill-buffer "*forj*")' 2>/dev/null || true
    emacs_eval '(forj-start)' >/dev/null
    echo -e "${GREEN}✅${NC}"
}

# Check system status
check_system() {
    echo -e "${BLUE}🔍 System Status Check${NC}"
    echo "====================="
    
    # Check if key functions exist
    local functions=(
        "forj-start"
        "forj-tools-dispatch" 
        "forj-tools-list"
        "forj-check-for-tool-trigger"
        "forj-process-prompt-with-context"
    )
    
    for func in "${functions[@]}"; do
        echo -n "Function $func: "
        if emacs_eval "(fboundp '$func)" | grep -q "t"; then
            echo -e "${GREEN}✅ Available${NC}"
        else
            echo -e "${RED}❌ Missing${NC}"
        fi
    done
    
    # Check registered tools
    echo ""
    echo "Registered tools:"
    local tools=$(emacs_eval "(forj-tools-list)")
    echo "$tools" | sed 's/[()]//g' | sed 's/"//g' | tr ' ' '\n' | grep -v '^$' | while read tool; do
        echo -e "  ${GREEN}•${NC} $tool"
    done
    
    # Check buffer status
    echo ""
    echo -n "Forj buffer: "
    if emacs_eval "(get-buffer \"*forj*\")" | grep -q "buffer"; then
        echo -e "${GREEN}✅ Active${NC}"
    else
        echo -e "${RED}❌ Not active${NC}"
    fi
}

# Debug current state
debug_state() {
    echo -e "${BLUE}🐛 Debug Information${NC}"
    echo "===================="
    
    # Show current working directory
    echo "Emacs working directory:"
    emacs_eval "(pwd)" | sed 's/"//g'
    
    # Show forj buffer content (last 500 chars)
    echo ""
    echo "Forj buffer content (last 500 chars):"
    echo "-------------------------------------"
    local content=$(emacs_eval '(with-current-buffer "*forj*" (buffer-substring-no-properties (max (point-min) (- (point-max) 500)) (point-max)))')
    echo "$content" | sed 's/\\n/\n/g' | tail -10
    
    # Check for errors in messages
    echo ""
    echo "Recent messages (errors):"
    echo "-------------------------"
    emacs_eval '(with-current-buffer "*Messages*" (buffer-substring-no-properties (max (point-min) (- (point-max) 1000)) (point-max)))' | grep -i error | tail -5 || echo "No recent errors"
}

# Interactive query testing
test_query() {
    local query="$1"
    
    if [ -z "$query" ]; then
        echo "Enter query to test:"
        read -r query
    fi
    
    echo -e "${BLUE}🧪 Testing query: '$query'${NC}"
    
    # Test trigger detection
    echo -n "Trigger detection: "
    local trigger_result=$(emacs_eval "(forj-check-for-tool-trigger \"$query\")")
    if [ "$trigger_result" = "nil" ]; then
        echo -e "${YELLOW}No direct trigger${NC}"
    else
        echo -e "${GREEN}Direct trigger activated${NC}"
        echo "Result preview: $(echo "$trigger_result" | cut -c1-100)..."
        return
    fi
    
    # Test full conversation flow
    echo "Testing full conversation flow..."
    emacs_eval '(kill-buffer "*forj*")' 2>/dev/null || true
    emacs_eval '(forj-start)' >/dev/null
    
    emacs_eval "(let ((prompt-text \"$query\")) (with-current-buffer (get-buffer-create \"*forj-prompt*\") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))" >/dev/null
    
    echo "Waiting for response..."
    sleep 3
    
    local response=$(emacs_eval '(with-current-buffer "*forj*" (buffer-substring-no-properties (max (point-min) (- (point-max) 800)) (point-max)))')
    echo ""
    echo "Response:"
    echo "--------"
    echo "$response" | sed 's/\\n/\n/g' | tail -15
}

# Validate elisp code
validate_code() {
    echo -e "${BLUE}✅ Code Validation${NC}"
    echo "=================="
    
    local files=("forj.el" "forj-api.el" "forj-tools.el")
    
    for file in "${files[@]}"; do
        if [ -f "$file" ]; then
            echo -n "Checking $file... "
            
            # Check parentheses balance
            local paren_check=$(emacs_eval "(with-temp-buffer (insert-file-contents \"$file\") (goto-char (point-min)) (let ((open 0) (line 1)) (while (not (eobp)) (let ((char (char-after))) (cond ((eq char ?\() (setq open (1+ open))) ((eq char ?\)) (setq open (1- open))) ((eq char ?\n) (setq line (1+ line)))) (when (< open 0) (cl-return (format \"Extra ) at line %d\" line)))) (forward-char)) (if (= open 0) \"Balanced\" (format \"Unmatched ( - missing %d )\" open))))")
            
            if echo "$paren_check" | grep -q "Balanced"; then
                echo -e "${GREEN}✅ Syntax OK${NC}"
            else
                echo -e "${RED}❌ Syntax Error${NC}"
                echo "  $paren_check"
            fi
        fi
    done
}

# Performance benchmarks
run_performance() {
    echo -e "${BLUE}⚡ Performance Benchmarks${NC}"
    echo "========================"
    
    # Setup clean environment
    emacs_eval '(kill-buffer "*forj*")' 2>/dev/null || true
    emacs_eval '(forj-start)' >/dev/null
    
    local queries=(
        "What directory am I in?"
        "List files here" 
        "Search for defun"
    )
    
    for query in "${queries[@]}"; do
        echo -n "Testing '$query'... "
        
        local start_time=$(date +%s.%N)
        emacs_eval "(let ((prompt-text \"$query\")) (with-current-buffer (get-buffer-create \"*forj-prompt*\") (erase-buffer) (insert prompt-text) (forj-prompt-submit)))" >/dev/null
        sleep 2  # Wait for completion
        local end_time=$(date +%s.%N)
        
        local duration=$(echo "$end_time - $start_time" | bc -l)
        echo -e "${BLUE}${duration}s${NC}"
    done
}

# Main command dispatcher
main() {
    cd "$(dirname "$0")/../.."  # Go to project root
    
    local command="${1:-help}"
    
    case "$command" in
        "reload")
            reload_forj
            ;;
        "check")
            check_system
            ;;
        "debug")
            debug_state
            ;;
        "test-query")
            test_query "$2"
            ;;
        "validate-code")
            validate_code
            ;;
        "clean-start")
            reload_forj
            echo -e "${GREEN}✅ Clean start complete${NC}"
            ;;
        "performance")
            run_performance
            ;;
        "regression")
            echo -e "${BLUE}🧪 Running regression tests...${NC}"
            ./scripts/forj-scripts/forj-test-tools.sh
            ;;
        "help"|*)
            show_help
            ;;
    esac
}

main "$@"