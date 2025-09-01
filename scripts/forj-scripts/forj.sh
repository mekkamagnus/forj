#!/bin/bash
# forj.sh - Master workflow script for Forj development and testing

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

show_help() {
    cat << EOF
${BOLD}🚀 Forj Development & Testing Suite${NC}

Usage: $0 [command] [options]

${BOLD}Setup & Environment:${NC}
    setup               - Initial environment setup and socket discovery
    status              - Check system status and health
    
${BOLD}Development Workflow:${NC}
    dev reload          - Reload all Forj modules after changes
    dev check           - Check system functions and tool registration  
    dev debug           - Show debug information and current state
    dev validate        - Validate elisp code syntax
    dev clean           - Clean restart of entire system
    
${BOLD}Testing:${NC}
    test quick          - Run essential functionality tests
    test tools          - Comprehensive tool testing suite
    test regression     - Full regression test suite
    test query "text"   - Test specific natural language query
    test performance    - Run performance benchmarks
    
${BOLD}Interactive Testing:${NC}
    query "question"    - Test query interactively
    tool tool_name args - Test specific tool directly
    
${BOLD}Automation:${NC}
    ci                  - Run CI/CD pipeline tests
    watch               - Watch for file changes and auto-test

${BOLD}Examples:${NC}
    $0 setup                           # Initial setup
    $0 test regression                 # Full test suite
    $0 dev reload                      # After code changes
    $0 query "what directory am i in"  # Test specific query
    $0 tool get_current_directory '{}'  # Direct tool test

EOF
}

# Setup and environment management
setup_environment() {
    echo -e "${BLUE}🔧 Setting up Forj testing environment...${NC}"
    "$SCRIPT_DIR/forj-test-setup.sh"
    echo -e "${GREEN}✅ Setup complete!${NC}"
    echo "You can now run: $0 status"
}

# Status check
check_status() {
    echo -e "${BLUE}📊 Forj System Status${NC}"
    "$SCRIPT_DIR/forj-dev-workflow.sh" check
}

# Development workflow commands
dev_command() {
    local subcommand="$1"
    shift
    
    case "$subcommand" in
        "reload")
            "$SCRIPT_DIR/forj-dev-workflow.sh" reload
            ;;
        "check")
            "$SCRIPT_DIR/forj-dev-workflow.sh" check
            ;;
        "debug")
            "$SCRIPT_DIR/forj-dev-workflow.sh" debug
            ;;
        "validate")
            "$SCRIPT_DIR/forj-dev-workflow.sh" validate-code
            ;;
        "clean")
            "$SCRIPT_DIR/forj-dev-workflow.sh" clean-start
            ;;
        *)
            echo -e "${RED}❌ Unknown dev command: $subcommand${NC}"
            echo "Available: reload, check, debug, validate, clean"
            exit 1
            ;;
    esac
}

# Testing commands
test_command() {
    local subcommand="$1"
    shift
    
    case "$subcommand" in
        "quick")
            echo -e "${BLUE}🧪 Quick functionality test...${NC}"
            # Run a subset of key tests
            if "$SCRIPT_DIR/forj-regression-tests.sh" | grep -E "(System Tests|Direct Tool Tests|Natural Language)"; then
                echo -e "${GREEN}✅ Quick tests passed${NC}"
            else
                echo -e "${RED}❌ Quick tests failed${NC}"
                exit 1
            fi
            ;;
        "tools")
            "$SCRIPT_DIR/forj-test-tools.sh"
            ;;
        "regression")
            "$SCRIPT_DIR/forj-regression-tests.sh"
            ;;
        "query")
            local query="$1"
            if [ -z "$query" ]; then
                echo "Usage: $0 test query \"your question here\""
                exit 1
            fi
            "$SCRIPT_DIR/forj-dev-workflow.sh" test-query "$query"
            ;;
        "performance")
            "$SCRIPT_DIR/forj-dev-workflow.sh" performance
            ;;
        *)
            echo -e "${RED}❌ Unknown test command: $subcommand${NC}"
            echo "Available: quick, tools, regression, query, performance"
            exit 1
            ;;
    esac
}

# Interactive query using existing Emacs instance
interactive_query() {
    local query="$1"
    if [ -z "$query" ]; then
        echo "Usage: $0 query \"your question here\""
        exit 1
    fi
    
    echo -e "${BLUE}🗣️ Forj Query: '$query'${NC}"
    echo "---"
    
    # Use the enhanced emacsclient wrapper to submit query to existing Emacs
    "$PROJECT_ROOT/scripts/emacsclient-core/execute-on-emacsclient.sh" forj-query "$query"
}

# Direct tool testing
direct_tool() {
    local tool_name="$1"
    local args="$2"
    
    if [ -z "$tool_name" ]; then
        echo "Usage: $0 tool tool_name '{\"arg\": \"value\"}'"
        exit 1
    fi
    
    if [ -z "$args" ]; then
        args='{}'
    fi
    
    # Load environment
    if [ -f "$PROJECT_ROOT/.test-env" ]; then
        source "$PROJECT_ROOT/.test-env"
    else
        echo -e "${RED}❌ Run '$0 setup' first${NC}"
        exit 1
    fi
    
    echo -e "${BLUE}🔧 Testing tool: $tool_name${NC}"
    echo "Args: $args"
    echo ""
    
    local json_call="{\"id\": \"direct-test\", \"name\": \"$tool_name\", \"args\": $args, \"meta\": {}}"
    local result=$(emacsclient --socket-name "$FORJ_SOCKET" -e "(forj-tools-dispatch '$json_call')" 2>/dev/null)
    
    echo "Result:"
    echo "$result" | python3 -m json.tool 2>/dev/null || echo "$result"
}

# CI/CD pipeline
run_ci() {
    echo -e "${BOLD}🚀 CI/CD Pipeline${NC}"
    echo "================="
    
    # 1. Setup
    echo -e "${BLUE}1. Environment Setup${NC}"
    setup_environment
    echo ""
    
    # 2. Code validation
    echo -e "${BLUE}2. Code Validation${NC}"
    "$SCRIPT_DIR/forj-dev-workflow.sh" validate-code
    echo ""
    
    # 3. System check
    echo -e "${BLUE}3. System Check${NC}"
    "$SCRIPT_DIR/forj-dev-workflow.sh" check
    echo ""
    
    # 4. Regression tests
    echo -e "${BLUE}4. Regression Tests${NC}"
    "$SCRIPT_DIR/forj-regression-tests.sh"
    echo ""
    
    # 5. Performance tests
    echo -e "${BLUE}5. Performance Tests${NC}"
    "$SCRIPT_DIR/forj-dev-workflow.sh" performance
    echo ""
    
    echo -e "${GREEN}🎉 CI/CD Pipeline Complete!${NC}"
}

# Watch mode for development
watch_mode() {
    if ! command -v fswatch &> /dev/null; then
        echo -e "${YELLOW}⚠️ fswatch not found. Install with: brew install fswatch${NC}"
        echo "Falling back to basic watch mode..."
        
        while true; do
            echo -e "${BLUE}Running quick test...${NC}"
            test_command quick
            echo -e "${GREEN}Waiting 30 seconds... (Ctrl+C to stop)${NC}"
            sleep 30
        done
    else
        echo -e "${BLUE}👀 Watching for file changes...${NC}"
        echo "Monitoring: *.el files"
        echo "Press Ctrl+C to stop"
        
        fswatch -o "$PROJECT_ROOT"/*.el | while read f; do
            echo -e "\n${YELLOW}📝 Files changed, running tests...${NC}"
            dev_command reload
            test_command quick
            echo -e "${GREEN}✅ Tests complete. Watching for changes...${NC}"
        done
    fi
}

# Main command dispatcher
main() {
    cd "$PROJECT_ROOT"
    
    local command="${1:-help}"
    shift 2>/dev/null || true
    
    case "$command" in
        "setup")
            setup_environment
            ;;
        "status")
            check_status
            ;;
        "dev")
            dev_command "$@"
            ;;
        "test")
            test_command "$@"
            ;;
        "query")
            interactive_query "$@"
            ;;
        "tool")
            direct_tool "$@"
            ;;
        "ci")
            run_ci
            ;;
        "watch")
            watch_mode
            ;;
        "help"|*)
            show_help
            ;;
    esac
}

main "$@"