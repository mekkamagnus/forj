#!/bin/bash

# demo-query-interpreter.sh - Demonstration script for forj query interpreter
# Shows how to use the TCP/emacsclient testing approach from CLAUDE.md

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}Forj Query Interpreter Demo${NC}"
echo "================================="
echo

# Check if we're in the forj directory
if [[ ! -f "forj.el" ]]; then
    echo "Please run this script from the forj project directory"
    exit 1
fi

# Find Emacs socket
echo -e "${BLUE}Finding Emacs server socket...${NC}"
SOCKET_PATHS=(/var/folders/*/T/emacs*/server /tmp/emacs*/server)
SOCKET=""

for path in "${SOCKET_PATHS[@]}"; do
    if [[ -S "$path" ]]; then
        SOCKET="$path"
        break
    fi
done

if [[ -z "$SOCKET" ]]; then
    echo "No Emacs server socket found. Please start Emacs and run M-x server-start"
    exit 1
fi

echo "Found socket: $SOCKET"
echo

# Function to run emacsclient commands
run_emacs() {
    emacsclient --socket-name "$SOCKET" -e "$1" 2>/dev/null || {
        echo "Command failed: $1"
        return 1
    }
}

# Test connection
echo -e "${BLUE}Testing connection...${NC}"
if run_emacs '(+ 1 2)' >/dev/null; then
    echo -e "${GREEN}✓ Connected to Emacs${NC}"
else
    echo "Failed to connect to Emacs"
    exit 1
fi
echo

# Setup demo environment
echo -e "${BLUE}Setting up demo environment...${NC}"

# Switch to forj directory
run_emacs "(cd \"$(pwd)\")" || exit 1

# Kill existing forj buffer
run_emacs '(kill-buffer "*forj*")' 2>/dev/null || true

# Load forj.el
echo "Loading forj.el..."
run_emacs "(load-file \"forj.el\")" || exit 1

# Start forj
echo "Starting forj..."
run_emacs '(forj-start)' || exit 1

echo -e "${GREEN}✓ Demo environment ready${NC}"
echo

# Demo queries
echo -e "${BLUE}Running demo queries...${NC}"
echo

queries=(
    "what's in this directory"
    "find TODO comments"
    "list project files"
    "show me the current directory"
)

for query in "${queries[@]}"; do
    echo -e "${YELLOW}Query: $query${NC}"
    
    # Submit query using query interpreter
    submit_cmd="(when (fboundp 'forj-process-query-with-interpretation) (forj-process-query-with-interpretation \"$query\"))"
    
    if run_emacs "$submit_cmd"; then
        echo "✓ Query submitted"
        
        # Wait for processing
        sleep 2
        
        # Get response
        response=$(run_emacs '(with-current-buffer "*forj*" (buffer-substring-no-properties (max 1 (- (point-max) 500)) (point-max)))' 2>/dev/null)
        
        if [[ -n "$response" ]]; then
            echo "Response preview:"
            echo "$response" | tail -3 | sed 's/^/  /'
        else
            echo "No response captured"
        fi
    else
        echo "✗ Failed to submit query"
    fi
    
    echo "---"
done

echo
echo -e "${BLUE}Demo completed!${NC}"
echo
echo "Tips for further exploration:"
echo "1. Open Emacs and check the *forj* buffer to see full responses"
echo "2. Try different natural language queries"
echo "3. Run the full test suite: ./test/test-forj-query-tcp.sh"
echo "4. Check test results with: emacs -batch -l ert -l test/test-forj-query-interpreter.el -f ert-run-tests-batch-and-exit"
echo