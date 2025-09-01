#!/bin/bash
# forj-test-setup.sh - Setup testing environment for Forj

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m' 
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🚀 Forj Testing Setup"
echo "===================="

# Function to find Emacs socket
find_emacs_socket() {
    local socket=$(find /var/folders -name server 2>/dev/null | head -1)
    if [ -z "$socket" ]; then
        echo -e "${RED}❌ No Emacs server socket found${NC}"
        echo "Start Emacs with server enabled: (server-start)"
        exit 1
    fi
    echo -e "${GREEN}✅ Found Emacs socket: $socket${NC}"
    echo "$socket"
}

# Function to test emacsclient connection
test_connection() {
    local socket="$1"
    echo -n "Testing connection... "
    if result=$(emacsclient --socket-name "$socket" -e '(+ 1 2)' 2>/dev/null); then
        if [ "$result" = "3" ]; then
            echo -e "${GREEN}✅ Connected${NC}"
            return 0
        fi
    fi
    echo -e "${RED}❌ Connection failed${NC}"
    return 1
}

# Function to check if forj is loaded
check_forj_loaded() {
    local socket="$1"
    echo -n "Checking if forj is loaded... "
    if emacsclient --socket-name "$socket" -e '(fboundp (quote forj-start))' 2>/dev/null | grep -q "t"; then
        echo -e "${GREEN}✅ Forj loaded${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠️ Forj not loaded${NC}"
        return 1
    fi
}

# Function to setup clean test environment
setup_clean_environment() {
    local socket="$1"
    echo "Setting up clean test environment..."
    
    # Kill existing forj buffer
    emacsclient --socket-name "$socket" -e '(kill-buffer "*forj*")' 2>/dev/null || true
    
    # Load forj system
    emacsclient --socket-name "$socket" -e '(load-file "forj.el")' >/dev/null
    
    # Start forj
    emacsclient --socket-name "$socket" -e '(forj-start)' >/dev/null
    
    echo -e "${GREEN}✅ Clean environment ready${NC}"
}

# Main execution
main() {
    cd "$(dirname "$0")/../.."  # Go to project root
    
    local socket=$(find_emacs_socket)
    test_connection "$socket" || exit 1
    
    if ! check_forj_loaded "$socket"; then
        echo "Loading forj system..."
        setup_clean_environment "$socket"
    fi
    
    echo "📝 Test environment ready!"
    echo "Socket: $socket"
    echo "Use: export FORJ_SOCKET='$socket'"
    
    # Export for other scripts
    echo "export FORJ_SOCKET='$socket'" > .test-env
}

main "$@"