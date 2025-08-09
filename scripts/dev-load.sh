#!/bin/bash
# dev-load.sh - Load forj.el in development mode

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo -e "${YELLOW}Loading forj.el in development mode...${NC}"
echo "Project directory: $PROJECT_DIR"

# Check for GEMINI_API_KEY
if [ -z "$GEMINI_API_KEY" ]; then
    echo -e "${YELLOW}Warning: GEMINI_API_KEY environment variable not set${NC}"
    echo "Set it with: export GEMINI_API_KEY=\"your-api-key\""
    echo
fi

echo -e "${GREEN}Starting Emacs with forj.el loaded...${NC}"
echo "Commands to try:"
echo "  M-x forj-prompt"
echo "  M-x forj-check-syntax"
echo "  M-x forj-show-conversation"
echo "  M-x forj-version"
echo

# Start Emacs with forj.el loaded
emacs -Q \
    -L "$PROJECT_DIR" \
    -l "$PROJECT_DIR/forj.el" \
    --eval "(message \"forj.el loaded successfully! Try M-x forj-prompt\")"