#!/bin/bash
# check-parens.sh - Check for unbalanced parentheses in an Emacs Lisp file.

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- Function to show usage ---
usage() {
    echo "Usage: $0 <file-to-check.el>"
    echo
    echo "This script checks for unbalanced parentheses and other syntax errors"
    echo "in an Emacs Lisp file using Emacs in batch mode."
    exit 1
}

# --- Check for correct number of arguments ---
if [ "$#" -ne 1 ]; then
    usage
fi

FILE_TO_CHECK="$1"

# --- Check if file exists ---
if [ ! -f "$FILE_TO_CHECK" ]; then
    echo -e "${RED}Error: File not found: $FILE_TO_CHECK${NC}"
    exit 1
fi

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo -e "${YELLOW}Checking syntax of: $FILE_TO_CHECK${NC}"
echo "Project directory: $PROJECT_DIR"
echo "========================================"

# --- Run Emacs in batch mode to check syntax ---
# We load the file and then use `check-parens` to find syntax errors.
# The output is redirected to stderr, so we capture that.
emacs -batch \
    -L "$PROJECT_DIR" \
    --eval "(progn (find-file \"$FILE_TO_CHECK\") (check-parens))" 2>&1