#!/bin/bash
# test.sh - Run forj.el test suite

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Running forj.el test suite...${NC}"
echo "========================================"

# Check if Emacs is available
if ! command -v emacs &> /dev/null; then
    echo -e "${RED}Error: Emacs not found in PATH${NC}"
    exit 1
fi

# Get script directory (where this script is located)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "Project directory: $PROJECT_DIR"
echo "Running tests..."
echo

# Run tests with batch mode
if emacs -batch \
    -L "$PROJECT_DIR" \
    -l ert \
    -l "$PROJECT_DIR/forj.el" \
    -l "$PROJECT_DIR/test/forj-test.el" \
    -f ert-run-tests-batch-and-exit; then
    echo
    echo -e "${GREEN}✅ All tests passed!${NC}"
    exit 0
else
    echo
    echo -e "${RED}❌ Some tests failed!${NC}"
    exit 1
fi