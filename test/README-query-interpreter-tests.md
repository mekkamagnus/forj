# Query Interpreter Testing Suite

This directory contains comprehensive tests for the Natural Language Query Interpretation Layer (Specification 004) implemented in `forj-query-interpreter.el`.

## Test Files Overview

### Unit Tests
- **`test-forj-query-interpreter.el`** - Core unit tests for query interpretation logic
- **`test-forj-tools-query-integration.el`** - Integration tests with the tools system
- **`test-forj-query-performance.el`** - Performance and benchmarking tests

### Interactive Testing Scripts
- **`test-forj-query-tcp.sh`** - TCP/emacsclient testing script for interactive validation
- **`demo-query-interpreter.sh`** - Quick demo script showing query interpreter in action

## Running Tests

### Unit Tests (ERT Framework)

```bash
# Run all query interpreter unit tests
emacs -batch -l ert -l test/test-forj-query-interpreter.el -f ert-run-tests-batch-and-exit

# Run integration tests
emacs -batch -l ert -l forj.el -l test/test-forj-tools-query-integration.el -f ert-run-tests-batch-and-exit

# Run performance tests
emacs -batch -l ert -l forj.el -l test/test-forj-query-performance.el -f ert-run-tests-batch-and-exit
```

### Interactive TCP Testing

The TCP testing approach follows the examples from `CLAUDE.md` for testing interactive Emacs features:

```bash
# Quick demo (requires running Emacs with server)
./test/demo-query-interpreter.sh

# Comprehensive interactive testing
./test/test-forj-query-tcp.sh

# With custom socket path
./test/test-forj-query-tcp.sh -s /path/to/emacs/socket

# With custom test project directory
./test/test-forj-query-tcp.sh -p /tmp/my-test-project
```

## Test Categories

### 1. Core Functionality Tests

**AI Integration Tests:**
- Intent classification (question, request, complaint)
- Tool plan generation and validation
- AI response parsing with error recovery
- Context resolution ("this directory" → actual paths)

**Fallback Pattern Tests:**
- Directory listing patterns ("what's in this directory")
- Search patterns ("find TODO comments")
- File operation patterns ("read test.el")
- Pattern compilation and matching performance

### 2. Integration Tests

**Tool System Integration:**
- End-to-end query processing with actual tool execution
- Tool call JSON generation and dispatch
- Safety and approval mechanism preservation
- Sandboxing validation (project root constraints)

**Context Resolution:**
- "this directory" → current working directory
- "current file" → buffer file name
- "project root" → forj project root
- Parameter validation and normalization

### 3. Performance Tests

**Latency Requirements (per Specification 004):**
- AI interpretation: <300ms average (development target)
- Fallback patterns: <100ms average
- Memory usage: <50MB for query processing

**Benchmarked Operations:**
- Pattern matching compilation and execution
- Context resolution performance
- Tool plan validation speed
- Concurrent query handling
- Large query processing (1000+ character queries)

### 4. Interactive Validation Tests

**TCP/Emacsclient Testing:**
- Socket discovery and connection validation
- Query submission through `forj-process-query-with-interpretation`
- Response validation in `*forj*` buffer
- Tool execution result verification
- Error handling and recovery testing

## Test Environment Setup

### Prerequisites
- Running Emacs instance with server enabled (`M-x server-start`)
- forj.el loaded and functional
- GEMINI_API_KEY environment variable (for AI tests)
- emacsclient available in PATH

### Socket Discovery
Tests automatically discover Emacs server sockets in standard locations:
- `/var/folders/*/T/emacs*/server`
- `/tmp/emacs*/server`
- `~/.emacs.d/server/server`

### Test Project Creation
Interactive tests create temporary test projects with:
- Sample Emacs Lisp files with functions and TODOs
- Markdown documentation files
- Text files with various content patterns
- Subdirectory structure for recursive testing

## Expected Test Results

### AI-Enabled Scenarios
When `forj-query-ai-enabled` is `t` and `GEMINI_API_KEY` is available:
- Natural language queries classified by AI intent
- Tool plans generated with confidence scoring
- Graceful fallback to patterns when AI fails

### Fallback-Only Scenarios
When AI is disabled or unavailable:
- Pattern-based matching for common queries
- Deterministic tool selection based on keywords
- Fast response times (<100ms average)

### Error Conditions
Tests validate behavior under:
- Malformed AI responses
- API timeouts and connection failures
- Invalid tool names in AI plans
- Path traversal attempts (security)
- Memory and processing limits

## Performance Benchmarks

The test suite includes comprehensive benchmarking:

```elisp
;; Run interactive benchmark
M-x test-forj-query-performance-run-benchmark

;; Expected results:
;; Fallback Pattern Matching: <50ms/call
;; Context Resolution: <10ms/call  
;; Tool Plan Validation: <20ms/call
```

## Test Coverage Areas

1. **Core Logic Coverage**
   - ✅ Intent classification and mapping
   - ✅ Tool plan generation and validation
   - ✅ Context reference resolution
   - ✅ Fallback pattern matching

2. **Integration Coverage**
   - ✅ forj-tools.el dispatcher integration
   - ✅ Safety mechanism preservation
   - ✅ Project sandboxing enforcement
   - ✅ Conversation buffer integration

3. **Error Handling Coverage**
   - ✅ AI API failures and recovery
   - ✅ Malformed response parsing
   - ✅ Invalid tool name filtering
   - ✅ Security constraint validation

4. **Performance Coverage**
   - ✅ Latency benchmarking
   - ✅ Memory usage validation
   - ✅ Concurrent query handling
   - ✅ Large query processing

## Troubleshooting Tests

### Common Issues

**Socket Connection Failures:**
```bash
# Check for running Emacs server
ps aux | grep emacs
# Ensure server is started in Emacs
M-x server-start
```

**Test Environment Issues:**
```bash
# Verify forj.el loads correctly
emacs -batch -l forj.el -eval '(message "forj loaded successfully")'

# Check query interpreter availability
emacs -batch -l forj.el -eval '(message "Query interpreter: %s" (fboundp '\''forj-query-interpret))'
```

**Performance Issues:**
- Run performance tests individually to identify bottlenecks
- Check AI API response times with manual testing
- Monitor memory usage during large query processing

## Contributing to Tests

When adding new query interpreter features:

1. **Add unit tests** for new functions in `test-forj-query-interpreter.el`
2. **Add integration tests** for tool system interactions
3. **Update performance tests** if new operations affect timing
4. **Add interactive tests** for user-facing functionality
5. **Update this README** with new test coverage information

Follow the established patterns for test structure, mocking, and validation to maintain consistency across the test suite.