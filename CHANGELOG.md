# Changelog

All notable changes to forj.el will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Nothing yet

### Changed
- Nothing yet

### Deprecated
- Nothing yet

### Removed
- Nothing yet

### Fixed
- Nothing yet

### Security
- Nothing yet

## [0.1.0-alpha] - 2025-08-09

### Added
- Initial project structure with proper Emacs package headers
- ERT test framework with 14 comprehensive tests
- Basic interactive commands: `forj-prompt`, `forj-check-syntax`, `forj-show-conversation`
- Customization variables for API configuration
- Debug logging system with `forj-debug` function
- Environment variable API key retrieval (`GEMINI_API_KEY`)
- Development scripts: `test.sh`, `dev-load.sh`
- Makefile with development targets
- Comprehensive documentation structure
- Stub implementations for all Phase 1 MVP functions

### Technical Details
- Package requires Emacs 27.1+, cl-lib 0.5+, json 1.4+
- Lexical binding enabled throughout
- Autoload cookies for interactive commands
- Proper error handling with user-friendly messages
- TDD approach with 100% test coverage for implemented features

### Development Infrastructure
- 14 ERT tests covering package structure, API key handling, interactive commands
- Batch testing support for CI/CD
- Development environment scripts
- Makefile for common development tasks
- Git repository initialization

[Unreleased]: https://github.com/mekael/forj.el/compare/v0.1.0-alpha...HEAD
[0.1.0-alpha]: https://github.com/mekael/forj.el/releases/tag/v0.1.0-alpha