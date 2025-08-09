# Contributing to Forj.el

First off, thank you for considering contributing to Forj.el! We welcome any help, from bug reports to new features. This document provides the information you need to get started with development.

## Development Philosophy

Forj.el is built from the ground up to be **quintessentially Emacs**. We embrace the tools and philosophies of the Emacs Lisp ecosystem to create a tool that feels like a natural extension of the editor.

Our development practices follow these Emacs-native conventions:

*   **Dependency Management:** Project dependencies are declared in the `Package-Requires:` header in the main `forj.el` file. They are managed by the built-in Emacs package manager.
*   **Build System:** We use a `Makefile` to automate common tasks. The primary "build" step is byte-compiling `.el` files into `.elc` files for faster loading.
*   **Testing Framework:** We use `ert` (Emacs Lisp Regression Testing), the built-in testing framework for Emacs. Tests are located in the `tests/` directory.
*   **Continuous Integration:** Our GitHub Actions workflow uses a command-line Emacs process to run the `ert` test suite.
*   **Distribution:** The package is intended for distribution on **MELPA**, the primary community package archive for Emacs.
*   **User Configuration:** We use the Emacs `customize` system. User-configurable variables are defined with `defcustom`, making them accessible via `M-x customize-group`.

## Getting Started

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/your-username/forj.el.git
    ```
2.  **Run the tests:**
    The easiest way to run the test suite is via the Makefile:
    ```bash
    make test
    ```
    This will run all `ert` tests in a clean, command-line Emacs instance.

## Submitting a Pull Request

1.  Fork the repository.
2.  Create a new branch for your feature or bugfix.
3.  Make your changes.
4.  Ensure the tests pass (`make test`).
5.  Submit a pull request with a clear description of your changes.
