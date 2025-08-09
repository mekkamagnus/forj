;;; forj.el --- AI co-pilot for Emacs Lisp development -*- lexical-binding: t -*-

;; Copyright (C) 2025 Mekael

;; Author: Mekael <mekael@example.com>
;; Version: 0.1.0-alpha
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (json "1.4"))
;; Keywords: convenience, tools, ai, lisp, development
;; URL: https://github.com/mekael/forj.el

;;; Commentary:

;; Forj.el is an AI co-pilot for Emacs with deep Emacs Lisp integration.
;; It provides AI-powered code analysis, refactoring, and validation with
;; a conversational interface similar to Claude Code or GitHub Copilot Chat.
;;
;; Key features:
;; - Custom parentheses balance checker for AI-generated code validation
;; - Real-time conversation buffer showing AI activity
;; - File system operations with current directory context  
;; - Cross-buffer editing capabilities
;; - Secure Gemini API integration
;;
;; Usage:
;; M-x forj-prompt - Start conversational coding session
;; M-x forj-check-syntax - Validate current buffer syntax
;; 
;; Configuration:
;; Set GEMINI_API_KEY environment variable before using.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup forj nil
  "AI co-pilot for Emacs Lisp development."
  :prefix "forj-"
  :group 'tools)

(defcustom forj-api-provider 'gemini
  "AI provider for forj operations.
Currently only 'gemini is supported during development."
  :type '(choice (const :tag "Google Gemini" gemini))
  :group 'forj)

(defcustom forj-api-model "gemini-2.0-flash-exp"
  "AI model to use for API calls.
Default is Gemini 2.0 Flash for development."
  :type 'string
  :group 'forj)

(defcustom forj-response-timeout 30
  "Timeout for API responses in seconds."
  :type 'integer
  :group 'forj)

(defcustom forj-auto-validate t
  "Automatically validate AI-generated code with forj-paren-checker."
  :type 'boolean
  :group 'forj)

(defcustom forj-conversation-buffer "*forj*"
  "Name of the buffer for conversation display."
  :type 'string
  :group 'forj)

(defcustom forj-debug nil
  "Enable debug mode for detailed logging."
  :type 'boolean
  :group 'forj)

;;; Variables

(defvar forj-conversation-history nil
  "List storing conversation history.")

(defvar forj-current-activity nil
  "Current activity status for display.")

;;; Utility Functions

(defun forj-debug (format-string &rest args)
  "Log debug message if `forj-debug' is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when forj-debug
    (message "[FORJ DEBUG] %s" (apply #'format format-string args))))

(defun forj-get-api-key ()
  "Retrieve Gemini API key from environment variable.
Signals an error if GEMINI_API_KEY is not set."
  (or (getenv "GEMINI_API_KEY")
      (user-error "GEMINI_API_KEY environment variable not set. See README for setup instructions")))

;;; Core Validation Functions (Phase 1.2)

(defun forj-paren-check (code-string)
  "Analyze CODE-STRING for parentheses balance and syntax errors.
Returns structured validation results for AI consumption."
  (let ((stack '())
        (errors '())
        (line 1)
        (column 1)
        (in-string nil)
        (in-comment nil)
        (paren-pairs '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))))
    (dotimes (pos (length code-string))
      (let ((char (aref code-string pos)))
        (cond
         ((eq char ?\\) ; Skip escaped characters
          (setq pos (1+ pos)))
         ((eq char ?\")
          (setq in-string (not in-string)))
         ((eq char ?\;)
          (unless in-string (setq in-comment t)))
         ((eq char ?\n)
          (setq in-comment nil)
          (setq line (1+ line))
          (setq column 0))
         ((not (or in-string in-comment))
          (cond
           ((assoc char paren-pairs)
            (push (list char line column) stack))
           ((rassoc char paren-pairs)
            (if (null stack)
                (push (list :type 'extra-paren :line line :column column :char char) errors)
              (let* ((opening (pop stack))
                     (expected (cdr (assoc (car opening) paren-pairs))))
                (unless (eq char expected)
                  (push (list :type 'mismatched-paren :line line :column column :found char :expected expected :opening-line (cadr opening)) errors))))))))
      (setq column (1+ column))))
    (while stack
      (let ((unclosed (pop stack)))
        (push (list :type 'unclosed-paren :line (cadr unclosed) :column (caddr unclosed) :char (car unclosed)) errors)))
    (if errors
        (list :status 'invalid :errors (nreverse errors))
      (list :status 'valid))))

(provide 'forj)

;;; forj.el ends here