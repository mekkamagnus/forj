;;; forj-api-test.el --- Tests for Forj API Integration -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the minimal API integration (Phase 1.4.5)
;; These tests validate the core API functionality needed for immediate testing

;;; Code:

(require 'ert)
(require 'json)
(require 'url)

(ert-deftest forj-test-get-api-key-success ()
  "Test successful API key retrieval from environment."
  (cl-letf (((symbol-function 'getenv) (lambda (var) (when (string= var "GEMINI_API_KEY") "test-key-123"))))
    (should (string= (forj-get-api-key) "test-key-123"))))

(ert-deftest forj-test-get-api-key-missing ()
  "Test API key retrieval when key is missing."
  (cl-letf (((symbol-function 'getenv) (lambda (_) nil)))
    (should-error (forj-get-api-key) :type 'error)))

(ert-deftest forj-test-api-request-structure ()
  "Test API request structure generation."
  (let ((prompt "Test prompt")
        (context "Test context"))
    (should (string-match-p "generativelanguage.googleapis.com" 
                            (forj-build-api-url)))
    (should (string-match-p "gemini-2.0-flash-exp"
                            (forj-build-api-url)))))

(ert-deftest forj-test-prompt-basic ()
  "Test basic prompt functionality."
  (with-temp-buffer
    (insert "Test buffer content")
    (let ((result (forj-prompt "Analyze this buffer")))
      (should (plist-get result :success)))))

(ert-deftest forj-test-response-validation ()
  "Test AI response validation with paren checker."
  (let ((response ";; Valid elisp code\n(defun test () \"test\")"))
    (should (forj-validate-response response))))

(ert-deftest forj-test-response-invalid ()
  "Test handling of invalid AI responses."
  (let ((response ";; Invalid elisp code\n(defun test () \"test")) ; Missing closing paren
    (should-not (forj-validate-response response))))

(ert-deftest forj-test-project-context ()
  "Test project context generation."
  (let ((context (forj-get-project-context)))
    (should (string-match-p "Project Structure:" context))
    (should (string-match-p "File Types:" context))))

(ert-deftest forj-test-conversation-context ()
  "Test conversation context retrieval."
  (with-temp-buffer-window "*forj-conversation*" nil nil
    (insert "Test conversation history")
    (should (string-match-p "Test conversation" 
                            (forj-get-conversation-context)))))

(ert-deftest forj-test-conversation-context-empty ()
  "Test conversation context when no buffer exists."
  (when (get-buffer "*forj-conversation*")
    (kill-buffer "*forj-conversation*"))
  (should (string= "No previous conversation" 
                   (forj-get-conversation-context))))

(ert-deftest forj-test-api-integration ()
  "Test full API request/response cycle."
  (when (getenv "GEMINI_API_KEY")
    (let ((response (forj-api-request "Hello, world!")))
      (should response)
      (should (stringp response)))))

(ert-deftest forj-test-conversation-integration ()
  "Test conversation buffer integration."
  (forj-display-response "Test AI response")
  (should (get-buffer "*forj-conversation*"))
  (with-current-buffer "*forj-conversation*"
    (should (search-backward "Test AI response" nil t))))

(ert-deftest forj-test-code-validation ()
  "Test AI-generated code validation."
  (let ((valid-code "(defun test () \"hello\")")
        (invalid-code "(defun test () \"hello\"")) ; Missing closing paren
    (should (forj-validate-response valid-code))
    (should-not (forj-validate-response invalid-code))))

(ert-deftest forj-test-secure-credentials ()
  "Test API key security."
  (should-error (forj-get-api-key) :type 'error) ; When no env var
  (cl-letf (((symbol-function 'getenv) 
             (lambda (var) (when (string= var "GEMINI_API_KEY") "test-key"))))
    (should (string= (forj-get-api-key) "test-key"))))

(provide 'forj-api-test)
;;; forj-api-test.el ends here