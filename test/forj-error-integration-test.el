;;; forj-error-integration-test.el --- Integration Tests for Forj Error System -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests for error propagation across module boundaries,
;; API error handling in realistic scenarios, file operation error handling,
;; and Git integration error scenarios.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; Load the modules under test
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../forj-error-system.el" test-dir))
  (load-file (expand-file-name "../forj-api.el" test-dir))
  (load-file (expand-file-name "../forj.el" test-dir)))

;;; API Error Integration Tests

(ert-deftest forj-error-integration-api-no-key ()
  "Test API error handling when API key is missing."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (process-environment (copy-sequence process-environment)))
    
    ;; Remove API key from environment
    (setenv "GEMINI_API_KEY" nil)
    
    (should-error (forj-get-api-key) :type 'user-error)
    
    ;; Check error was logged with proper context
    (should (= (length forj-error-history) 1))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'user-error))
      (should (string-match-p "GEMINI_API_KEY" (forj-error-context-message error-context)))
      (should (forj-error-context-recovery-suggestions error-context)))))

(ert-deftest forj-error-integration-api-response-parsing ()
  "Test API response parsing error handling."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    ;; Test with malformed response
    (let ((bad-response '((invalid . "structure"))))
      (condition-case err
          (forj-parse-api-response bad-response)
        (error nil))
      
      (should (> (length forj-error-history) 0))
      (let ((error-context (car forj-error-history)))
        (should (eq (forj-error-context-type error-context) 'api-error))
        (should (string-match-p "candidates" (forj-error-context-message error-context)))))))

(ert-deftest forj-error-integration-api-validation ()
  "Test API response validation error handling."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    ;; Test with unbalanced parentheses
    (let ((invalid-response "(defun test ("))
      (should-not (forj-validate-response invalid-response))
      
      (should (> (length forj-error-history) 0))
      (let ((error-context (car forj-error-history)))
        (should (eq (forj-error-context-type error-context) 'validation-error))
        (should (string-match-p "syntax" (forj-error-context-message error-context)))))))

;;; File Operation Integration Tests

(ert-deftest forj-error-integration-file-read-nonexistent ()
  "Test file reading error for nonexistent files."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (nonexistent-file "/this/file/does/not/exist.el"))
    
    (should-error (forj-read-file nonexistent-file) :type 'file-error)
    
    ;; Check error context was created
    (should (> (length forj-error-history) 0))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'file-error)))))

(ert-deftest forj-error-integration-file-write-invalid-syntax ()
  "Test file writing with invalid syntax validation."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (temp-file (make-temp-file "forj-test" nil ".el"))
        (invalid-elisp "(defun broken ("))
    
    (unwind-protect
        (progn
          (condition-case err
              (forj-write-file temp-file invalid-elisp)
            (error nil))
          
          ;; Should have validation error
          (should (> (length forj-error-history) 0))
          (let ((error-context (car forj-error-history)))
            (should (eq (forj-error-context-type error-context) 'validation-error))
            (should (string-match-p "syntax" (forj-error-context-message error-context)))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest forj-error-integration-file-write-permission-denied ()
  "Test file writing to protected directory."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (protected-file "/root/forj-test.el")) ; Assuming /root is not writable
    
    (condition-case err
        (forj-write-file protected-file "(message \"test\")")
      (error nil))
    
    ;; Should have file error (may vary by system)
    (when (> (length forj-error-history) 0)
      (let ((error-context (car forj-error-history)))
        (should (memq (forj-error-context-type error-context) 
                     '(file-error system-error)))))))

;;; Cross-Module Error Propagation Tests

(ert-deftest forj-error-integration-api-to-main ()
  "Test error propagation from API module to main module."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (process-environment (copy-sequence process-environment)))
    
    ;; Remove API key to trigger error
    (setenv "GEMINI_API_KEY" nil)
    
    ;; Try to use API functionality that depends on key
    (condition-case err
        (forj-build-payload "test prompt")
      (error nil))
    
    ;; Should not error for payload building, but API request would fail
    ;; Test API request failure
    (condition-case err
        (forj-api-request "test prompt")
      (error nil))
    
    ;; Should have user error from missing API key
    (should (> (length forj-error-history) 0))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'user-error)))))

(ert-deftest forj-error-integration-nested-error-handling ()
  "Test nested error handling across function calls."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    (defun forj-test-nested-inner ()
      "Inner function that throws error."
      (forj-system-error "Inner function error"
                        :context "nested test inner"
                        :function "forj-test-nested-inner"))
    
    (defun forj-test-nested-outer ()
      "Outer function that calls inner."
      (forj-with-error-handling 'system-error
        (forj-test-nested-inner)))
    
    (condition-case err
        (forj-test-nested-outer)
      (error nil))
    
    ;; Should have errors from both levels
    (should (> (length forj-error-history) 0))
    
    ;; Clean up test functions
    (fmakunbound 'forj-test-nested-inner)
    (fmakunbound 'forj-test-nested-outer)))

;;; Error Context Preservation Tests

(ert-deftest forj-error-integration-context-preservation ()
  "Test that error context is preserved across module boundaries."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    ;; Create error with detailed context
    (condition-case err
        (forj-api-error "API integration test error"
                       :context "integration test scenario"
                       :details '(:endpoint "test-api" :method "POST")
                       :file "test-integration.el"
                       :recovery '("Check integration setup"
                                  "Verify test environment"))
      (error nil))
    
    (should (= (length forj-error-history) 1))
    (let ((error-context (car forj-error-history)))
      ;; Verify all context information is preserved
      (should (string-match-p "integration test scenario" 
                             (forj-error-context-operation-context error-context)))
      (should (forj-error-context-details error-context))
      (should (string-match-p "test-integration.el" 
                             (forj-error-context-operation-context error-context)))
      (should (equal (forj-error-context-recovery-suggestions error-context)
                    '("Check integration setup" "Verify test environment"))))))

;;; Error Metrics Integration Tests

(ert-deftest forj-error-integration-metrics-across-modules ()
  "Test error metrics collection across different modules."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (forj-error-metrics (make-hash-table :test 'equal)))
    
    ;; Generate errors from different modules
    (condition-case err (forj-user-error "User error from main") (error nil))
    (condition-case err (forj-api-error "API error from api module") (error nil))
    (condition-case err (forj-file-error "File error from file ops") (error nil))
    (condition-case err (forj-network-error "Network error") (error nil))
    
    ;; Check metrics
    (should (= (gethash 'user-error forj-error-metrics) 1))
    (should (= (gethash 'api-error forj-error-metrics) 1))
    (should (= (gethash 'file-error forj-error-metrics) 1))
    (should (= (gethash 'network-error forj-error-metrics) 1))
    
    ;; Test analysis
    (let ((analysis (forj-analyze-error-patterns)))
      (should (= (plist-get analysis :total-errors) 4))
      (should (= (length (plist-get analysis :type-distribution)) 4)))))

;;; Recovery Integration Tests

(ert-deftest forj-error-integration-recovery-file-operations ()
  "Test error recovery for file operations."
  (let ((forj-error-log-targets nil)
        (temp-dir (make-temp-file "forj-test" t))
        (test-file nil))
    
    (unwind-protect
        (progn
          (setq test-file (expand-file-name "subdir/test.el" temp-dir))
          
          ;; Try to write to non-existent subdirectory
          (let ((error-context (make-forj-error-context
                                :type 'file-error
                                :message "Directory does not exist"
                                :file-path test-file)))
            
            ;; Attempt recovery
            (let ((recovery-result (forj-attempt-file-error-recovery error-context)))
              ;; Recovery might succeed (creating directory) or fail
              (should (or (null recovery-result) (booleanp recovery-result))))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Logging Integration Tests

(ert-deftest forj-error-integration-logging-targets ()
  "Test error logging to multiple targets."
  (let ((temp-log-file (make-temp-file "forj-error-log"))
        (forj-error-log-file temp-log-file)
        (forj-error-log-targets '(messages-buffer file)))
    
    (unwind-protect
        (progn
          ;; Generate test error
          (condition-case err
              (forj-system-error "Integration logging test"
                                :context "multi-target logging")
            (error nil))
          
          ;; Check file logging
          (should (file-exists-p temp-log-file))
          (with-temp-buffer
            (insert-file-contents temp-log-file)
            (should (string-match-p "Integration logging test" (buffer-string)))
            (should (string-match-p "SYSTEM" (buffer-string)))))
      
      ;; Cleanup
      (when (file-exists-p temp-log-file)
        (delete-file temp-log-file)))))

(ert-deftest forj-error-integration-conversation-buffer-logging ()
  "Test error logging to conversation buffer."
  (let ((forj-error-log-targets '(conversation-buffer))
        (forj-conversation-buffer "*test-forj-conversation*")
        (test-buffer nil))
    
    (unwind-protect
        (progn
          ;; Create test conversation buffer
          (setq test-buffer (get-buffer-create forj-conversation-buffer))
          
          ;; Generate test error
          (condition-case err
              (forj-network-error "Conversation buffer logging test")
            (error nil))
          
          ;; Check buffer contents
          (with-current-buffer test-buffer
            (should (string-match-p "🚨.*Conversation buffer logging test" 
                                   (buffer-string)))))
      
      ;; Cleanup
      (when test-buffer
        (kill-buffer test-buffer)))))

;;; Performance Integration Tests

(ert-deftest forj-error-integration-performance-under-load ()
  "Test error system performance under load."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (start-time (current-time)))
    
    ;; Generate many errors with full context
    (dotimes (i 50)
      (condition-case err
          (forj-api-error (format "Load test error %d" i)
                         :context (format "performance test iteration %d" i)
                         :details `(:iteration ,i :batch-size 50)
                         :recovery '("This is a test" "No action needed"))
        (error nil)))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should complete within reasonable time
      (should (< elapsed 2.0))
      ;; All errors should be tracked
      (should (= (length forj-error-history) 50))
      ;; Metrics should be updated
      (should (= (gethash 'api-error forj-error-metrics) 50)))))

;;; Real-world Scenario Tests

(ert-deftest forj-error-integration-realistic-api-failure ()
  "Test realistic API failure scenario."
  (let ((forj-error-log-targets '(messages-buffer))
        (forj-error-history nil))
    
    ;; Simulate network failure during API request
    (defun forj-test-simulate-network-failure ()
      (forj-network-error "Connection timeout"
                         :context "API request to Gemini"
                         :details '(:timeout 30 :endpoint "generativelanguage.googleapis.com")
                         :recovery '("Check internet connectivity"
                                    "Verify API service status"
                                    "Try again in a few moments")))
    
    (condition-case err
        (forj-test-simulate-network-failure)
      (error nil))
    
    ;; Verify proper error handling
    (should (= (length forj-error-history) 1))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'network-error))
      (should (string-match-p "timeout" (forj-error-context-message error-context)))
      (should (plist-get (forj-error-context-details error-context) :timeout)))
    
    ;; Clean up
    (fmakunbound 'forj-test-simulate-network-failure)))

(ert-deftest forj-error-integration-file-permissions-cascade ()
  "Test cascading file permission errors."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (temp-file (make-temp-file "forj-readonly")))
    
    (unwind-protect
        (progn
          ;; Make file read-only
          (set-file-modes temp-file #o444)
          
          ;; Try to write to read-only file
          (condition-case err
              (forj-write-file temp-file "(message \"test\")")
            (error nil))
          
          ;; Should have appropriate error
          (when (> (length forj-error-history) 0)
            (let ((error-context (car forj-error-history)))
              (should (memq (forj-error-context-type error-context) 
                           '(file-error system-error))))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        ;; Restore write permissions before deleting
        (set-file-modes temp-file #o644)
        (delete-file temp-file)))))

;;; Error Chaining Tests

(ert-deftest forj-error-integration-error-chaining ()
  "Test chaining related errors."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    ;; Create initial error
    (condition-case err
        (forj-file-error "Initial file access error"
                        :context "file operation chain"
                        :file "/test/file.el")
      (error nil))
    
    (let ((initial-error (car forj-error-history)))
      
      ;; Create related error
      (condition-case err
          (forj-system-error "Subsequent system error"
                            :context "cleanup after file error"
                            :related (list initial-error))
        (error nil))
      
      (let ((related-error (car forj-error-history)))
        (should (forj-error-context-related-errors related-error))
        (should (= (length (forj-error-context-related-errors related-error)) 1))))))

;;; Test Helper Functions

(defun forj-error-integration-test-setup ()
  "Set up environment for integration tests."
  (setq forj-error-history nil)
  (setq forj-error-metrics (make-hash-table :test 'equal)))

(defun forj-error-integration-test-teardown ()
  "Clean up after integration tests."
  (forj-clear-error-history)
  ;; Clean up any test buffers
  (dolist (buffer-name '("*test-forj-conversation*" "*Forj Error Report*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

(provide 'forj-error-integration-test)
;;; forj-error-integration-test.el ends here