;;; forj-error-test.el --- Tests for Forj Error Handling System -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive test suite for the centralized error handling system.
;; Tests cover error classification, context enrichment, formatting,
;; recovery suggestions, and integration with existing systems.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the modules under test
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../forj-error-system.el" test-dir))
  (load-file (expand-file-name "../forj.el" test-dir)))

;;; Basic Error Creation Tests

(ert-deftest forj-error-test-basic-error-creation ()
  "Test basic error context creation."
  (let ((forj-error-log-targets nil)) ; Disable logging for tests
    (should-error 
     (forj-user-error "Test error message")
     :type 'user-error)))

(ert-deftest forj-error-test-error-context-structure ()
  "Test error context structure creation."
  (let ((forj-error-log-targets nil)
        (error-context nil))
    (condition-case err
        (forj-user-error "Test message"
                        :context "test context"
                        :details '(:key "value")
                        :recovery "test recovery")
      (user-error
       (setq error-context (car forj-error-history))))
    
    (should error-context)
    (should (forj-error-context-p error-context))
    (should (eq (forj-error-context-type error-context) 'user-error))
    (should (string= (forj-error-context-message error-context) "Test message"))
    (should (forj-error-context-timestamp error-context))))

(ert-deftest forj-error-test-error-types ()
  "Test all error types can be created."
  (let ((forj-error-log-targets nil)
        (types '(user-error system-error network-error validation-error api-error)))
    (dolist (type types)
      (let ((error-context nil))
        (condition-case err
            (forj-error type "Test message")
          (error
           (setq error-context (car forj-error-history))))
        
        (should error-context)
        (should (eq (forj-error-context-type error-context) type))))))

;;; Error Classification Tests

(ert-deftest forj-error-test-severity-levels ()
  "Test error severity level assignment."
  (let ((forj-error-log-targets nil))
    (condition-case err
        (forj-error 'user-error "Test" :severity 'critical)
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-severity error-context) 'critical)))))

(ert-deftest forj-error-test-context-enrichment ()
  "Test automatic context enrichment."
  (let ((forj-error-log-targets nil))
    (condition-case err
        (forj-system-error "Test system error" 
                          :context "test operation"
                          :file "/test/file.el")
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should (string-match-p "test operation" 
                             (forj-error-context-operation-context error-context)))
      (should (string-match-p "/test/file.el" 
                             (forj-error-context-operation-context error-context)))
      (should (string-match-p "System environment issue" 
                             (forj-error-context-operation-context error-context))))))

;;; Recovery Suggestion Tests

(ert-deftest forj-error-test-recovery-suggestions ()
  "Test automated recovery suggestions."
  (let ((suggestions (forj-get-error-recovery-suggestions 'file-error)))
    (should (listp suggestions))
    (should (> (length suggestions) 0))
    (should (cl-some (lambda (s) (string-match-p "permission" s)) suggestions))))

(ert-deftest forj-error-test-custom-recovery ()
  "Test custom recovery suggestions."
  (let ((forj-error-log-targets nil))
    (condition-case err
        (forj-user-error "Test error" 
                        :recovery '("Custom suggestion 1" "Custom suggestion 2"))
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should (equal (forj-error-context-recovery-suggestions error-context)
                    '("Custom suggestion 1" "Custom suggestion 2"))))))

;;; Error Formatting Tests

(ert-deftest forj-error-test-human-formatting ()
  "Test human-readable error formatting."
  (let ((error-context (make-forj-error-context
                        :type 'user-error
                        :severity 'high
                        :message "Test error message"
                        :operation-context "test context"
                        :function-name "test-function"
                        :file-path "/test/file.el"
                        :recovery-suggestions '("Try again" "Check input"))))
    
    (let ((formatted (forj-format-human-error error-context)))
      (should (string-match-p "\\[HIGH\\] Test error message" formatted))
      (should (string-match-p "Context: test context" formatted))
      (should (string-match-p "Function: test-function" formatted))
      (should (string-match-p "file.el" formatted))
      (should (string-match-p "Try again" formatted))
      (should (string-match-p "Check input" formatted)))))

(ert-deftest forj-error-test-machine-formatting ()
  "Test machine-readable error formatting."
  (let ((error-context (make-forj-error-context
                        :type 'api-error
                        :severity 'medium
                        :message "API request failed"
                        :timestamp (current-time)
                        :details '(:status-code 500))))
    
    (let ((formatted (forj-format-machine-error error-context)))
      (should (stringp formatted))
      ;; Should be valid JSON or at least structured data
      (should (string-match-p "api-error" formatted))
      (should (string-match-p "medium" formatted))
      (should (string-match-p "API request failed" formatted)))))

;;; Error History and Metrics Tests

(ert-deftest forj-error-test-history-tracking ()
  "Test error history tracking."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    ;; Generate some test errors
    (condition-case err (forj-user-error "Error 1") (error nil))
    (condition-case err (forj-system-error "Error 2") (error nil))
    (condition-case err (forj-user-error "Error 3") (error nil))
    
    (should (= (length forj-error-history) 3))
    (should (string= (forj-error-context-message (nth 0 forj-error-history)) "Error 3"))
    (should (string= (forj-error-context-message (nth 2 forj-error-history)) "Error 1"))))

(ert-deftest forj-error-test-metrics-tracking ()
  "Test error frequency metrics."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (forj-error-metrics (make-hash-table :test 'equal)))
    
    ;; Generate test errors
    (condition-case err (forj-user-error "Error 1") (error nil))
    (condition-case err (forj-user-error "Error 2") (error nil))
    (condition-case err (forj-system-error "Error 3") (error nil))
    
    (should (= (gethash 'user-error forj-error-metrics) 2))
    (should (= (gethash 'system-error forj-error-metrics) 1))))

(ert-deftest forj-error-test-history-filtering ()
  "Test error history filtering by type."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    (condition-case err (forj-user-error "User error") (error nil))
    (condition-case err (forj-system-error "System error") (error nil))
    (condition-case err (forj-network-error "Network error") (error nil))
    
    (let ((user-errors (forj-get-error-history 'user-error)))
      (should (= (length user-errors) 1))
      (should (eq (forj-error-context-type (car user-errors)) 'user-error)))
    
    (let ((all-errors (forj-get-error-history)))
      (should (= (length all-errors) 3)))))

;;; Error Logging Tests

(ert-deftest forj-error-test-logging-to-messages ()
  "Test error logging to messages buffer."
  (let ((forj-error-log-targets '(messages-buffer)))
    (with-current-buffer "*Messages*"
      (let ((initial-point (point-max)))
        (condition-case err
            (forj-user-error "Test logging message")
          (error nil))
        
        ;; Check that something was logged
        (goto-char initial-point)
        (should (re-search-forward "Forj Error:" nil t))))))

(ert-deftest forj-error-test-logging-disabled ()
  "Test that logging can be disabled."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    (condition-case err
        (forj-user-error "Silent error")
      (error nil))
    
    ;; Error should still be in history
    (should (= (length forj-error-history) 1))
    ;; But no logging should occur (this is tested implicitly by not crashing)))

;;; Integration Tests

(ert-deftest forj-error-test-with-error-handling-macro ()
  "Test the forj-with-error-handling macro."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    (should-error
     (forj-with-error-handling 'user-error
       (error "Test error from macro"))
     :type 'user-error)
    
    (should (= (length forj-error-history) 1))
    (should (eq (forj-error-context-type (car forj-error-history)) 'user-error))))

(ert-deftest forj-error-test-error-wrapping ()
  "Test wrapping existing errors."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil))
    
    (condition-case err
        (forj-wrap-existing-error "Original error message" 
                                 :type 'system-error
                                 :context "wrapping test")
      (error nil))
    
    (should (= (length forj-error-history) 1))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'system-error))
      (should (string-match-p "Original error message" 
                             (forj-error-context-message error-context))))))

;;; Stack Trace Tests

(ert-deftest forj-error-test-stack-trace-capture ()
  "Test stack trace capture when enabled."
  (let ((forj-error-enable-stack-trace t)
        (forj-error-log-targets nil)
        (forj-error-history nil))
    
    (condition-case err
        (forj-system-error "Stack trace test")
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should (forj-error-context-stack-trace error-context))
      (should (listp (forj-error-context-stack-trace error-context))))))

(ert-deftest forj-error-test-stack-trace-disabled ()
  "Test that stack trace capture can be disabled."
  (let ((forj-error-enable-stack-trace nil)
        (forj-error-log-targets nil)
        (forj-error-history nil))
    
    (condition-case err
        (forj-system-error "No stack trace test")
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should-not (forj-error-context-stack-trace error-context)))))

;;; Error Analysis Tests

(ert-deftest forj-error-test-error-analysis ()
  "Test error pattern analysis."
  (let ((forj-error-log-targets nil)
        (forj-error-history nil)
        (forj-error-metrics (make-hash-table :test 'equal)))
    
    ;; Generate test data
    (condition-case err (forj-user-error "Error 1") (error nil))
    (condition-case err (forj-user-error "Error 2") (error nil))
    (condition-case err (forj-system-error "Error 3") (error nil))
    
    (let ((analysis (forj-analyze-error-patterns)))
      (should (= (plist-get analysis :total-errors) 3))
      (should (eq (plist-get analysis :most-common-type) 'user-error))
      (should (listp (plist-get analysis :type-distribution)))
      (should (listp (plist-get analysis :recent-errors))))))

;;; Recovery System Tests

(ert-deftest forj-error-test-recovery-attempt ()
  "Test automated error recovery attempts."
  (let ((error-context (make-forj-error-context
                        :type 'file-error
                        :message "Test file error"
                        :file-path "/tmp/test-recovery-file.txt")))
    
    ;; This should not crash even if recovery fails
    (let ((result (forj-attempt-error-recovery error-context)))
      (should (or (null result) (booleanp result))))))

;;; Configuration Tests

(ert-deftest forj-error-test-configuration-defaults ()
  "Test error system configuration defaults."
  (should (listp forj-error-log-targets))
  (should (stringp forj-error-log-file))
  (should (booleanp forj-error-enable-recovery-suggestions))
  (should (numberp forj-error-max-history)))

;;; Management Function Tests

(ert-deftest forj-error-test-clear-history ()
  "Test clearing error history and metrics."
  (let ((forj-error-log-targets nil))
    ;; Generate some errors
    (condition-case err (forj-user-error "Test 1") (error nil))
    (condition-case err (forj-system-error "Test 2") (error nil))
    
    (should (> (length forj-error-history) 0))
    (should (> (hash-table-count forj-error-metrics) 0))
    
    (forj-clear-error-history)
    
    (should (= (length forj-error-history) 0))
    (should (= (hash-table-count forj-error-metrics) 0))))

(ert-deftest forj-error-test-show-error-report ()
  "Test error report generation."
  (let ((forj-error-log-targets nil))
    ;; Generate some test errors
    (condition-case err (forj-user-error "Report test 1") (error nil))
    (condition-case err (forj-system-error "Report test 2") (error nil))
    
    ;; This should not crash
    (should-not (forj-show-error-report))
    
    ;; Check that report buffer was created
    (should (get-buffer "*Forj Error Report*"))))

;;; Helper Functions for Tests

(defun forj-error-test-setup ()
  "Set up clean environment for error tests."
  (setq forj-error-history nil)
  (setq forj-error-metrics (make-hash-table :test 'equal))
  (setq forj-error-log-targets nil))

(defun forj-error-test-teardown ()
  "Clean up after error tests."
  (forj-clear-error-history)
  (when (get-buffer "*Forj Error Report*")
    (kill-buffer "*Forj Error Report*")))

;;; Performance Tests

(ert-deftest forj-error-test-performance ()
  "Test error handling performance."
  (let ((forj-error-log-targets nil)
        (start-time (current-time)))
    
    ;; Generate many errors quickly
    (dotimes (i 100)
      (condition-case err 
          (forj-user-error (format "Performance test error %d" i))
        (error nil)))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should complete within reasonable time (less than 1 second for 100 errors)
      (should (< elapsed 1.0))
      ;; All errors should be tracked
      (should (= (length forj-error-history) 100)))))

;;; Edge Case Tests

(ert-deftest forj-error-test-edge-cases ()
  "Test edge cases and boundary conditions."
  (let ((forj-error-log-targets nil))
    
    ;; Test empty message
    (condition-case err
        (forj-user-error "")
      (error nil))
    (should (string= (forj-error-context-message (car forj-error-history)) ""))
    
    ;; Test nil context
    (condition-case err
        (forj-system-error "Test" :context nil)
      (error nil))
    (should (forj-error-context-p (car forj-error-history)))
    
    ;; Test very long message
    (let ((long-message (make-string 1000 ?x)))
      (condition-case err
          (forj-network-error long-message)
        (error nil))
      (should (string= (forj-error-context-message (car forj-error-history)) 
                      long-message)))))

(provide 'forj-error-test)
;;; forj-error-test.el ends here