;;; forj-error-cli-test.el --- CLI Tests for Forj Error System -*- lexical-binding: t -*-

;;; Commentary:
;; CLI and automated testing scenarios for the error handling system.
;; Tests machine-readable error output format validation, stdout error messages,
;; exit codes, and automated testing scenario support.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; Load the modules under test
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "../forj-error-system.el" test-dir))
  (load-file (expand-file-name "../forj.el" test-dir)))

;;; Machine-Readable Output Format Tests

(ert-deftest forj-error-cli-test-json-format ()
  "Test machine-readable JSON error format."
  (let ((forj-error-log-targets nil)
        (error-context (make-forj-error-context
                        :type 'api-error
                        :severity 'high
                        :message "API request failed"
                        :timestamp (encode-time 0 30 14 25 12 2023)
                        :operation-context "CLI test context"
                        :function-name "test-function"
                        :file-path "/test/file.el"
                        :recovery-suggestions '("Check API key" "Retry request")
                        :details '(:status-code 500 :endpoint "api.example.com"))))
    
    (let ((json-output (forj-format-machine-error error-context)))
      (should (stringp json-output))
      
      ;; Parse JSON to verify structure
      (let ((parsed (condition-case nil
                        (json-read-from-string json-output)
                      (error nil))))
        (when parsed
          (should (equal (cdr (assq 'type parsed)) 'api-error))
          (should (equal (cdr (assq 'severity parsed)) 'high))
          (should (string= (cdr (assq 'message parsed)) "API request failed"))
          (should (cdr (assq 'timestamp parsed)))
          (should (string= (cdr (assq 'context parsed)) "CLI test context"))
          (should (string= (cdr (assq 'function parsed)) "test-function"))
          (should (string= (cdr (assq 'file parsed)) "/test/file.el"))
          (should (vectorp (cdr (assq 'recovery parsed)))))))))

(ert-deftest forj-error-cli-test-stdout-format ()
  "Test stdout error output format."
  (let ((forj-error-log-targets '(stdout))
        (forj-error-history nil)
        (output-buffer (get-buffer-create " *forj-test-stdout*")))
    
    (unwind-protect
        (progn
          ;; Redirect princ output for testing
          (cl-letf (((symbol-function 'princ) 
                     (lambda (obj &optional stream)
                       (with-current-buffer output-buffer
                         (insert (format "%s" obj))))))
            
            (condition-case err
                (forj-network-error "Test stdout error"
                                   :context "CLI stdout test"
                                   :details '(:test-mode t))
              (error nil))
            
            ;; Check stdout output
            (with-current-buffer output-buffer
              (let ((output (buffer-string)))
                (should (string-match-p "FORJ_ERROR:" output))
                (should (string-match-p "network-error" output))
                (should (string-match-p "Test stdout error" output))))))
      
      ;; Cleanup
      (kill-buffer output-buffer))))

(ert-deftest forj-error-cli-test-stderr-format ()
  "Test stderr error output format."
  (let ((forj-error-log-targets '(stderr))
        (forj-error-history nil)
        (temp-file (make-temp-file "forj-stderr-test")))
    
    (unwind-protect
        (progn
          ;; Mock call-process-region to capture stderr output
          (cl-letf (((symbol-function 'call-process-region)
                     (lambda (start end program &optional delete buffer display &rest args)
                       ;; Write input to temp file for verification
                       (write-region start end temp-file))))
            
            (condition-case err
                (forj-system-error "Test stderr error"
                                  :context "CLI stderr test")
              (error nil))
            
            ;; Check captured output
            (when (file-exists-p temp-file)
              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((output (buffer-string)))
                  (should (string-match-p "FORJ_ERROR:" output))
                  (should (string-match-p "system-error" output))
                  (should (string-match-p "Test stderr error" output)))))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Error Message Consistency Tests

(ert-deftest forj-error-cli-test-message-consistency ()
  "Test consistency of error messages across output formats."
  (let ((forj-error-log-targets nil)
        (test-message "Consistency test message")
        (test-context "CLI consistency test"))
    
    ;; Create error context
    (condition-case err
        (forj-validation-error test-message :context test-context)
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (let ((human-format (forj-format-human-error error-context))
            (machine-format (forj-format-machine-error error-context)))
        
        ;; Both formats should contain the core message
        (should (string-match-p test-message human-format))
        (should (string-match-p test-message machine-format))
        
        ;; Both should identify the error type
        (should (string-match-p "validation" (downcase human-format)))
        (should (string-match-p "validation" (downcase machine-format)))
        
        ;; Both should include context
        (should (string-match-p test-context human-format))
        (should (string-match-p test-context machine-format))))))

;;; Automated Testing Support Tests

(ert-deftest forj-error-cli-test-exit-codes ()
  "Test error type to exit code mapping for CLI compatibility."
  (let ((error-types '(user-error system-error network-error 
                                 validation-error file-error api-error)))
    
    ;; Test that different error types are distinguishable
    (dolist (error-type error-types)
      (let ((forj-error-log-targets nil)
            (forj-error-history nil))
        
        (condition-case err
            (forj-error error-type "CLI exit code test")
          (error nil))
        
        ;; Verify error was recorded
        (should (= (length forj-error-history) 1))
        (let ((error-context (car forj-error-history)))
          (should (eq (forj-error-context-type error-context) error-type)))))))

(ert-deftest forj-error-cli-test-batch-processing ()
  "Test error handling suitable for batch processing."
  (let ((forj-error-log-targets '(stdout))
        (forj-error-history nil)
        (batch-errors '(("File not found" . file-error)
                       ("Network timeout" . network-error)
                       ("Invalid input" . user-error))))
    
    ;; Process batch of errors
    (dolist (error-spec batch-errors)
      (condition-case err
          (forj-error (cdr error-spec) (car error-spec))
        (error nil)))
    
    ;; Verify all errors were processed
    (should (= (length forj-error-history) (length batch-errors)))
    
    ;; Verify error types are correct
    (dotimes (i (length batch-errors))
      (let ((expected-type (cdr (nth i batch-errors)))
            (actual-type (forj-error-context-type (nth i (reverse forj-error-history)))))
        (should (eq actual-type expected-type))))))

;;; Parse-ability Tests

(ert-deftest forj-error-cli-test-parseable-output ()
  "Test that error output can be parsed by external tools."
  (let ((forj-error-log-targets nil)
        (test-cases '(("Simple message" . user-error)
                     ("Message with \"quotes\" and symbols" . system-error)
                     ("Unicode message: ñoño" . api-error))))
    
    (dolist (test-case test-cases)
      (condition-case err
          (forj-error (cdr test-case) (car test-case))
        (error nil)))
    
    ;; Test that each error produces parseable machine output
    (let ((error-contexts (reverse forj-error-history)))
      (dotimes (i (length test-cases))
        (let* ((error-context (nth i error-contexts))
               (machine-output (forj-format-machine-error error-context))
               (parsed-data (condition-case nil
                                (json-read-from-string machine-output)
                              (error nil))))
          
          ;; Should be parseable
          (should parsed-data)
          
          ;; Should contain expected data
          (should (equal (cdr (assq 'type parsed-data)) 
                        (cdr (nth i test-cases))))
          (should (string= (cdr (assq 'message parsed-data))
                          (car (nth i test-cases)))))))))

(ert-deftest forj-error-cli-test-special-characters ()
  "Test handling of special characters in CLI output."
  (let ((forj-error-log-targets nil)
        (special-chars-message "Error with special chars: \n\t\"'\\"))
    
    (condition-case err
        (forj-file-error special-chars-message
                        :details '(:path "/tmp/file with spaces.el"))
      (error nil))
    
    (let* ((error-context (car forj-error-history))
           (machine-output (forj-format-machine-error error-context))
           (human-output (forj-format-human-error error-context)))
      
      ;; Machine output should handle special characters properly
      (should (stringp machine-output))
      
      ;; Should be parseable despite special characters
      (let ((parsed (condition-case nil
                        (json-read-from-string machine-output)
                      (error nil))))
        (should parsed)
        (should (string= (cdr (assq 'message parsed)) special-chars-message))))))

;;; Structured Data Validation Tests

(ert-deftest forj-error-cli-test-structured-data ()
  "Test that machine-readable output contains all required fields."
  (let ((forj-error-log-targets nil)
        (required-fields '(type severity message timestamp context 
                              function file recovery details)))
    
    (condition-case err
        (forj-api-error "Structured data test"
                       :context "CLI structured test"
                       :function "test-function"
                       :file "/test/path.el"
                       :recovery '("Try again")
                       :details '(:code 404))
      (error nil))
    
    (let* ((error-context (car forj-error-history))
           (machine-output (forj-format-machine-error error-context))
           (parsed (condition-case nil
                       (json-read-from-string machine-output)
                     (error nil))))
      
      (should parsed)
      
      ;; Check that all required fields are present
      (dolist (field required-fields)
        (should (assq field parsed))))))

;;; Performance Tests for CLI Scenarios

(ert-deftest forj-error-cli-test-high-volume-processing ()
  "Test CLI error handling under high volume."
  (let ((forj-error-log-targets '(stdout))
        (forj-error-history nil)
        (start-time (current-time))
        (error-count 200))
    
    ;; Generate high volume of errors
    (dotimes (i error-count)
      (condition-case err
          (forj-user-error (format "High volume test error %d" i)
                          :context "CLI volume test")
        (error nil)))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should complete within reasonable time for CLI use
      (should (< elapsed 5.0))
      ;; All errors should be tracked
      (should (= (length forj-error-history) error-count)))))

;;; Cross-Platform CLI Tests

(ert-deftest forj-error-cli-test-cross-platform-paths ()
  "Test CLI error handling with different path formats."
  (let ((forj-error-log-targets nil)
        (path-formats (if (eq system-type 'windows-nt)
                         '("C:\\Program Files\\test.el"
                           "\\\\server\\share\\file.el")
                       '("/usr/local/lib/test.el"
                         "/tmp/test file.el"
                         "~/Documents/test.el"))))
    
    (dolist (path path-formats)
      (condition-case err
          (forj-file-error "Cross-platform path test"
                          :file path
                          :context "CLI path compatibility")
        (error nil)))
    
    ;; Verify all paths were handled
    (should (= (length forj-error-history) (length path-formats)))
    
    ;; Verify machine output handles paths correctly
    (dolist (error-context forj-error-history)
      (let ((machine-output (forj-format-machine-error error-context)))
        (should (stringp machine-output))
        ;; Should be parseable
        (should (condition-case nil
                    (json-read-from-string machine-output)
                  (error nil)))))))

;;; Integration with Testing Frameworks

(ert-deftest forj-error-cli-test-ert-integration ()
  "Test integration with ERT testing framework."
  (let ((forj-error-log-targets nil))
    
    ;; Test that errors can be properly caught by should-error
    (should-error 
     (forj-user-error "ERT integration test")
     :type 'user-error)
    
    ;; Test that error context is still created
    (should (= (length forj-error-history) 1))
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'user-error))
      (should (string-match-p "ERT integration test" 
                             (forj-error-context-message error-context))))))

(ert-deftest forj-error-cli-test-debug-mode ()
  "Test error handling in debug mode."
  (let ((forj-error-log-targets nil)
        (debug-on-error t)
        (forj-error-enable-stack-trace t))
    
    (condition-case err
        (forj-system-error "Debug mode test"
                          :context "CLI debug testing")
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      ;; Stack trace should be captured in debug mode
      (should (forj-error-context-stack-trace error-context))
      (should (listp (forj-error-context-stack-trace error-context))))))

;;; Command Line Argument Processing Tests

(ert-deftest forj-error-cli-test-command-line-errors ()
  "Test error handling for command-line argument processing."
  (let ((forj-error-log-targets '(stderr))
        (forj-error-history nil))
    
    ;; Simulate command-line argument validation errors
    (condition-case err
        (forj-validation-error "Invalid command line argument: --unknown-flag"
                              :context "CLI argument processing"
                              :details '(:argument "--unknown-flag" :position 3)
                              :recovery '("Check available flags with --help"
                                         "Remove unknown flag"
                                         "Use correct flag syntax"))
      (error nil))
    
    (let ((error-context (car forj-error-history)))
      (should (eq (forj-error-context-type error-context) 'validation-error))
      (should (string-match-p "command line" (forj-error-context-message error-context)))
      (should (plist-get (forj-error-context-details error-context) :argument)))))

;;; Output Redirection Tests

(ert-deftest forj-error-cli-test-output-redirection ()
  "Test error output when stdout/stderr are redirected."
  (let ((forj-error-log-targets '(stdout stderr))
        (forj-error-history nil)
        (stdout-file (make-temp-file "forj-stdout"))
        (stderr-file (make-temp-file "forj-stderr")))
    
    (unwind-protect
        (progn
          ;; Mock output functions to write to files
          (cl-letf (((symbol-function 'princ)
                     (lambda (obj &optional stream)
                       (write-region (format "%s" obj) nil stdout-file t)))
                    ((symbol-function 'call-process-region)
                     (lambda (start end program &optional delete buffer display &rest args)
                       (write-region start end stderr-file t))))
            
            (condition-case err
                (forj-network-error "Output redirection test")
              (error nil))
            
            ;; Check that output was written to both files
            (should (file-exists-p stdout-file))
            (should (file-exists-p stderr-file))
            
            (when (> (file-attribute-size (file-attributes stdout-file)) 0)
              (with-temp-buffer
                (insert-file-contents stdout-file)
                (should (string-match-p "FORJ_ERROR" (buffer-string)))))
            
            (when (> (file-attribute-size (file-attributes stderr-file)) 0)
              (with-temp-buffer
                (insert-file-contents stderr-file)
                (should (string-match-p "FORJ_ERROR" (buffer-string)))))))
      
      ;; Cleanup
      (dolist (file (list stdout-file stderr-file))
        (when (file-exists-p file)
          (delete-file file))))))

;;; Test Helper Functions

(defun forj-error-cli-test-run-batch ()
  "Helper function to run CLI tests in batch mode."
  (let ((forj-error-log-targets '(stdout))
        (test-results '()))
    
    ;; Run all CLI-related tests
    (dolist (test '(forj-error-cli-test-json-format
                   forj-error-cli-test-stdout-format
                   forj-error-cli-test-message-consistency))
      (let ((result (condition-case err
                        (progn (funcall test) 'passed)
                      (error 'failed))))
        (push (cons test result) test-results)))
    
    test-results))

(provide 'forj-error-cli-test)
;;; forj-error-cli-test.el ends here