;;; test-forj-tools-query-integration.el --- Integration tests for query interpreter and tools -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests for forj-query-interpreter.el with forj-tools.el
;; Tests end-to-end query processing and tool execution

;;; Code:

(require 'ert)
(require 'forj-query-interpreter)
(require 'forj-tools)
(require 'json)

;;; Test Utilities

(defvar test-integration--temp-files '()
  "List of temporary files created during tests.")

(defvar test-integration--original-project-root nil
  "Store original project root for restoration.")

(defun test-integration--setup ()
  "Set up integration test environment."
  (setq test-integration--temp-files '())
  (setq test-integration--original-project-root forj-tools-project-root)
  ;; Create a temporary test directory
  (let ((temp-dir (make-temp-file "forj-test-" t)))
    (setq forj-tools-project-root temp-dir)
    (setq default-directory temp-dir)
    ;; Create some test files
    (test-integration--create-test-files temp-dir)))

(defun test-integration--teardown ()
  "Clean up integration test environment."
  ;; Clean up temporary files
  (dolist (file test-integration--temp-files)
    (when (file-exists-p file)
      (if (file-directory-p file)
          (delete-directory file t)
        (delete-file file))))
  (setq test-integration--temp-files '())
  ;; Restore original project root
  (setq forj-tools-project-root test-integration--original-project-root))

(defun test-integration--create-test-files (dir)
  "Create test files in DIR for integration testing."
  (let ((test-el (expand-file-name "test.el" dir))
        (test-txt (expand-file-name "readme.txt" dir))
        (test-md (expand-file-name "notes.md" dir))
        (subdir (expand-file-name "subdir" dir)))
    
    ;; Create main test files
    (with-temp-file test-el
      (insert ";;; test.el --- Test file\n\n")
      (insert "(defun test-function ()\n")
      (insert "  \"A test function.\"\n")
      (insert "  (message \"Hello from test\"))\n")
      (insert "\n(provide 'test)\n"))
    
    (with-temp-file test-txt
      (insert "This is a test README file.\n")
      (insert "It contains some sample text for testing.\n")
      (insert "TODO: Add more content here.\n"))
    
    (with-temp-file test-md
      (insert "# Test Notes\n\n")
      (insert "This is a markdown file for testing.\n")
      (insert "## Features\n\n")
      (insert "- Feature 1\n")
      (insert "- Feature 2\n"))
    
    ;; Create subdirectory with file
    (make-directory subdir)
    (with-temp-file (expand-file-name "subfile.txt" subdir)
      (insert "This is a file in a subdirectory.\n"))
    
    ;; Track files for cleanup
    (push test-el test-integration--temp-files)
    (push test-txt test-integration--temp-files)
    (push test-md test-integration--temp-files)
    (push subdir test-integration--temp-files)))

(defun test-integration--parse-tool-result (result-json)
  "Parse tool result JSON string and return alist."
  (condition-case err
      (json-read-from-string result-json)
    (error
     (error "Failed to parse tool result: %s" (error-message-string err)))))

;;; Fallback Pattern Integration Tests

(ert-deftest test-integration-query-list-files-fallback ()
  "Test directory listing query using fallback patterns."
  (test-integration--setup)
  (unwind-protect
      (progn
        ;; Disable AI to force fallback
        (setq forj-query-ai-enabled nil)
        
        (let* ((result (forj-query-process "what's in this directory"))
               (status (plist-get result :status))
               (tool-calls (plist-get result :tool_calls)))
          
          ;; Should successfully process query
          (should (eq status 'completed))
          (should tool-calls)
          (should (= (length tool-calls) 1))
          
          ;; Parse the tool call result
          (let* ((tool-call-json (car tool-calls))
                 (call-data (json-read-from-string tool-call-json)))
            (should (eq (cdr (assq 'name call-data)) 'list_files))
            (should (assq 'args call-data)))))
    (test-integration--teardown)))

(ert-deftest test-integration-query-search-fallback ()
  "Test search query using fallback patterns."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-query-ai-enabled nil)
        
        (let* ((result (forj-query-process "find TODO comments"))
               (status (plist-get result :status)))
          
          (should (eq status 'completed))))
    (test-integration--teardown)))

;;; Tool Execution Integration Tests

(ert-deftest test-integration-actual-tool-execution ()
  "Test actual tool execution through query interpreter."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-query-ai-enabled nil) ; Use predictable fallback
        
        ;; Test list_files execution
        (let* ((interpretation (forj-query-interpret "list files in this directory"))
               (tool-calls (forj-query-build-tool-calls interpretation)))
          
          (should tool-calls)
          
          ;; Execute the tool call
          (let* ((tool-call (car tool-calls))
                 (result-json (forj-tools-dispatch tool-call))
                 (result (test-integration--parse-tool-result result-json)))
            
            ;; Verify successful execution
            (should (eq (cdr (assq 'ok result)) t))
            (should (assq 'result result))
            
            ;; Verify files are listed
            (let ((files (cdr (assq 'result result))))
              (should (consp files))
              ;; Should find our test files
              (should (cl-some (lambda (f) 
                                (string-match-p "test\\.el" (alist-get 'path f))) 
                              files))))))
    (test-integration--teardown)))

(ert-deftest test-integration-read-file-execution ()
  "Test file reading through query interpreter."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-query-ai-enabled nil)
        
        ;; Create a simple tool plan for reading a file
        (let* ((tool-call (json-encode
                          `((id . "test_1")
                            (name . read_file)
                            (args . ((path . "test.el")))
                            (meta . ((source . "test"))))))
               (result-json (forj-tools-dispatch tool-call))
               (result (test-integration--parse-tool-result result-json)))
          
          ;; Verify successful file read
          (should (eq (cdr (assq 'ok result)) t))
          (let ((file-result (cdr (assq 'result result))))
            (should (assq 'content file-result))
            (should (assq 'path file-result))
            (should (string-match-p "test-function" 
                                   (cdr (assq 'content file-result)))))))
    (test-integration--teardown)))

;;; Safety and Approval Tests

(ert-deftest test-integration-approval-required-write ()
  "Test that write operations require approval."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-tools-approve-destructive t)
        
        ;; Attempt write without approval
        (let* ((tool-call (json-encode
                          `((id . "test_write")
                            (name . write_file)
                            (args . ((path . "new-file.txt") (content . "test content")))
                            (meta . ((source . "test"))))))
               (result-json (forj-tools-dispatch tool-call))
               (result (test-integration--parse-tool-result result-json)))
          
          ;; Should fail due to missing approval
          (should (eq (cdr (assq 'ok result)) nil))
          (let ((error-data (cdr (assq 'error result))))
            (should (eq (cdr (assq 'type error-data)) 'approval-required)))))
    (test-integration--teardown)))

(ert-deftest test-integration-approval-granted-write ()
  "Test write operation with approval granted."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-tools-approve-destructive t)
        
        ;; Write with approval
        (let* ((tool-call (json-encode
                          `((id . "test_write_approved")
                            (name . write_file)
                            (args . ((path . "approved-file.txt") (content . "approved content")))
                            (meta . ((source . "test") (approved . t))))))
               (result-json (forj-tools-dispatch tool-call))
               (result (test-integration--parse-tool-result result-json)))
          
          ;; Should succeed with approval
          (should (eq (cdr (assq 'ok result)) t))
          (let ((write-result (cdr (assq 'result result))))
            (should (assq 'path write-result))
            (should (assq 'bytes_written write-result)))
          
          ;; Verify file was actually created
          (let ((file-path (expand-file-name "approved-file.txt" (forj--project-root))))
            (should (file-exists-p file-path))
            (should (string= (with-temp-buffer 
                              (insert-file-contents file-path)
                              (buffer-string))
                            "approved content")))))
    (test-integration--teardown)))

;;; Sandboxing Tests

(ert-deftest test-integration-path-sandboxing ()
  "Test that operations are sandboxed to project root."
  (test-integration--setup)
  (unwind-protect
      (progn
        ;; Try to access file outside project root
        (let* ((outside-path "/tmp/outside-project.txt")
               (tool-call (json-encode
                          `((id . "test_sandbox")
                            (name . read_file)
                            (args . ((path . ,outside-path)))
                            (meta . ((source . "test"))))))
               (result-json (forj-tools-dispatch tool-call))
               (result (test-integration--parse-tool-result result-json)))
          
          ;; Should fail due to sandbox violation
          (should (eq (cdr (assq 'ok result)) nil))
          (let ((error-data (cdr (assq 'error result))))
            (should (string-match-p "outside project root" 
                                   (cdr (assq 'message error-data)))))))
    (test-integration--teardown)))

;;; Context Resolution Integration

(ert-deftest test-integration-context-resolution-in-tools ()
  "Test context resolution integration with tool execution."
  (test-integration--setup)
  (unwind-protect
      (progn
        ;; Test context resolution with actual directory
        (let* ((args '((:directory . "this directory")))
               (resolved (forj-query--resolve-context-references args))
               (resolved-dir (alist-get :directory resolved)))
          
          (should (string= resolved-dir (expand-file-name default-directory)))
          
          ;; Use resolved context in tool call
          (let* ((tool-call (json-encode
                            `((id . "test_context")
                              (name . list_files)
                              (args . ,(json-read-from-string (json-encode resolved)))
                              (meta . ((source . "test"))))))
                 (result-json (forj-tools-dispatch tool-call))
                 (result (test-integration--parse-tool-result result-json)))
            
            (should (eq (cdr (assq 'ok result)) t)))))
    (test-integration--teardown)))

;;; Error Handling Integration

(ert-deftest test-integration-tool-error-handling ()
  "Test error handling in tool execution through query interpreter."
  (test-integration--setup)
  (unwind-protect
      (progn
        ;; Try to read non-existent file
        (let* ((tool-call (json-encode
                          `((id . "test_error")
                            (name . read_file)
                            (args . ((path . "nonexistent-file.txt")))
                            (meta . ((source . "test"))))))
               (result-json (forj-tools-dispatch tool-call))
               (result (test-integration--parse-tool-result result-json)))
          
          ;; Should handle error gracefully
          (should (eq (cdr (assq 'ok result)) nil))
          (should (assq 'error result))))
    (test-integration--teardown)))

;;; Performance Integration Tests

(ert-deftest test-integration-multiple-tool-calls ()
  "Test handling multiple tool calls in sequence."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-query-ai-enabled nil)
        
        ;; Create interpretation with multiple tools
        (let* ((interpretation (list :status 'tool_plan
                                    :plan (list :needs_tools t
                                               :tools (list 
                                                      (list :name 'list_files
                                                           :args '((:directory . "."))
                                                           :confidence 0.9)
                                                      (list :name 'read_file
                                                           :args '((:path . "test.el"))
                                                           :confidence 0.8))
                                               :rationale "Multiple operations")))
               (tool-calls (forj-query-build-tool-calls interpretation)))
          
          (should (= (length tool-calls) 2))
          
          ;; Execute all tool calls
          (let ((results '()))
            (dolist (tool-call tool-calls)
              (let* ((result-json (forj-tools-dispatch tool-call))
                     (result (test-integration--parse-tool-result result-json)))
                (push result results)))
            
            ;; All should succeed
            (should (= (length results) 2))
            (dolist (result results)
              (should (eq (cdr (assq 'ok result)) t))))))
    (test-integration--teardown)))

;;; End-to-End Workflow Tests

(ert-deftest test-integration-complete-workflow ()
  "Test complete workflow from query to tool execution."
  (test-integration--setup)
  (unwind-protect
      (progn
        (setq forj-query-ai-enabled nil) ; Predictable fallback behavior
        
        ;; Test the complete workflow
        (let* ((query "what files are in this directory")
               (interpretation (forj-query-interpret query))
               (tool-calls (forj-query-build-tool-calls interpretation)))
          
          ;; Verify interpretation
          (should (eq (plist-get interpretation :status) 'tool_plan))
          (should tool-calls)
          
          ;; Execute tools and verify results
          (let ((execution-results '()))
            (dolist (tool-call tool-calls)
              (let* ((result-json (forj-tools-dispatch tool-call))
                     (result (test-integration--parse-tool-result result-json)))
                (should (eq (cdr (assq 'ok result)) t))
                (push result execution-results)))
            
            ;; Verify we got meaningful results
            (should execution-results)
            (let ((first-result (car execution-results)))
              (should (assq 'result first-result))))))
    (test-integration--teardown)))

(provide 'test-forj-tools-query-integration)

;;; test-forj-tools-query-integration.el ends here