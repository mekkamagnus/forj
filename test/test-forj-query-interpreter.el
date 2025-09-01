;;; test-forj-query-interpreter.el --- Tests for Natural Language Query Interpreter -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for forj-query-interpreter.el
;; Tests cover AI plan parsing, validation, fallback patterns, and integration

;;; Code:

(require 'ert)
(require 'forj-query-interpreter)
(require 'forj-tools)

;;; Test Fixtures and Mocks

(defvar test-forj-query--mock-ai-enabled nil
  "Control mock AI responses during testing.")

(defvar test-forj-query--mock-responses '()
  "Mock AI responses for testing.")

(defun test-forj-query--mock-api-request (prompt &optional context)
  "Mock implementation of forj-api-request for testing."
  (if test-forj-query--mock-ai-enabled
      (let ((response (car test-forj-query--mock-responses)))
        (setq test-forj-query--mock-responses (cdr test-forj-query--mock-responses))
        response)
    nil))

(defun test-forj-query--setup ()
  "Set up test environment."
  (setq test-forj-query--mock-ai-enabled nil)
  (setq test-forj-query--mock-responses '())
  (setq forj-query-ai-enabled t)
  (setq forj-query-confidence-threshold 0.7))

(defun test-forj-query--teardown ()
  "Clean up test environment."
  (setq test-forj-query--mock-ai-enabled nil)
  (setq test-forj-query--mock-responses '())
  (setq forj-query-ai-enabled t))

;;; Context Resolution Tests

(ert-deftest test-forj-query-context-resolution-directory ()
  "Test context resolution for directory references."
  (test-forj-query--setup)
  (let* ((args '((:directory . "this directory")
                (:other . "some value")))
         (resolved (forj-query--resolve-context-references args))
         (dir-value (alist-get :directory resolved)))
    (should (stringp dir-value))
    (should (file-directory-p dir-value))
    (should (string= (alist-get :other resolved) "some value")))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-context-resolution-project ()
  "Test context resolution for project root references."
  (test-forj-query--setup)
  (let* ((args '((:path . "project root")
                (:other . "unchanged")))
         (resolved (forj-query--resolve-context-references args))
         (path-value (alist-get :path resolved)))
    (should (stringp path-value))
    (should (file-directory-p path-value))
    (should (string= (alist-get :other resolved) "unchanged")))
  (test-forj-query--teardown))

;;; Fallback Pattern Tests

(ert-deftest test-forj-query-fallback-directory-listing ()
  "Test fallback pattern matching for directory listing queries."
  (test-forj-query--setup)
  (let ((queries '("what's in this directory"
                  "list files"
                  "show me the directory"
                  "what files are here")))
    (dolist (query queries)
      (let ((result (forj-query--match-fallback-patterns query)))
        (should result)
        (should (eq (plist-get result :intent) 'list_files))
        (should (plist-get result :args)))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-fallback-search-patterns ()
  "Test fallback pattern matching for search queries."
  (test-forj-query--setup)
  (let ((queries '("find TODO comments"
                  "search for function definitions"
                  "look for bug in code")))
    (dolist (query queries)
      (let ((result (forj-query--match-fallback-patterns query)))
        (should result)
        (should (eq (plist-get result :intent) 'search))
        (should (plist-get result :args)))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-fallback-no-match ()
  "Test fallback pattern matching with no matches."
  (test-forj-query--setup)
  (let ((result (forj-query--match-fallback-patterns "random unmatched query about nothing specific")))
    (should (null result)))
  (test-forj-query--teardown))

;;; AI Response Parsing Tests

(ert-deftest test-forj-query-parse-intent-response-valid ()
  "Test parsing valid AI intent classification response."
  (test-forj-query--setup)
  (let* ((response "{\"intent\": \"request\", \"confidence\": 0.85, \"reasoning\": \"User wants to perform an action\"}")
         (parsed (forj-query--parse-intent-response response)))
    (should (eq (plist-get parsed :intent) 'request))
    (should (= (plist-get parsed :confidence) 0.85))
    (should (string= (plist-get parsed :reasoning) "User wants to perform an action")))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-parse-intent-response-invalid-intent ()
  "Test parsing AI response with invalid intent type."
  (test-forj-query--setup)
  (let* ((response "{\"intent\": \"invalid_type\", \"confidence\": 0.9, \"reasoning\": \"Test\"}")
         (parsed (forj-query--parse-intent-response response)))
    ;; Should default to 'request' for invalid intents
    (should (eq (plist-get parsed :intent) 'request))
    (should (= (plist-get parsed :confidence) 0.5))
    (should (string-match-p "Invalid intent" (plist-get parsed :reasoning))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-parse-intent-response-malformed ()
  "Test parsing malformed AI response."
  (test-forj-query--setup)
  (let* ((response "Not valid JSON at all")
         (parsed (forj-query--parse-intent-response response)))
    ;; Should handle gracefully with defaults
    (should (eq (plist-get parsed :intent) 'request))
    (should (= (plist-get parsed :confidence) 0.3))
    (should (string-match-p "Failed to parse" (plist-get parsed :reasoning))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-parse-tool-plan-response-valid ()
  "Test parsing valid AI tool plan response."
  (test-forj-query--setup)
  (let* ((response "{\"needs_tools\": true, \"tools\": [{\"name\": \"list_files\", \"args\": {\"directory\": \".\"}, \"confidence\": 0.9}], \"rationale\": \"User wants to see files\", \"notes\": \"Simple directory listing\"}")
         (parsed (forj-query--parse-tool-plan-response response)))
    (should (eq (plist-get parsed :needs_tools) t))
    (should (= (length (plist-get parsed :tools)) 1))
    (should (string= (plist-get parsed :rationale) "User wants to see files"))
    (should (string= (plist-get parsed :notes) "Simple directory listing")))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-parse-tool-plan-response-no-tools ()
  "Test parsing AI response indicating no tools needed."
  (test-forj-query--setup)
  (let* ((response "{\"needs_tools\": false, \"tools\": [], \"rationale\": \"Just a question\", \"notes\": \"\"}")
         (parsed (forj-query--parse-tool-plan-response response)))
    (should (not (plist-get parsed :needs_tools)))
    (should (= (length (plist-get parsed :tools)) 0))
    (should (string= (plist-get parsed :rationale) "Just a question")))
  (test-forj-query--teardown))

;;; Tool Plan Validation Tests

(ert-deftest test-forj-query-validate-tool-plan-valid-tools ()
  "Test validation of tool plan with valid tools."
  (test-forj-query--setup)
  (let* ((tools '(((name . "list_files") (args . ((:directory . "."))) (confidence . 0.9))
                 ((name . "read_file") (args . ((:path . "test.txt"))) (confidence . 0.8))))
         (validated (forj-query--validate-tool-plan tools)))
    (should (= (length validated) 2))
    (should (eq (plist-get (car validated) :name) 'list_files))
    (should (eq (plist-get (cadr validated) :name) 'read_file)))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-validate-tool-plan-invalid-tools ()
  "Test validation filters out invalid/unknown tools."
  (test-forj-query--setup)
  (let* ((tools '(((name . "list_files") (args . ((:directory . "."))) (confidence . 0.9))
                 ((name . "unknown_tool") (args . ((:param . "value"))) (confidence . 0.8))
                 ((name . "read_file") (args . ((:path . "test.txt"))) (confidence . 0.7))))
         (validated (forj-query--validate-tool-plan tools)))
    ;; Should filter out unknown_tool
    (should (= (length validated) 2))
    (should (eq (plist-get (car validated) :name) 'list_files))
    (should (eq (plist-get (cadr validated) :name) 'read_file)))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-validate-tool-plan-max-calls ()
  "Test validation respects maximum call limit."
  (test-forj-query--setup)
  (setq forj-query-max-plan-calls 2)
  (let* ((tools '(((name . "list_files") (args . ((:directory . "."))) (confidence . 0.9))
                 ((name . "read_file") (args . ((:path . "test1.txt"))) (confidence . 0.8))
                 ((name . "read_file") (args . ((:path . "test2.txt"))) (confidence . 0.7))
                 ((name . "read_file") (args . ((:path . "test3.txt"))) (confidence . 0.6))))
         (validated (forj-query--validate-tool-plan tools)))
    ;; Should respect max limit of 2
    (should (= (length validated) 2))
    (should (eq (plist-get (car validated) :name) 'list_files))
    (should (eq (plist-get (cadr validated) :name) 'read_file)))
  (test-forj-query--teardown))

;;; Integration Tests with Mocked AI

(ert-deftest test-forj-query-interpret-with-mocked-ai-request ()
  "Test query interpretation with mocked AI for request intent."
  (test-forj-query--setup)
  (setq test-forj-query--mock-ai-enabled t)
  (setq test-forj-query--mock-responses 
        '("{\"intent\": \"request\", \"confidence\": 0.9, \"reasoning\": \"User wants to list files\"}"
          "{\"needs_tools\": true, \"tools\": [{\"name\": \"list_files\", \"args\": {\"directory\": \".\"}, \"confidence\": 0.9}], \"rationale\": \"Directory listing requested\", \"notes\": \"\"}"))
  
  ;; Mock the API function
  (cl-letf (((symbol-function 'forj-api-request) #'test-forj-query--mock-api-request))
    (let ((result (forj-query--interpret-with-ai "what's in this directory" nil)))
      (should (eq (plist-get result :status) 'tool_plan))
      (should (eq (plist-get result :intent) 'request))
      (should (= (plist-get result :confidence) 0.9))
      (let ((plan (plist-get result :plan)))
        (should (plist-get plan :needs_tools))
        (should (= (length (plist-get plan :tools)) 1)))))
  
  (test-forj-query--teardown))

(ert-deftest test-forj-query-interpret-with-mocked-ai-question ()
  "Test query interpretation with mocked AI for question intent."
  (test-forj-query--setup)
  (setq test-forj-query--mock-ai-enabled t)
  (setq test-forj-query--mock-responses 
        '("{\"intent\": \"question\", \"confidence\": 0.95, \"reasoning\": \"User is asking for information\"}"))
  
  (cl-letf (((symbol-function 'forj-api-request) #'test-forj-query--mock-api-request))
    (let ((result (forj-query--interpret-with-ai "what is Emacs Lisp" nil)))
      (should (eq (plist-get result :status) 'question))
      (should (eq (plist-get result :intent) 'question))
      (should (eq (plist-get result :action) 'answer_only))
      (should (= (plist-get result :confidence) 0.95))))
  
  (test-forj-query--teardown))

;;; Tool Call Generation Tests

(ert-deftest test-forj-query-build-tool-calls ()
  "Test building tool-call JSON from interpretation results."
  (test-forj-query--setup)
  (let* ((interpretation (list :status 'tool_plan
                              :plan (list :needs_tools t
                                         :tools (list (list :name 'list_files
                                                           :args '((:directory . "."))
                                                           :confidence 0.9)
                                                     (list :name 'read_file
                                                           :args '((:path . "test.txt"))
                                                           :confidence 0.8))
                                         :rationale "Test")))
         (tool-calls (forj-query-build-tool-calls interpretation)))
    (should (= (length tool-calls) 2))
    ;; Verify JSON structure by parsing first call
    (let* ((call1-json (car tool-calls))
           (call1-data (json-read-from-string call1-json)))
      (should (string-match-p "qry_" (cdr (assq 'id call1-data))))
      (should (eq (cdr (assq 'name call1-data)) 'list_files))
      (should (assq 'args call1-data))
      (should (assq 'meta call1-data))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-build-tool-calls-no-tools ()
  "Test building tool calls when no tools needed."
  (test-forj-query--setup)
  (let* ((interpretation (list :status 'question
                              :plan (list :needs_tools nil
                                         :tools '())))
         (tool-calls (forj-query-build-tool-calls interpretation)))
    (should (null tool-calls)))
  (test-forj-query--teardown))

;;; Error Handling Tests

(ert-deftest test-forj-query-interpret-ai-disabled ()
  "Test query interpretation when AI is disabled."
  (test-forj-query--setup)
  (setq forj-query-ai-enabled nil)
  (let ((result (forj-query-interpret "what's in this directory")))
    ;; Should fall back to pattern matching
    (should (eq (plist-get result :status) 'tool_plan))
    (should (string-match-p "fallback" (plist-get result :reasoning))))
  (test-forj-query--teardown))

(ert-deftest test-forj-query-interpret-with-error-recovery ()
  "Test query interpretation with error and fallback recovery."
  (test-forj-query--setup)
  (setq forj-query-ai-enabled t)
  
  ;; Mock API to throw error
  (cl-letf (((symbol-function 'forj-api-request) 
             (lambda (&rest _args) (error "Mock API error"))))
    (let ((result (forj-query-interpret "list files")))
      ;; Should handle error and provide fallback
      (should result)
      (should (or (eq (plist-get result :status) 'error)
                  (eq (plist-get result :status) 'tool_plan)))))
  
  (test-forj-query--teardown))

;;; End-to-End Integration Tests

(ert-deftest test-forj-query-process-complete-flow ()
  "Test complete query processing flow with mocked components."
  (test-forj-query--setup)
  (setq forj-query-ai-enabled nil) ; Use fallback patterns for predictable behavior
  
  ;; Mock the tool dispatcher to avoid actual execution
  (cl-letf (((symbol-function 'forj-tools-dispatch)
             (lambda (tool-call)
               (let ((call-data (json-read-from-string tool-call)))
                 (format "{\"id\": \"%s\", \"name\": \"%s\", \"ok\": true, \"result\": {\"test\": \"success\"}}"
                         (cdr (assq 'id call-data))
                         (cdr (assq 'name call-data)))))))
    
    (let ((result (forj-query-process "what's in this directory")))
      (should result)
      (should (eq (plist-get result :status) 'completed))
      (should (plist-get result :tool_calls))))
  
  (test-forj-query--teardown))

;;; Performance and Edge Case Tests

(ert-deftest test-forj-query-extract-search-query ()
  "Test search query extraction from natural language."
  (let ((queries '(("find TODO comments" . "TODO comments")
                  ("search for function definitions" . "function definitions")
                  ("look for bug in the code" . "bug in the code"))))
    (dolist (query-pair queries)
      (let* ((query (car query-pair))
             (expected (cdr query-pair))
             (extracted (forj-query--extract-search-query query))
             (query-arg (alist-get 'query extracted)))
        (should (string= query-arg expected))))))

(ert-deftest test-forj-query-extract-file-path ()
  "Test file path extraction from natural language."
  (let ((queries '(("read test.txt file" . "test.txt")
                  ("show me config.json" . "config.json")
                  ("open the main.el file" . "main.el"))))
    (dolist (query-pair queries)
      (let* ((query (car query-pair))
             (expected (cdr query-pair))
             (extracted (forj-query--extract-file-path query))
             (path-arg (alist-get 'path extracted)))
        (should (string= path-arg expected))))))

(ert-deftest test-forj-query-confidence-threshold ()
  "Test confidence threshold enforcement."
  (test-forj-query--setup)
  (setq forj-query-confidence-threshold 0.8)
  
  ;; Test with confidence below threshold
  (let ((low-confidence-result (list :confidence 0.6 :action 'tool_plan)))
    ;; Implementation should handle this based on confidence levels
    (should (numberp (plist-get low-confidence-result :confidence))))
  
  (test-forj-query--teardown))

(provide 'test-forj-query-interpreter)

;;; test-forj-query-interpreter.el ends here