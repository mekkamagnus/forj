;;; test-forj-query-performance.el --- Performance tests for query interpreter -*- lexical-binding: t -*-

;;; Commentary:
;; Performance tests for forj-query-interpreter.el as specified in 
;; specs/004-query-interpretation-layer-specs.md
;;
;; Tests interpretation latency, memory usage, and behavior under various conditions

;;; Code:

(require 'ert)
(require 'forj-query-interpreter)
(require 'benchmark)

;;; Performance Test Configuration

(defvar test-forj-query-perf--ai-timeout 3
  "Timeout for AI interpretation tests in seconds.")

(defvar test-forj-query-perf--fallback-timeout 1
  "Timeout for fallback interpretation tests in seconds.")

(defvar test-forj-query-perf--memory-threshold-mb 50
  "Memory usage threshold in MB.")

(defvar test-forj-query-perf--sample-queries
  '(;; Simple queries
    "list files"
    "what's in this directory"
    "show me the current directory"
    
    ;; Search queries
    "find TODO comments"
    "search for function definitions"
    "look for bug in code"
    
    ;; File operations
    "read test.el"
    "show me the contents of README.md"
    "open configuration file"
    
    ;; Complex queries
    "analyze the project structure and find all Emacs Lisp files with TODO comments"
    "search through all markdown files for documentation about installation"
    "list all files modified in the last week and show their Git status")
  "Sample queries for performance testing.")

;;; Performance Measurement Utilities

(defun test-forj-query-perf--measure-time (func)
  "Measure execution time of FUNC in milliseconds.
Returns (time-ms . result)."
  (let ((start-time (current-time))
        (result (funcall func))
        (end-time (current-time)))
    (cons (* 1000 (float-time (time-subtract end-time start-time))) result)))

(defun test-forj-query-perf--measure-memory (func)
  "Measure memory usage of FUNC in bytes.
Returns (memory-bytes . result)."
  (garbage-collect)
  (let ((start-mem (memory-use-counts))
        (result (funcall func))
        (end-mem (memory-use-counts)))
    (garbage-collect)
    (cons (- (car end-mem) (car start-mem)) result)))

(defun test-forj-query-perf--measure-both (func)
  "Measure both time and memory usage of FUNC.
Returns plist with :time-ms, :memory-bytes, :result."
  (garbage-collect)
  (let ((start-time (current-time))
        (start-mem (memory-use-counts))
        (result (funcall func))
        (end-time (current-time))
        (end-mem (memory-use-counts)))
    (garbage-collect)
    (list :time-ms (* 1000 (float-time (time-subtract end-time start-time)))
          :memory-bytes (- (car end-mem) (car start-mem))
          :result result)))

;;; Mock Functions for Controlled Testing

(defvar test-forj-query-perf--mock-ai-enabled nil
  "Control mock AI responses during performance testing.")

(defvar test-forj-query-perf--mock-response-delay 0
  "Artificial delay for mock responses in seconds.")

(defun test-forj-query-perf--mock-api-request (prompt &optional context)
  "Mock API request with controlled timing."
  (when (> test-forj-query-perf--mock-response-delay 0)
    (sleep-for test-forj-query-perf--mock-response-delay))
  
  (if test-forj-query-perf--mock-ai-enabled
      "{\"intent\": \"request\", \"confidence\": 0.9, \"reasoning\": \"Mock response\"}"
    nil))

;;; Performance Tests

(ert-deftest test-forj-query-performance-fallback-latency ()
  "Test fallback pattern matching latency meets requirements (<100ms)."
  (let ((forj-query-ai-enabled nil) ; Force fallback
        (total-time 0)
        (query-count 0))
    
    (dolist (query test-forj-query-perf--sample-queries)
      (let* ((measurement (test-forj-query-perf--measure-time
                          (lambda ()
                            (forj-query--match-fallback-patterns query))))
             (time-ms (car measurement)))
        (setq total-time (+ total-time time-ms))
        (setq query-count (1+ query-count))
        
        ;; Individual query should be under 100ms
        (should (< time-ms 100))))
    
    ;; Average should be well under 100ms
    (let ((avg-time (/ total-time query-count)))
      (should (< avg-time 50))
      (message "Fallback average latency: %.2fms" avg-time))))

(ert-deftest test-forj-query-performance-ai-latency-with-mock ()
  "Test AI interpretation latency with mock responses."
  (let ((forj-query-ai-enabled t)
        (test-forj-query-perf--mock-ai-enabled t)
        (test-forj-query-perf--mock-response-delay 0.1) ; 100ms delay
        (total-time 0)
        (query-count 0))
    
    ;; Mock the API function
    (cl-letf (((symbol-function 'forj-api-request) #'test-forj-query-perf--mock-api-request))
      
      (dolist (query '("list files" "find TODO" "read config.el"))
        (let* ((measurement (test-forj-query-perf--measure-time
                            (lambda ()
                              (condition-case nil
                                  (forj-query--classify-intent query)
                                (error nil)))))
               (time-ms (car measurement)))
          (setq total-time (+ total-time time-ms))
          (setq query-count (1+ query-count))
          
          ;; Should be reasonable considering mock delay
          (should (< time-ms 500)))))
    
    (let ((avg-time (/ total-time query-count)))
      (message "AI interpretation average latency (mocked): %.2fms" avg-time))))

(ert-deftest test-forj-query-performance-memory-usage ()
  "Test memory usage stays within reasonable limits."
  (let ((forj-query-ai-enabled nil) ; Use fallback for consistent results
        (max-memory 0)
        (total-memory 0)
        (query-count 0))
    
    (dolist (query test-forj-query-perf--sample-queries)
      (let* ((measurement (test-forj-query-perf--measure-memory
                          (lambda ()
                            (forj-query--match-fallback-patterns query))))
             (memory-bytes (car measurement)))
        (setq max-memory (max max-memory memory-bytes))
        (setq total-memory (+ total-memory memory-bytes))
        (setq query-count (1+ query-count))))
    
    (let* ((avg-memory (/ total-memory query-count))
           (max-memory-mb (/ max-memory 1024.0 1024.0))
           (avg-memory-mb (/ avg-memory 1024.0 1024.0)))
      
      ;; Memory usage should be reasonable
      (should (< max-memory-mb test-forj-query-perf--memory-threshold-mb))
      
      (message "Memory usage - Max: %.2fMB, Avg: %.2fMB" max-memory-mb avg-memory-mb))))

(ert-deftest test-forj-query-performance-context-resolution ()
  "Test context resolution performance."
  (let ((test-args '((:directory . "this directory")
                    (:path . "current file")
                    (:project . "project root")
                    (:other . "unchanged value")))
        (iterations 100)
        (total-time 0))
    
    (dotimes (i iterations)
      (let* ((measurement (test-forj-query-perf--measure-time
                          (lambda ()
                            (forj-query--resolve-context-references test-args))))
             (time-ms (car measurement)))
        (setq total-time (+ total-time time-ms))))
    
    (let ((avg-time (/ total-time iterations)))
      ;; Context resolution should be very fast
      (should (< avg-time 10))
      (message "Context resolution average: %.2fms" avg-time))))

(ert-deftest test-forj-query-performance-pattern-compilation ()
  "Test pattern matching compilation and execution performance."
  (let ((patterns forj-query--fallback-patterns)
        (test-queries '("list files" "find bugs" "read config" "unknown query")))
    
    ;; Test pattern compilation time
    (let* ((measurement (test-forj-query-perf--measure-time
                        (lambda ()
                          ;; Simulate pattern compilation
                          (dolist (pattern patterns)
                            (let ((regex (plist-get pattern :pattern)))
                              (string-match-p regex "test query"))))))
           (compile-time (car measurement)))
      
      (should (< compile-time 50))
      (message "Pattern compilation time: %.2fms" compile-time))
    
    ;; Test pattern matching execution time
    (let ((total-match-time 0))
      (dolist (query test-queries)
        (let* ((measurement (test-forj-query-perf--measure-time
                            (lambda ()
                              (forj-query--match-fallback-patterns query))))
               (time-ms (car measurement)))
          (setq total-match-time (+ total-match-time time-ms))))
      
      (let ((avg-match-time (/ total-match-time (length test-queries))))
        (should (< avg-match-time 25))
        (message "Pattern matching average: %.2fms" avg-match-time)))))

(ert-deftest test-forj-query-performance-tool-plan-validation ()
  "Test tool plan validation performance."
  (let ((sample-tools '(((name . "list_files") 
                        (args . ((:directory . "."))) 
                        (confidence . 0.9))
                       ((name . "read_file") 
                        (args . ((:path . "test.txt"))) 
                        (confidence . 0.8))
                       ((name . "search") 
                        (args . ((:query . "TODO") (:max_results . 100))) 
                        (confidence . 0.7))
                       ((name . "unknown_tool") 
                        (args . ((:param . "value"))) 
                        (confidence . 0.6))))
        (iterations 50)
        (total-time 0))
    
    (dotimes (i iterations)
      (let* ((measurement (test-forj-query-perf--measure-time
                          (lambda ()
                            (forj-query--validate-tool-plan sample-tools))))
             (time-ms (car measurement)))
        (setq total-time (+ total-time time-ms))))
    
    (let ((avg-time (/ total-time iterations)))
      ;; Tool plan validation should be fast
      (should (< avg-time 20))
      (message "Tool plan validation average: %.2fms" avg-time))))

(ert-deftest test-forj-query-performance-concurrent-queries ()
  "Test performance with multiple concurrent query interpretations."
  (let ((forj-query-ai-enabled nil) ; Use fallback for consistency
        (queries '("list files" "find TODO" "read config" "search bugs"))
        (concurrent-results '()))
    
    ;; Simulate concurrent queries by measuring them all together
    (let* ((measurement (test-forj-query-perf--measure-both
                        (lambda ()
                          (mapcar (lambda (query)
                                   (forj-query--match-fallback-patterns query))
                                 queries))))
           (time-ms (plist-get measurement :time-ms))
           (memory-bytes (plist-get measurement :memory-bytes)))
      
      ;; Concurrent processing should still be reasonable
      (should (< time-ms 200))
      (should (< (/ memory-bytes 1024.0 1024.0) 10))
      
      (message "Concurrent queries - Time: %.2fms, Memory: %.2fMB" 
               time-ms (/ memory-bytes 1024.0 1024.0)))))

(ert-deftest test-forj-query-performance-large-query-handling ()
  "Test performance with large/complex queries."
  (let ((large-queries 
         '("Please analyze the entire project structure, identify all Emacs Lisp files containing function definitions with TODO comments, search through all markdown documentation files for installation instructions, find any configuration files that might need updating, and provide a comprehensive report of the project's current state including file sizes, modification dates, and any potential issues that might need attention"
           "Find all instances of the word 'function' in all files, then search for corresponding test files, analyze the code quality, identify any potential bugs or improvements, and generate a detailed report with recommendations for refactoring and optimization"
           "List every file in the project recursively, read the contents of all Emacs Lisp files, analyze their syntax, check for any parentheses mismatches, identify deprecated functions, and provide suggestions for modernizing the codebase"))
        (forj-query-ai-enabled nil)) ; Use fallback patterns
    
    (dolist (query large-queries)
      (let* ((measurement (test-forj-query-perf--measure-both
                          (lambda ()
                            (forj-query--match-fallback-patterns query))))
             (time-ms (plist-get measurement :time-ms))
             (memory-bytes (plist-get measurement :memory-bytes)))
        
        ;; Large queries should still be handled efficiently
        (should (< time-ms 100))
        (should (< (/ memory-bytes 1024.0 1024.0) 5))
        
        (message "Large query - Time: %.2fms, Memory: %.2fMB, Length: %d chars" 
                 time-ms (/ memory-bytes 1024.0 1024.0) (length query))))))

(ert-deftest test-forj-query-performance-error-recovery ()
  "Test performance of error handling and recovery."
  (let ((forj-query-ai-enabled t)
        (test-forj-query-perf--mock-ai-enabled nil)) ; Force API errors
    
    ;; Mock API to throw errors
    (cl-letf (((symbol-function 'forj-api-request) 
               (lambda (&rest _args) (error "Mock API error"))))
      
      (let* ((measurement (test-forj-query-perf--measure-both
                          (lambda ()
                            (condition-case err
                                (forj-query-interpret "test query")
                              (error err)))))
             (time-ms (plist-get measurement :time-ms))
             (memory-bytes (plist-get measurement :memory-bytes)))
        
        ;; Error recovery should be fast
        (should (< time-ms 100))
        (should (< (/ memory-bytes 1024.0 1024.0) 5))
        
        (message "Error recovery - Time: %.2fms, Memory: %.2fMB" 
                 time-ms (/ memory-bytes 1024.0 1024.0))))))

;;; Performance Benchmarking

(defun test-forj-query-performance-run-benchmark ()
  "Run comprehensive performance benchmark for query interpretation."
  (interactive)
  (message "Starting forj query interpreter performance benchmark...")
  
  (let ((results '()))
    
    ;; Benchmark fallback pattern matching
    (let ((bench-result (benchmark-run 100
                          (forj-query--match-fallback-patterns "list files"))))
      (push (list :test "Fallback Pattern Matching"
                 :time-per-call (* 1000 (car bench-result))
                 :gc-count (cadr bench-result)
                 :gc-time (* 1000 (caddr bench-result)))
            results))
    
    ;; Benchmark context resolution
    (let ((bench-result (benchmark-run 1000
                          (forj-query--resolve-context-references 
                           '((:directory . "this directory"))))))
      (push (list :test "Context Resolution"
                 :time-per-call (* 1000 (car bench-result))
                 :gc-count (cadr bench-result)
                 :gc-time (* 1000 (caddr bench-result)))
            results))
    
    ;; Benchmark tool plan validation
    (let ((bench-result (benchmark-run 200
                          (forj-query--validate-tool-plan
                           '(((name . "list_files") (args . ((:directory . ".")))))))))
      (push (list :test "Tool Plan Validation"
                 :time-per-call (* 1000 (car bench-result))
                 :gc-count (cadr bench-result)
                 :gc-time (* 1000 (caddr bench-result)))
            results))
    
    ;; Display results
    (message "\n=== Forj Query Interpreter Performance Benchmark ===")
    (dolist (result (nreverse results))
      (message "%s: %.2fms/call (GC: %d collections, %.2fms total)"
               (plist-get result :test)
               (plist-get result :time-per-call)
               (plist-get result :gc-count)
               (plist-get result :gc-time)))
    
    (message "Benchmark complete.")
    results))

(provide 'test-forj-query-performance)

;;; test-forj-query-performance.el ends here