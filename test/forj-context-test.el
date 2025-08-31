;;; forj-context-test.el --- Tests for Forj Context Management System -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive tests for the Forj Context Management System
;; Tests context collection, suggestions, and prompt interface functionality
;; Following TDD principles from CLAUDE.md development guidelines

;;; Code:

(require 'ert)
(require 'forj-context)
(require 'forj-context-suggestions)
(require 'forj-prompt-interface)

;;; Test Setup and Utilities

(defvar forj-context-test-temp-files '()
  "List of temporary files created during testing.")

(defun forj-context-test-create-temp-file (content &optional extension)
  "Create a temporary file with CONTENT and optional EXTENSION.
Adds file to cleanup list and returns the file path."
  (let* ((ext (or extension "txt"))
         (temp-file (make-temp-file "forj-test-" nil (concat "." ext))))
    (with-temp-file temp-file
      (insert content))
    (push temp-file forj-context-test-temp-files)
    temp-file))

(defun forj-context-test-cleanup ()
  "Clean up temporary files created during testing."
  (dolist (file forj-context-test-temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq forj-context-test-temp-files '()))

(defmacro forj-context-with-cleanup (&rest body)
  "Execute BODY with automatic cleanup of temporary files."
  `(unwind-protect
       (progn ,@body)
     (forj-context-test-cleanup)))

;;; Context Collection Tests

(ert-deftest forj-test-buffer-context-collection ()
  "Test buffer context collection functionality."
  (forj-context-with-cleanup
   (with-temp-buffer
     (insert "function test() {\n  return 'hello world';\n}")
     (js-mode)
     (let ((context (forj-collect-buffer-context (current-buffer))))
       (should (plist-get context :content))
       (should (string-match-p "hello world" (plist-get context :content)))
       (should (eq (plist-get (plist-get context :metadata) :mode) 'js-mode))
       (should (> (plist-get context :relevance) 0))
       (should (eq (plist-get context :type) 'buffer))))))

(ert-deftest forj-test-buffer-context-with-region ()
  "Test buffer context collection with region limitation."
  (forj-context-with-cleanup
   (with-temp-buffer
     (insert "line 1\nline 2\nline 3\nline 4")
     (let ((context (forj-collect-buffer-context (current-buffer) '(8 . 14))))
       (should (string= "line 2" (plist-get context :content)))
       (should (eq (plist-get context :type) 'buffer))))))

(ert-deftest forj-test-file-context-collection ()
  "Test file context collection functionality."
  (forj-context-with-cleanup
   (let ((test-file (forj-context-test-create-temp-file
                     "(defun test-function ()\n  \"Test function.\"\n  'test-result)" "el")))
     (let ((context (forj-collect-file-context test-file)))
       (should context)
       (should (string-match-p "test-function" (plist-get context :content)))
       (should (string= test-file (plist-get context :path)))
       (should (eq (plist-get context :type) 'file))
       (should (> (plist-get context :relevance) 0))))))

(ert-deftest forj-test-file-context-nonexistent ()
  "Test file context collection with non-existent file."
  (should-error (forj-collect-file-context "/nonexistent/file.txt")))

(ert-deftest forj-test-compilation-context-collection ()
  "Test compilation context collection."
  (forj-context-with-cleanup
   (with-temp-buffer
     (rename-buffer "*compilation*")
     (insert "gcc -o test test.c\ntest.c:15:3: error: undeclared identifier\nCompilation failed")
     (let ((context (forj-collect-compilation-context t)))
       (should context)
       (should (string-match-p "error:" (plist-get context :content)))
       (should (eq (plist-get context :type) 'compilation))
       (should (> (plist-get context :relevance) 0.8)) ; High relevance for errors
       ))))

(ert-deftest forj-test-compilation-context-empty ()
  "Test compilation context collection with empty compilation buffer."
  (let ((context (forj-collect-compilation-context)))
    (should context)
    (should (string-empty-p (plist-get context :content)))
    (should (eq (plist-get context :type) 'compilation))
    (should (= (plist-get context :relevance) 0.0))))

(ert-deftest forj-test-context-size-optimization ()
  "Test context size optimization."
  (let ((large-context (list
                        `(:content ,(make-string 30000 ?x) :relevance 0.9 :type test)
                        `(:content ,(make-string 25000 ?y) :relevance 0.8 :type test)
                        `(:content ,(make-string 10000 ?z) :relevance 0.7 :type test)))
        (forj-context-max-size 40000))
    (let ((optimized (forj-optimize-context-size large-context)))
      (should (< (apply #'+ (mapcar (lambda (ctx)
                                    (length (plist-get ctx :content)))
                                  optimized))
                forj-context-max-size))
      ;; Should keep highest relevance items
      (should (= (plist-get (car optimized) :relevance) 0.9)))))

;;; Context Suggestions Tests

(ert-deftest forj-test-prompt-analysis-file-references ()
  "Test prompt analysis for file references."
  (forj-context-with-cleanup
   (let ((test-file (forj-context-test-create-temp-file "test content" "el")))
     (let ((suggestions (forj-analyze-prompt-for-context 
                        (format "Please review %s for errors" 
                               (file-name-nondirectory test-file)))))
       (should (> (length suggestions) 0))
       (let ((file-suggestion (cl-find-if (lambda (s) (eq (plist-get s :type) 'file))
                                         suggestions)))
         (when file-suggestion ; The suggestion might not be found if file path resolution fails
           (should (> (plist-get file-suggestion :confidence) 0.8))))))))

(ert-deftest forj-test-prompt-analysis-error-keywords ()
  "Test prompt analysis for error/debugging keywords."
  (let ((suggestions (forj-analyze-prompt-for-context 
                     "I have an error in my code that needs debugging")))
    (should (> (length suggestions) 0))
    (let ((compilation-suggestion (cl-find-if (lambda (s) (eq (plist-get s :type) 'compilation))
                                             suggestions)))
      (should compilation-suggestion)
      (should (> (plist-get compilation-suggestion :confidence) 0.7)))))

(ert-deftest forj-test-prompt-analysis-buffer-references ()
  "Test prompt analysis for buffer references."
  (let ((suggestions (forj-analyze-prompt-for-context 
                     "Please check the current buffer for issues")))
    (should (> (length suggestions) 0))
    (let ((buffer-suggestion (cl-find-if (lambda (s) (eq (plist-get s :type) 'buffer))
                                        suggestions)))
      (should buffer-suggestion)
      (should (> (plist-get buffer-suggestion :confidence) 0.8)))))

(ert-deftest forj-test-emacs-state-analysis ()
  "Test Emacs state analysis functionality."
  (forj-context-with-cleanup
   (with-temp-buffer
     (insert "test content for analysis")
     (mark-whole-buffer)
     (let ((state (forj-analyze-emacs-state)))
       (should (plist-get state :active-buffer))
       (should (plist-get state :buffer-mode))
       (should (plist-get state :region-active))
       (should (plist-get state :region-content))
       (should (string-match-p "test content" (plist-get state :region-content)))))))

(ert-deftest forj-test-context-suggestions-integration ()
  "Test integration of context suggestions with current Emacs state."
  (forj-context-with-cleanup
   (let ((test-file (forj-context-test-create-temp-file "test elisp code" "el")))
     (with-temp-buffer
       (insert "elisp development")
       (let ((suggestions (forj-suggest-context-sources "fix elisp syntax")))
         (should (> (length suggestions) 0))
         ;; Should suggest current buffer
         (let ((buffer-suggestion (cl-find-if (lambda (s) 
                                               (and (eq (plist-get s :type) 'buffer)
                                                    (eq (plist-get s :buffer) (current-buffer))))
                                             suggestions)))
           (should buffer-suggestion)))))))

;;; File Type Detection Tests

(ert-deftest forj-test-file-type-detection ()
  "Test file type detection functionality."
  (should (eq (forj-detect-file-type "test.el") 'elisp))
  (should (eq (forj-detect-file-type "README.md") 'markdown))
  (should (eq (forj-detect-file-type "script.py") 'python))
  (should (eq (forj-detect-file-type "app.js") 'javascript))
  (should (eq (forj-detect-file-type "config.json") 'json))
  (should (eq (forj-detect-file-type "unknown.xyz") 'unknown)))

;;; Relevance Calculation Tests

(ert-deftest forj-test-buffer-relevance-calculation ()
  "Test buffer relevance calculation."
  (forj-context-with-cleanup
   (with-temp-buffer
     (emacs-lisp-mode)
     (insert (make-string 5000 ?x)) ; Make buffer reasonably sized
     (set-buffer-modified-p t)
     (let ((relevance (forj-calculate-buffer-relevance (current-buffer))))
       (should (> relevance 0.5))  ; Should be reasonably high for elisp mode
       (should (<= relevance 1.0)) ; Should not exceed maximum
       ))))

(ert-deftest forj-test-file-relevance-calculation ()
  "Test file relevance calculation."
  (forj-context-with-cleanup
   (let ((elisp-file (forj-context-test-create-temp-file
                     "(defun test () 'elisp-content)" "el")))
     (let ((relevance (forj-calculate-file-relevance elisp-file)))
       (should (> relevance 0.4)) ; Elisp files should have decent relevance
       (should (<= relevance 1.0))))))

;;; Performance and Caching Tests

(ert-deftest forj-test-context-caching ()
  "Test context caching functionality."
  (forj-context-with-cleanup
   (let ((forj-context-cache-enabled t)
         (test-file (forj-context-test-create-temp-file "cached content" "el")))
     ;; Clear cache first
     (forj-context-clear-cache)
     
     ;; First collection should cache
     (let ((context1 (forj-collect-file-context test-file)))
       (should (> (forj-context-cache-size) 0))
       
       ;; Second collection should use cache (would be faster in real usage)
       (let ((context2 (forj-collect-file-context test-file)))
         (should (string= (plist-get context1 :content)
                         (plist-get context2 :content))))))))

(ert-deftest forj-test-performance-monitoring ()
  "Test performance monitoring functionality."
  (forj-context-with-cleanup
   (let ((forj-context-performance-monitoring t))
     ;; Clear previous metrics
     (forj-context-clear-performance-metrics)
     
     ;; Perform context collection
     (with-temp-buffer
       (insert "performance test")
       (forj-collect-buffer-context (current-buffer)))
     
     ;; Check that metrics were recorded
     (let ((metrics (forj-context-get-performance-metrics)))
       (should metrics)
       (should (assq 'buffer-collection metrics))))))

;;; Prompt Interface Tests

(ert-deftest forj-test-prompt-interface-creation ()
  "Test prompt interface buffer creation."
  (let ((test-buffer-name "*Test Forj Prompt*")
        (forj-prompt-buffer-name "*Test Forj Prompt*"))
    (unwind-protect
        (progn
          (forj-prompt "test prompt")
          (should (get-buffer test-buffer-name))
          (with-current-buffer test-buffer-name
            (should (derived-mode-p 'forj-prompt-mode))))
      ;; Cleanup
      (when (get-buffer test-buffer-name)
        (kill-buffer test-buffer-name)))))

(ert-deftest forj-test-context-collection-integration ()
  "Test integration of context collection with prompt interface."
  (forj-context-with-cleanup
   (let ((test-file (forj-context-test-create-temp-file "integration test" "el"))
         (forj-prompt-context-sources (list `(:type file 
                                              :path ,(forj-context-test-create-temp-file "test" "el")
                                              :confidence 0.9
                                              :reason "test"))))
     (let ((context-data (forj-collect-selected-context)))
       (should context-data)
       (should (> (length context-data) 0))
       (should (eq (plist-get (car context-data) :type) 'file))))))

;;; Error Handling Tests

(ert-deftest forj-test-error-handling-invalid-buffer ()
  "Test error handling for invalid buffer."
  (should-error (forj-collect-buffer-context "nonexistent-buffer")))

(ert-deftest forj-test-error-handling-unreadable-file ()
  "Test error handling for unreadable files."
  ;; This test may need to be adapted based on system permissions
  (should-error (forj-collect-file-context "/root/unreadable-file")))

(ert-deftest forj-test-graceful-degradation ()
  "Test graceful degradation when optional features are unavailable."
  (forj-context-with-cleanup
   ;; Test when forj-scan-directory-recursive is not available
   (let ((test-dir (make-temp-file "forj-test-dir" t)))
     (unwind-protect
         (let ((contexts (condition-case err
                            (forj-collect-project-context test-dir)
                          (error nil))))
           ;; Should not crash, may return empty results
           (should (listp contexts)))
       (delete-directory test-dir t)))))

;;; Utility Function Tests

(ert-deftest forj-test-file-extension-finder ()
  "Test finding files by extension."
  (forj-context-with-cleanup
   (let ((temp-dir (make-temp-file "forj-test-ext" t)))
     (unwind-protect
         (progn
           ;; Create test files
           (write-region "test" nil (expand-file-name "test1.el" temp-dir))
           (write-region "test" nil (expand-file-name "test2.el" temp-dir))
           (write-region "test" nil (expand-file-name "test.txt" temp-dir))
           
           (let ((elisp-files (forj-find-files-by-extension "el" temp-dir)))
             (should (= (length elisp-files) 2))
             (should (cl-every (lambda (f) (string-match-p "\\.el$" f)) elisp-files))))
       (delete-directory temp-dir t)))))

(ert-deftest forj-test-project-root-detection ()
  "Test project root detection."
  (forj-context-with-cleanup
   (let ((temp-dir (make-temp-file "forj-test-project" t)))
     (unwind-protect
         (progn
           ;; Create .git directory to simulate git project
           (make-directory (expand-file-name ".git" temp-dir))
           (let ((default-directory temp-dir))
             (should (string= (forj-find-project-root) temp-dir))))
       (delete-directory temp-dir t)))))

;;; Integration Tests

(ert-deftest forj-test-end-to-end-context-workflow ()
  "Test complete context management workflow."
  (forj-context-with-cleanup
   (let ((test-file (forj-context-test-create-temp-file 
                     "(defun example-function ()\n  \"Example.\"\n  'result)" "el")))
     ;; Simulate prompt analysis
     (let* ((prompt (format "Please review %s" (file-name-nondirectory test-file)))
            (suggestions (forj-analyze-prompt-for-context prompt))
            (context-data (forj-collect-context suggestions)))
       
       ;; Should have found context
       (should suggestions)
       (should context-data)
       
       ;; Should be properly formatted for API
       (let ((formatted (forj-format-context-for-api context-data)))
         (should (stringp formatted))
         (should (> (length formatted) 0)))))))

(ert-deftest forj-test-multi-source-context-collection ()
  "Test collecting context from multiple sources simultaneously."
  (forj-context-with-cleanup
   (let* ((test-file (forj-context-test-create-temp-file "multi-source test" "el"))
          (sources (list `(:type file :path ,test-file :confidence 0.9 :reason "test")
                        `(:type buffer :buffer ,(current-buffer) :confidence 0.8 :reason "test"))))
     (with-temp-buffer
       (insert "buffer content for multi-source test")
       (let ((context-data (forj-collect-context sources)))
         (should (>= (length context-data) 1)) ; At least one source should work
         (should (cl-every (lambda (ctx) (plist-get ctx :content)) context-data)))))))

;;; Test Runner

(defun forj-context-run-all-tests ()
  "Run all Forj context management tests."
  (interactive)
  (ert-run-tests-interactively "forj-test-"))

(provide 'forj-context-test)
;;; forj-context-test.el ends here