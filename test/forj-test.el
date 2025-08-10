;;; forj-test.el --- Tests for forj.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'forj)

(ert-deftest forj-paren-check-valid ()
  "Test forj-paren-check with valid code."
  (should (equal (forj-paren-check "(defun hello () (message \"world\"))")
                 '(:status balanced))))

(ert-deftest forj-paren-check-unclosed-paren ()
  "Test forj-paren-check with unclosed parenthesis."
  (let ((result (forj-paren-check "(defun hello () (message \"world\"")))
    (should (eq (plist-get result :status) 'unbalanced))
    (should (eq (plist-get (plist-get result :error) :type) 'unclosed-opening))))

(ert-deftest forj-paren-check-extra-paren ()
  "Test forj-paren-check with extra closing parenthesis."
  (let ((result (forj-paren-check "(defun hello () (message \"world\")))")))
    (should (eq (plist-get result :status) 'unbalanced))
    (should (eq (plist-get (plist-get result :error) :type) 'unmatched-closing))))

(ert-deftest forj-paren-check-mismatched-paren ()
  "Test forj-paren-check with mismatched parentheses."
  (let ((result (forj-paren-check "(defun hello [) (message \"world\")]")))
    (should (eq (plist-get result :status) 'unbalanced))
    (should (eq (plist-get (plist-get result :error) :type) 'mismatched-closing))))

;; File Reading Tests
(ert-deftest forj-read-file-basic ()
  "Test forj-read-file with basic file reading."
  (with-temp-buffer
    (let ((test-file (make-temp-file "forj-test" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "Hello, World!\nThis is a test file."))
            (should (string= (forj-read-file test-file)
                            "Hello, World!\nThis is a test file.")))
        (when (file-exists-p test-file)
          (delete-file test-file))))))

(ert-deftest forj-read-file-with-size-limit ()
  "Test forj-read-file with size limit parameter."
  (with-temp-buffer
    (let ((test-file (make-temp-file "forj-test" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "This is a longer test file with more content than the limit"))
            (let ((result (forj-read-file test-file 10)))
              (should (= (length result) 10))
              (should (string= result "This is a "))))
        (when (file-exists-p test-file)
          (delete-file test-file))))))

(ert-deftest forj-read-file-nonexistent ()
  "Test forj-read-file with nonexistent file."
  (should-error (forj-read-file "/nonexistent/file.txt") :type 'file-error))

(ert-deftest forj-list-files-basic ()
  "Test forj-list-files with basic directory listing."
  (let ((test-dir (make-temp-file "forj-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file (expand-file-name "test1.el" test-dir)
            (insert "(defun test1 ())"))
          (with-temp-file (expand-file-name "test2.txt" test-dir)
            (insert "Hello World"))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# Test"))
          
          ;; Test basic listing
          (let ((files (forj-list-files test-dir)))
            (should (>= (length files) 3))
            (should (member "test1.el" (mapcar 'file-name-nondirectory files)))
            (should (member "test2.txt" (mapcar 'file-name-nondirectory files)))
            (should (member "README.md" (mapcar 'file-name-nondirectory files)))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-list-files-with-pattern ()
  "Test forj-list-files with pattern matching."
  (let ((test-dir (make-temp-file "forj-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file (expand-file-name "test1.el" test-dir)
            (insert "(defun test1 ())"))
          (with-temp-file (expand-file-name "test2.el" test-dir)
            (insert "(defun test2 ())"))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# Test"))
          
          ;; Test pattern matching
          (let ((el-files (forj-list-files test-dir "\\.el$")))
            (should (= (length el-files) 2))
            (should (cl-every (lambda (f) (string-match-p "\\.el$" f)) el-files))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-read-project-files-basic ()
  "Test forj-read-project-files with basic functionality."
  (let ((test-dir (make-temp-file "forj-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file (expand-file-name "test.el" test-dir)
            (insert "(defun hello-world ())"))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# Project README"))
          (with-temp-file (expand-file-name "notes.txt" test-dir)
            (insert "Some notes"))
          
          ;; Test reading project files
          (let ((default-directory test-dir))
            (let ((project-files (forj-read-project-files '("el" "md"))))
            (should (>= (length project-files) 2))
            (should (assoc-string "test.el" project-files))
            (should (assoc-string "README.md" project-files))
            (should (string= (cdr (assoc-string "test.el" project-files))
                            "(defun hello-world ())"))
            (should (string= (cdr (assoc-string "README.md" project-files))
                            "# Project README")))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-read-project-files-with-limits ()
  "Test forj-read-project-files with max-files limit."
  (let ((test-dir (make-temp-file "forj-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create multiple test files
          (dotimes (i 5)
            (with-temp-file (expand-file-name (format "test%d.el" i) test-dir)
              (insert (format "(defun test%d ())" i))))
          
          ;; Test with limit
          (let ((default-directory test-dir)
                (project-files (forj-read-project-files '("el") 3)))
            (should (<= (length project-files) 3))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

;; File Writing Tests
(ert-deftest forj-write-file-basic ()
  "Test forj-write-file with basic file writing."
  (let ((test-file (make-temp-file "forj-write-test" nil ".txt")))
    (unwind-protect
        (progn
          (let ((result (forj-write-file test-file "Hello, World!\nThis is a test.")))
            (should (plist-get result :success))
            (should (file-exists-p test-file))
            (should (string= (with-temp-buffer
                              (insert-file-contents test-file)
                              (buffer-string))
                            "Hello, World!\nThis is a test."))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest forj-write-file-with-backup ()
  "Test forj-write-file creates backups for existing files."
  (let ((test-file (make-temp-file "forj-write-test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create initial content
          (with-temp-file test-file
            (insert "Original content"))
          
          ;; Write new content and check backup
          (let ((result (forj-write-file test-file "New content")))
            (should (plist-get result :success))
            (should (plist-get result :backup-path))
            (should (file-exists-p (plist-get result :backup-path)))
            (should (string= (with-temp-buffer
                              (insert-file-contents (plist-get result :backup-path))
                              (buffer-string))
                            "Original content"))
            (should (string= (with-temp-buffer
                              (insert-file-contents test-file)
                              (buffer-string))
                            "New content"))))
      (when (file-exists-p test-file)
        (delete-file test-file))
      ;; Clean up backup files
      (dolist (backup-file (directory-files (file-name-directory test-file) t 
                                           (concat (file-name-nondirectory test-file) "\\.bak\\.")))
        (delete-file backup-file)))))

(ert-deftest forj-write-file-elisp-validation ()
  "Test forj-write-file validates Emacs Lisp files before writing."
  (let ((test-file (make-temp-file "forj-write-test" nil ".el")))
    (unwind-protect
        (progn
          ;; Test valid Emacs Lisp
          (let ((result (forj-write-file test-file "(defun test-func () \"Test function\")")))
            (should (plist-get result :success)))
          
          ;; Test invalid Emacs Lisp (should fail validation)
          (let ((result (forj-write-file test-file "(defun broken-func ( \"Unclosed paren\"")))
            (should-not (plist-get result :success))
            (should (plist-get result :error-message))))
      (when (file-exists-p test-file)
        (delete-file test-file))
      ;; Clean up backup files
      (dolist (backup-file (directory-files (file-name-directory test-file) t 
                                           (concat (file-name-nondirectory test-file) "\\.bak\\.")))
        (delete-file backup-file)))))

(ert-deftest forj-write-file-permissions ()
  "Test forj-write-file handles permission errors gracefully."
  (let ((readonly-dir (make-temp-file "forj-readonly-test" t)))
    (unwind-protect
        (progn
          (set-file-modes readonly-dir #o444) ; Read-only directory
          (let ((test-file (expand-file-name "test.txt" readonly-dir)))
            (let ((result (forj-write-file test-file "Test content")))
              (should-not (plist-get result :success))
              (should (plist-get result :error-message)))))
      (when (file-directory-p readonly-dir)
        (set-file-modes readonly-dir #o755) ; Restore permissions for cleanup
        (delete-directory readonly-dir t)))))

(ert-deftest forj-write-file-no-backup ()
  "Test forj-write-file with no-backup option."
  (let ((test-file (make-temp-file "forj-write-test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create initial content
          (with-temp-file test-file
            (insert "Original content"))
          
          ;; Write new content without backup
          (let ((result (forj-write-file test-file "New content" t)))
            (should (plist-get result :success))
            (should-not (plist-get result :backup-path))
            (should (string= (with-temp-buffer
                              (insert-file-contents test-file)
                              (buffer-string))
                            "New content"))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;; File Metadata Tests
(ert-deftest forj-file-metadata-basic ()
  "Test forj-file-metadata with basic file metadata extraction."
  (let ((test-file (make-temp-file "forj-metadata-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun hello () \"Test function\")"))
          (let ((metadata (forj-file-metadata test-file)))
            (should (plist-get metadata :path))
            (should (numberp (plist-get metadata :size)))
            (should (> (plist-get metadata :size) 0))
            (should (eq (plist-get metadata :type) 'elisp))
            (should (plist-get metadata :readable))
            (should (plist-get metadata :modified-time))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest forj-list-files-with-metadata-basic ()
  "Test forj-list-files-with-metadata returns metadata for all files."
  (let ((test-dir (make-temp-file "forj-metadata-dir" t)))
    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file (expand-file-name "test.el" test-dir)
            (insert "(defun test ())"))
          (with-temp-file (expand-file-name "readme.md" test-dir)
            (insert "# Test"))
          
          ;; Test metadata collection
          (let ((files-meta (forj-list-files-with-metadata test-dir)))
            (should (>= (length files-meta) 2))
            (dolist (meta files-meta)
              (should (plist-get meta :path))
              (should (plist-get meta :type))
              (should (numberp (plist-get meta :size))))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-scan-directory-recursive-basic ()
  "Test forj-scan-directory-recursive with nested directories."
  (let ((test-dir (make-temp-file "forj-scan-test" t)))
    (unwind-protect
        (progn
          ;; Create nested structure
          (make-directory (expand-file-name "subdir" test-dir))
          (with-temp-file (expand-file-name "main.el" test-dir)
            (insert "(defun main ())"))
          (with-temp-file (expand-file-name "subdir/sub.txt" test-dir)
            (insert "Sub file content"))
          
          ;; Test recursive scanning
          (let ((scan-results (forj-scan-directory-recursive test-dir 2 10)))
            (should (>= (length scan-results) 2))
            (should (cl-some (lambda (meta) 
                              (string-match-p "main\\.el$" (plist-get meta :path)))
                            scan-results))
            (should (cl-some (lambda (meta) 
                              (string-match-p "sub\\.txt$" (plist-get meta :path)))
                            scan-results))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-scan-directory-recursive-limits ()
  "Test forj-scan-directory-recursive respects depth and file limits."
  (let ((test-dir (make-temp-file "forj-limits-test" t)))
    (unwind-protect
        (progn
          ;; Create deep nested structure
          (make-directory (expand-file-name "level1/level2/level3" test-dir) t)
          (with-temp-file (expand-file-name "root.txt" test-dir)
            (insert "Root file"))
          (with-temp-file (expand-file-name "level1/level1.txt" test-dir)
            (insert "Level 1 file"))
          (with-temp-file (expand-file-name "level1/level2/level2.txt" test-dir)
            (insert "Level 2 file"))
          (with-temp-file (expand-file-name "level1/level2/level3/level3.txt" test-dir)
            (insert "Level 3 file"))
          
          ;; Test depth limit (should not include level3 file)
          (let ((scan-results (forj-scan-directory-recursive test-dir 2 10)))
            (should (cl-some (lambda (meta) 
                              (string-match-p "level2\\.txt$" (plist-get meta :path)))
                            scan-results))
            (should-not (cl-some (lambda (meta) 
                                  (string-match-p "level3\\.txt$" (plist-get meta :path)))
                                scan-results)))
          
          ;; Test file limit
          (let ((scan-results (forj-scan-directory-recursive test-dir 5 2)))
            (should (<= (length scan-results) 2))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

;; Integration Tests with Conversation System
(ert-deftest forj-browse-and-read-file-integration ()
  "Test forj-browse-and-read-file logs to conversation history."
  (let ((test-dir (make-temp-file "forj-integration-test" t))
        (forj-conversation-history nil)) ; Reset history
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file (expand-file-name "test.el" test-dir)
            (insert "(defun test-function () \"Integration test\")"))
          
          ;; Mock the file selection (simulate user choosing the file)
          (cl-letf (((symbol-function 'forj-browse-files)
                     (lambda (&optional dir)
                       (expand-file-name "test.el" test-dir))))
            (let ((content (forj-browse-and-read-file test-dir)))
              ;; Verify content was read
              (should (stringp content))
              (should (string-match-p "defun test-function" content))
              
              ;; Verify conversation history was updated
              (should (> (length forj-conversation-history) 0))
              (let ((last-entry (car (last forj-conversation-history))))
                (should (eq (plist-get last-entry :role) 'system))
                (should (string-match-p "Read file:" (plist-get last-entry :content)))))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-scan-and-display-project-integration ()
  "Test forj-scan-and-display-project logs comprehensive project info."
  (let ((test-dir (make-temp-file "forj-project-test" t))
        (forj-conversation-history nil)) ; Reset history
    (unwind-protect
        (progn
          ;; Create project structure
          (make-directory (expand-file-name "src" test-dir))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# Test Project"))
          (with-temp-file (expand-file-name "src/main.el" test-dir)
            (insert "(defun main () \"Main function\")"))
          (with-temp-file (expand-file-name "config.json" test-dir)
            (insert "{\"version\": \"1.0.0\"}"))
          
          ;; Run project scan
          (let ((results (forj-scan-and-display-project test-dir)))
            ;; Verify results
            (should (listp results))
            (should (> (length results) 2))
            
            ;; Verify conversation history
            (should (>= (length forj-conversation-history) 2))
            (let ((entries (mapcar (lambda (entry) (plist-get entry :content)) 
                                  forj-conversation-history)))
              ;; Should have scan start message
              (should (cl-some (lambda (msg) (string-match-p "Starting project scan" msg)) entries))
              ;; Should have completion summary
              (should (cl-some (lambda (msg) (string-match-p "Project scan complete" msg)) entries))
              ;; Should have file type breakdown
              (should (cl-some (lambda (msg) (string-match-p "File types:" msg)) entries)))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-activity-tracking-integration ()
  "Test activity tracking during file operations."
  (let ((test-file (make-temp-file "forj-activity-test" nil ".txt"))
        (forj-current-activity nil)) ; Reset activity
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Test file content for activity tracking"))
          
          ;; Test activity during file reading
          (forj-set-activity "Testing activity tracking")
          (should (string= forj-current-activity "Testing activity tracking"))
          
          ;; Test activity reset
          (forj-set-activity nil)
          (should (null forj-current-activity))
          
          ;; Test activity during file operations
          (cl-letf (((symbol-function 'forj-browse-files)
                     (lambda (&optional dir) test-file)))
            (forj-browse-and-read-file)
            ;; Activity should be reset after operation
            (should (null forj-current-activity))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;; Advanced File Operations Tests
(ert-deftest forj-edit-file-region-basic ()
  "Test forj-edit-file-region with basic region editing."
  (let ((test-file (make-temp-file "forj-region-test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create test file with multiple lines
          (with-temp-file test-file
            (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))
          
          ;; Edit middle region (lines 2-3)
          (let ((result (forj-edit-file-region test-file 2 3 "New Line 2\nNew Line 3")))
            (should (plist-get result :success))
            (should (plist-get result :backup-path))
            
            ;; Verify the edit
            (let ((new-content (forj-read-file test-file)))
              (should (string-match-p "Line 1\nNew Line 2\nNew Line 3\nLine 4\nLine 5" new-content)))))
      (when (file-exists-p test-file)
        (delete-file test-file))
      ;; Clean up backup files
      (dolist (backup-file (directory-files (file-name-directory test-file) t 
                                           (concat (file-name-nondirectory test-file) "\\.bak\\.")))
        (delete-file backup-file)))))

(ert-deftest forj-backup-and-restore-utilities ()
  "Test forj-backup-file and forj-restore-backup utilities."
  (let ((test-file (make-temp-file "forj-backup-test" nil ".txt"))
        (original-content "Original file content"))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file test-file
            (insert original-content))
          
          ;; Create backup
          (let ((backup-result (forj-backup-file test-file)))
            (should (plist-get backup-result :success))
            (should (plist-get backup-result :backup-path))
            (should (file-exists-p (plist-get backup-result :backup-path)))
            
            ;; Modify original file
            (with-temp-file test-file
              (insert "Modified content"))
            
            ;; Restore from backup
            (let ((restore-result (forj-restore-backup (plist-get backup-result :backup-path) test-file)))
              (should (plist-get restore-result :success))
              (should (numberp (plist-get restore-result :restored-size)))
              
              ;; Verify restoration
              (should (string= (forj-read-file test-file) original-content))
              
              ;; Clean up backup
              (delete-file (plist-get backup-result :backup-path)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest forj-file-locking-basic ()
  "Test forj-lock-file and forj-unlock-file basic functionality."
  (let ((test-file (make-temp-file "forj-lock-test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file test-file
            (insert "Test content"))
          
          ;; Lock the file
          (let ((lock-result (forj-lock-file test-file)))
            (should (plist-get lock-result :success))
            (should (plist-get lock-result :lock-id))
            (should (file-exists-p (concat test-file ".forj-lock")))
            
            ;; Unlock the file
            (let ((unlock-result (forj-unlock-file test-file (plist-get lock-result :lock-id))))
              (should (plist-get unlock-result :success))
              (should-not (file-exists-p (concat test-file ".forj-lock"))))))
      (when (file-exists-p test-file)
        (delete-file test-file))
      ;; Clean up lock files
      (when (file-exists-p (concat test-file ".forj-lock"))
        (delete-file (concat test-file ".forj-lock"))))))

(ert-deftest forj-file-modification-tracking ()
  "Test file modification tracking functionality."
  (let ((test-file (make-temp-file "forj-tracking-test" nil ".txt"))
        (forj-file-modifications (make-hash-table :test 'equal))) ; Reset tracking
    (unwind-protect
        (progn
          ;; Track a modification
          (let ((mod-info (forj-track-file-modification test-file 'write)))
            (should (plist-get mod-info :timestamp))
            (should (eq (plist-get mod-info :operation) 'write))
            
            ;; Check history
            (let ((history (forj-get-file-modification-history test-file)))
              (should (= (length history) 1))
              (should (eq (plist-get (car history) :operation) 'write)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest forj-git-integration-basic ()
  "Test basic Git integration functions."
  ;; Test git repo detection
  (let ((git-root (forj-in-git-repo-p)))
    (if git-root
        (progn
          ;; We're in a git repo, test Git functions
          (should (stringp git-root))
          
          ;; Test git status on existing file
          (let ((git-status (forj-git-file-status "forj.el")))
            (should (plist-get git-status :status))
            (should (memq (plist-get git-status :status) 
                         '(clean modified staged staged-and-modified untracked no-git)))))
      ;; Not in a git repo, should return no-git status
      (let ((git-status (forj-git-file-status "forj.el")))
        (should (eq (plist-get git-status :status) 'no-git))))))

;; Directory Operations Tests (1.4.3 requirements)
(ert-deftest forj-directory-operations-basic ()
  "Test forj-directory-operations function for current directory context."
  (let ((test-dir (make-temp-file "forj-dir-ops-test" t)))
    (unwind-protect
        (progn
          ;; Create test project structure
          (with-temp-file (expand-file-name "test.el" test-dir)
            (insert ";; Test elisp file\n(defun hello-world () \"Hello\")\n"))
          (with-temp-file (expand-file-name "package.el" test-dir)
            (insert ";;; Package: test-package\n"))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# Test Project\n"))
          (make-directory (expand-file-name ".git" test-dir))
          
          ;; Test directory operations
          (let ((default-directory test-dir))
            (let ((dir-context (forj-directory-operations)))
              (should (plist-get dir-context :directory))
              (should (plist-get dir-context :files))
              (should (plist-get dir-context :project-type))
              (should (plist-get dir-context :file-count))
              
              ;; Should detect elisp project type
              (should (eq (plist-get dir-context :project-type) 'elisp-package))
              
              ;; Should exclude .git directory
              (let ((files (plist-get dir-context :files)))
                (should-not (cl-some (lambda (f) (string-match-p "\\.git" f)) files)))
              
              ;; Should include supported file types
              (let ((file-names (mapcar 'file-name-nondirectory (plist-get dir-context :files))))
                (should (member "test.el" file-names))
                (should (member "README.md" file-names))))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest forj-directory-operations-project-detection ()
  "Test project type detection in forj-directory-operations."
  ;; Test general project detection
  (let ((test-dir (make-temp-file "forj-general-test" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "main.py" test-dir)
            (insert "print('hello world')"))
          (with-temp-file (expand-file-name "README.md" test-dir)
            (insert "# General Project"))
          
          (let ((default-directory test-dir))
            (let ((dir-context (forj-directory-operations)))
              (should (eq (plist-get dir-context :project-type) 'general)))))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(provide 'forj-test)
;;; forj-test.el ends here