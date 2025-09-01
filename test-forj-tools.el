;;; test-forj-tools.el --- Tests for forj-tools dispatcher -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive ERT tests for forj-tools dispatcher and core tool handlers.

;;; Code:

(require 'ert)
(require 'forj-tools)
(require 'forj-paren-checker)

;; Test helpers
(defvar test-forj-tools--temp-dir nil)

(defun test-forj-tools--setup ()
  "Set up test environment."
  (setq test-forj-tools--temp-dir (make-temp-file "forj-tools-test" t))
  (setq forj-tools-project-root test-forj-tools--temp-dir))

(defun test-forj-tools--teardown ()
  "Clean up test environment."
  (when (and test-forj-tools--temp-dir (file-exists-p test-forj-tools--temp-dir))
    (delete-directory test-forj-tools--temp-dir t))
  (setq test-forj-tools--temp-dir nil
        forj-tools-project-root nil))

(defun test-forj-tools--create-file (path content)
  "Create test file at PATH with CONTENT."
  (let ((full-path (expand-file-name path test-forj-tools--temp-dir)))
    (make-directory (file-name-directory full-path) t)
    (with-temp-file full-path
      (insert content))
    full-path))

;; Paren checker tests
(ert-deftest forj-test-paren-check-valid ()
  "Test paren checker with valid code."
  (let ((result (forj-paren-check "(defun hello () (message \"Hello\"))")))
    (should (eq (plist-get result :status) 'valid))
    (should (eq (plist-get result :balanced) t))))

(ert-deftest forj-test-paren-check-unmatched ()
  "Test paren checker with unmatched parentheses."
  (let ((result (forj-paren-check "(defun hello () (message \"Hello\"})")))
    (should (eq (plist-get result :status) 'invalid))
    (should (eq (plist-get result :balanced) nil))
    (let ((errors (plist-get result :errors)))
      (should (= (length errors) 1))
      (should (eq (plist-get (car errors) :type) 'unmatched-paren)))))

(ert-deftest forj-test-paren-check-unclosed ()
  "Test paren checker with unclosed parentheses."
  (let ((result (forj-paren-check "(defun hello () (message \"Hello\"")))
    (should (eq (plist-get result :status) 'invalid))
    (should (eq (plist-get result :balanced) nil))
    (let ((errors (plist-get result :errors)))
      (should (>= (length errors) 1))
      (should (memq 'unclosed-paren (mapcar (lambda (e) (plist-get e :type)) errors))))))

(ert-deftest forj-test-paren-check-string-handling ()
  "Test paren checker ignores parens in strings."
  (let ((result (forj-paren-check "(message \"Hello (world)\")")))
    (should (eq (plist-get result :status) 'valid))
    (should (eq (plist-get result :balanced) t))))

;; Tool dispatcher tests
(ert-deftest forj-test-tools-dispatcher-basic ()
  "Test basic tool dispatch functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((call-json "{\"id\":\"test1\",\"name\":\"read_file\",\"args\":{\"path\":\"nonexistent.txt\"}}")
             (result-json (forj-tools-dispatch call-json))
             (result (json-read-from-string result-json)))
        (should (assoc 'id result))
        (should (equal (cdr (assoc 'id result)) "test1"))
        (should (assoc 'name result))
        (should (equal (cdr (assoc 'name result)) "read_file")))
    (test-forj-tools--teardown)))

;; Read file tests
(ert-deftest forj-test-tools-read-file-basic ()
  "Test read_file tool with basic file."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((content "Hello, World!")
             (path (test-forj-tools--create-file "test.txt" content))
             (args (list (cons 'path path)))
             (result (forj-tools--read-file args nil)))
        (should (equal (alist-get 'content result) content))
        (should (equal (alist-get 'path result) path))
        (should (equal (alist-get 'encoding result) "utf-8"))
        (should (not (alist-get 'truncated result))))
    (test-forj-tools--teardown)))

(ert-deftest forj-test-tools-read-file-line-range ()
  "Test read_file tool with line range."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((content "Line 1\nLine 2\nLine 3\nLine 4")
             (path (test-forj-tools--create-file "multiline.txt" content))
             (args (list (cons 'path path) (cons 'start_line 2) (cons 'end_line 3)))
             (result (forj-tools--read-file args nil)))
        (should (string-match-p "Line 2" (alist-get 'content result)))
        (should (alist-get 'range result)))
    (test-forj-tools--teardown)))

;; Write file tests
(ert-deftest forj-test-tools-write-file-dry-run ()
  "Test write_file tool with dry run."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((content "Test content")
             (path (expand-file-name "new-file.txt" test-forj-tools--temp-dir))
             (args (list (cons 'path path) (cons 'content content) (cons 'dry_run t)))
             (result (forj-tools--write-file args nil)))
        (should (alist-get 'dry_run result))
        (should (alist-get 'preview result))
        (should (not (file-exists-p path))))
    (test-forj-tools--teardown)))

(ert-deftest forj-test-tools-write-file-actual ()
  "Test write_file tool actual write."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((content "Test content")
             (path (expand-file-name "new-file.txt" test-forj-tools--temp-dir))
             (args (list (cons 'path path) (cons 'content content)))
             (meta (list (cons 'approved t)))
             (result (forj-tools--write-file args meta)))
        (should (not (alist-get 'dry_run result)))
        (should (= (alist-get 'bytes_written result) (length content)))
        (should (file-exists-p path))
        (should (equal (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))
                      content)))
    (test-forj-tools--teardown)))

;; Edit region tests
(ert-deftest forj-test-tools-edit-region-basic ()
  "Test edit_region tool basic functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((original "Line 1\nLine 2\nLine 3")
             (path (test-forj-tools--create-file "edit-test.txt" original))
             (args (list (cons 'path path)
                        (cons 'start_line 2)
                        (cons 'end_line 2)
                        (cons 'new_content "Modified Line 2")
                        (cons 'dry_run t)))
             (result (forj-tools--edit-region args nil)))
        (should (alist-get 'dry_run result))
        (should (alist-get 'changed result)))
    (test-forj-tools--teardown)))

;; List files tests
(ert-deftest forj-test-tools-list-files-basic ()
  "Test list_files tool basic functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (progn
        (test-forj-tools--create-file "file1.txt" "content1")
        (test-forj-tools--create-file "subdir/file2.txt" "content2")
        (let* ((args nil)
               (result (forj-tools--list-files args nil)))
          (should (listp result))
          (should (>= (length result) 2))))
    (test-forj-tools--teardown)))

;; Search tests
(ert-deftest forj-test-tools-search-basic ()
  "Test search tool basic functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (progn
        (test-forj-tools--create-file "search-test.txt" "Hello world\nThis is a test\nHello again")
        (let* ((args (list (cons 'query "Hello")))
               (result (forj-tools--search args nil)))
          (should (listp result))
          (should (>= (length result) 1))
          (let ((match (car result)))
            (should (alist-get 'path match))
            (should (alist-get 'line match))
            (should (equal (alist-get 'match_text match) "Hello")))))
    (test-forj-tools--teardown)))

;; Glob tests
(ert-deftest forj-test-tools-glob-basic ()
  "Test glob tool basic functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (progn
        (test-forj-tools--create-file "test1.txt" "content")
        (test-forj-tools--create-file "test2.txt" "content")
        (test-forj-tools--create-file "other.md" "content")
        (let* ((args (list (cons 'patterns '("*.txt"))))
               (result (forj-tools--glob args nil)))
          (should (listp result))
          (should (= (length result) 2))))
    (test-forj-tools--teardown)))

;; Shell execution tests
(ert-deftest forj-test-tools-run-shell-basic ()
  "Test run_shell tool basic functionality."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((args (list (cons 'cmd '("echo" "hello"))))
             (meta (list (cons 'approved t)))
             (result (forj-tools--run-shell args meta)))
        (should (= (alist-get 'exit_code result) 0))
        (should (string-match-p "hello" (alist-get 'stdout result))))
    (test-forj-tools--teardown)))

;; Multi-edit tests
(ert-deftest forj-test-tools-multi-edit-dry-run ()
  "Test multi_edit tool with dry run."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((path (test-forj-tools--create-file "multi-edit.txt" "Line 1\nLine 2\nLine 3"))
             (edits (list (list (cons 'path path)
                               (cons 'start_line 1)
                               (cons 'end_line 1)
                               (cons 'new_content "Modified Line 1"))
                         (list (cons 'path path)
                               (cons 'start_line 3)
                               (cons 'end_line 3)
                               (cons 'new_content "Modified Line 3"))))
             (args (list (cons 'edits edits) (cons 'dry_run t)))
             (result (forj-tools--multi-edit args nil)))
        (should (alist-get 'dry_run result))
        (should (= (alist-get 'applied result) 2))
        (should (= (alist-get 'skipped result) 0)))
    (test-forj-tools--teardown)))

;; Todo tests
(ert-deftest forj-test-tools-todo-create ()
  "Test todo_write tool create action."
  (let* ((args (list (cons 'action 'create) (cons 'title "Test task")))
         (result (forj-tools--todo args nil)))
    (should (alist-get 'task result))
    (let ((task (alist-get 'task result)))
      (should (alist-get 'id task))
      (should (equal (alist-get 'title task) "Test task"))
      (should (equal (alist-get 'status task) "open")))))

(ert-deftest forj-test-tools-todo-list ()
  "Test todo_write tool list action."
  ;; Clear store first
  (clrhash forj--todo-store)
  ;; Create a task
  (forj-tools--todo (list (cons 'action 'create) (cons 'title "Test task")) nil)
  ;; List tasks
  (let* ((args (list (cons 'action 'list)))
         (result (forj-tools--todo args nil)))
    (should (alist-get 'tasks result))
    (should (>= (length (alist-get 'tasks result)) 1))))

;; Task tests
(ert-deftest forj-test-tools-task-create ()
  "Test task tool create action."
  (let* ((args (list (cons 'action 'create) 
                    (cons 'title "Test project task")
                    (cons 'description "A test task")))
         (result (forj-tools--task args nil)))
    (should (alist-get 'task result))
    (let ((task (alist-get 'task result)))
      (should (alist-get 'id task))
      (should (equal (alist-get 'title task) "Test project task"))
      (should (equal (alist-get 'status task) "open")))))

;; Path validation tests
(ert-deftest forj-test-tools-path-validation ()
  "Test project root path validation."
  (test-forj-tools--setup)
  (unwind-protect
      (let ((outside-path "/tmp/outside-project.txt"))
        (should-error (forj--normalize-path outside-path)))
    (test-forj-tools--teardown)))

;; Approval gating tests
(ert-deftest forj-test-tools-approval-required ()
  "Test that destructive operations require approval."
  (test-forj-tools--setup)
  (unwind-protect
      (let* ((forj-tools-approve-destructive t)
             (path (expand-file-name "test.txt" test-forj-tools--temp-dir))
             (args (list (cons 'path path) (cons 'content "test")))
             (result (forj-tools--write-file args nil)))
        (should (not (plist-get result :ok)))
        (should (plist-get result :error)))
    (test-forj-tools--teardown)))

;; JSON parsing tests
(ert-deftest forj-test-tools-json-roundtrip ()
  "Test JSON encoding and decoding."
  (let* ((original (list (cons 'id "test") (cons 'name "read_file")))
         (encoded (forj--json-encode original))
         (decoded (forj--json-decode encoded)))
    (should (equal (alist-get 'id original) (alist-get 'id decoded)))
    (should (equal (alist-get 'name original) (alist-get 'name decoded)))))

(provide 'test-forj-tools)

;;; test-forj-tools.el ends here