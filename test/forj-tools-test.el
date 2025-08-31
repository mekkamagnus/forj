;;; test/forj-tools-test.el --- Tests for forj-tools dispatcher -*- lexical-binding: t -*-

(require 'ert)
(require 'json)
(require 'forj-tools)

(defun forj--json-decode-test (s)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string s)))

(defun forj--make-call (id name args &optional meta)
  (json-encode (list (cons 'id id)
                     (cons 'name name)
                     (cons 'args args)
                     (cons 'meta (or meta '((cwd . ".") (timeout . 5)))))))

(ert-deftest forj-test-tools-read-file-basic ()
  "read_file returns content and metadata."
  (let* ((file "tmp-tools-a.txt"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "hello world"))
          (let* ((call (forj--make-call "1" 'read_file `((path . ,file) (max_bytes . 10000))))
                 (resp (forj-tools-dispatch call))
                 (obj (forj--json-decode-test resp)))
            (should (alist-get 'ok obj))
            (let* ((res (alist-get 'result obj)))
              (should (string= (alist-get 'path res) (expand-file-name file)))
              (should (string= (alist-get 'encoding res) "utf-8"))
              (should (string-match "hello world" (alist-get 'content res)))
              (should (numberp (alist-get 'size res))))))
      (ignore-errors (delete-file file)))))

(ert-deftest forj-test-tools-sandbox-rejects-parent ()
  "Paths outside project root are rejected."
  (let* ((call (forj--make-call "2" 'read_file '((path . "../outside.txt"))))
         (resp (forj-tools-dispatch call))
         (obj (forj--json-decode-test resp)))
    (should (not (alist-get 'ok obj)))
    (let ((err (alist-get 'error obj)))
      (should (string-match "validation-error" (alist-get 'message err))))))

(ert-deftest forj-test-tools-write-file-requires-approval ()
  "write_file non-dry-run requires approval."
  (let* ((file "tmp-tools-b.txt")
         (call (forj--make-call "3" 'write_file `((path . ,file) (content . "data")) '((cwd . ".") (timeout . 5)))))
    (let* ((resp (forj-tools-dispatch call))
           (obj (forj--json-decode-test resp)))
      (should (not (alist-get 'ok obj))))))

(ert-deftest forj-test-tools-write-file-dry-run ()
  "write_file dry-run previews change."
  (let* ((file "tmp-tools-c.txt")
         (call (forj--make-call "4" 'write_file `((path . ,file) (content . "abc") (dry_run . t)))))
    (let* ((resp (forj-tools-dispatch call))
           (obj (forj--json-decode-test resp)))
      (should (alist-get 'ok obj))
      (let ((res (alist-get 'result obj)))
        (should (equal (alist-get 'dry_run res) t))
        (should (string-match "Would write" (alist-get 'preview res)))))))

(ert-deftest forj-test-tools-edit-region-dry-run-expected-mismatch ()
  "edit_region dry_run aborts on expected mismatch."
  (let* ((file "tmp-tools-d.txt"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "line1\nline2\nline3\n"))
          (let* ((args `((path . ,file) (start_line . 2) (end_line . 3)
                         (new_content . "X\nY\n") (expected . "DIFFERENT") (dry_run . t)))
                 (call (forj--make-call "5" 'edit_region args))
                 (resp (forj-tools-dispatch call))
                 (obj (forj--json-decode-test resp)))
            (should (not (alist-get 'ok obj)))
            (should (string-match "expected text mismatch" (alist-get 'message (alist-get 'error obj))))))
      (ignore-errors (delete-file file)))))

(ert-deftest forj-test-tools-search-basic ()
  "search finds matches in project files."
  (let* ((file "tmp-tools-e.txt"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "alpha beta gamma\nalpha\n"))
          (let* ((args '((query . "alpha") (is_regex . nil) (max_results . 5)))
                 (call (forj--make-call "6" 'search args))
                 (resp (forj-tools-dispatch call))
                 (obj (forj--json-decode-test resp)))
            (should (alist-get 'ok obj))
            (let ((matches (alist-get 'result obj)))
              (should (> (length matches) 0))
              (should (string= (alist-get 'path (car matches)) (expand-file-name file))))))
      (ignore-errors (delete-file file)))))

(ert-deftest forj-test-tools-todo-create-list ()
  "todo_write supports create and list."
  (let* ((create (forj--make-call "7" 'todo_write '((action . create) (title . "T1"))))
         (r1 (forj--json-decode-test (forj-tools-dispatch create)))
         (list-call (forj--make-call "8" 'todo_write '((action . list))))
         (r2 (forj--json-decode-test (forj-tools-dispatch list-call))))
    (should (alist-get 'ok r1))
    (should (alist-get 'ok r2))
    (should (> (length (alist-get 'tasks (alist-get 'result r2))) 0))))

(ert-deftest forj-test-tools-task-create-complete ()
  "task supports create and complete."
  (let* ((create (forj--make-call "9" 'task '((action . create) (title . "Feature A"))))
         (r1 (forj--json-decode-test (forj-tools-dispatch create)))
         (task (alist-get 'task (alist-get 'result r1)))
         (id (alist-get 'id task))
         (complete (forj--make-call "10" 'task `((action . complete) (id . ,id))))
         (r2 (forj--json-decode-test (forj-tools-dispatch complete))))
    (should (alist-get 'ok r1))
    (should (alist-get 'ok r2))
    (let ((task2 (alist-get 'task (alist-get 'result r2))))
      (should (string= (alist-get 'status task2) "completed")))))

;;; test/forj-tools-test.el ends here

