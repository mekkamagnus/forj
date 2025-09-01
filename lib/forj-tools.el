;;; forj-tools.el --- Tool dispatcher for Forj coding agent -*- lexical-binding: t -*-

;; Copyright (C) 2024 Forj.el Contributors
;; Author: Forj.el Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: ai, tools, coding

;;; Commentary:
;; Implements a minimal tool registry/dispatcher and handlers for core
;; coding-agent tools specified in specs/003-coding-agent-tools-specs.md.
;; Focus: safety, project sandboxing, and approval gating.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup forj-tools nil
  "Tooling and dispatcher for Forj coding agent."
  :group 'forj
  :prefix "forj-tools-")

(defcustom forj-tools-max-results 200
  "Default maximum results for search and listings."
  :type 'integer 
  :group 'forj-tools)

(defcustom forj-tools-approve-destructive t
  "Whether destructive actions require explicit approval."
  :type 'boolean 
  :group 'forj-tools)

(defcustom forj-tools-project-root nil
  "Project root. When nil, defaults to `default-directory`."
  :type '(choice (const :tag "Auto (default-directory)" nil) directory)
  :group 'forj-tools)

;; Helper functions
(defun forj--project-root ()
  "Return normalized project root directory."
  (file-name-as-directory
   (expand-file-name (or forj-tools-project-root default-directory))))

(defun forj--within-project-p (path)
  "Return non-nil if PATH is within the project root."
  (let* ((root (forj--project-root))
         (abs (expand-file-name path)))
    (string-prefix-p root (file-name-as-directory (file-name-directory abs)))))

(defun forj--normalize-path (path)
  "Expand and validate PATH is within project root, else signal error."
  (let* ((abs (expand-file-name path))
         (root (forj--project-root)))
    (if (or (string-prefix-p root abs)
            (string-prefix-p root (file-name-as-directory abs)))
        abs
      (error "validation-error: path outside project root: %s" path))))

(defun forj--maybe-require-approval (action meta)
  "Check if ACTION requires approval and return error if not approved."
  (when (and forj-tools-approve-destructive
             (memq action '(write_file edit_region multi_edit run_shell)))
    ;; For now, return nil (approved). In full implementation, this would
    ;; show approval dialog and wait for user response
    nil))

(defun forj--gen-id ()
  "Generate a unique ID string."
  (format "%d-%d" (truncate (float-time)) (random 10000)))

;; JSON utilities
(defun forj--json-decode (s)
  "Decode JSON string S to alist."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string s)))

(defun forj--json-encode (obj)
  "Encode OBJ to JSON string."
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

(defun forj--tool-result (id name ok payload)
  "Create a tool-result JSON structure."
  (forj--json-encode
   `((id . ,id)
     (name . ,name)
     (ok . ,ok)
     ,(if ok
          `(result . ,payload)
        `(error . ,payload)))))

;; Tool registry
(defvar forj-tools--registry (make-hash-table :test 'equal)
  "Registry of available tools.")

(defun forj-tools-register (name fn)
  "Register a tool NAME handled by FN."
  (puthash name fn forj-tools--registry))

(defun forj-tools-list ()
  "Return list of registered tool names."
  (let (tools)
    (maphash (lambda (k _v) (push k tools)) forj-tools--registry)
    tools))

;; Core tool implementations
(defun forj-tools--read-file (args _meta)
  "Read file tool implementation."
  (let* ((path (alist-get 'path args))
         (max-bytes (or (alist-get 'max_bytes args) 100000))
         (start-line (alist-get 'start_line args))
         (end-line (alist-get 'end_line args)))
    (unless path (error "validation-error: path required"))
    (let ((normalized-path (forj--normalize-path path)))
      (unless (file-exists-p normalized-path)
        (error "file-error: file not found: %s" path))
      (with-temp-buffer
        (insert-file-contents normalized-path nil nil max-bytes)
        (let* ((content (if (and start-line end-line)
                           (let ((lines (split-string (buffer-string) "\n")))
                             (string-join 
                              (cl-subseq lines (1- start-line) end-line) "\n"))
                         (buffer-string)))
               (truncated (> (nth 7 (file-attributes normalized-path)) max-bytes))
               (size (nth 7 (file-attributes normalized-path))))
          `((path . ,path)
            (encoding . "utf-8")
            (content . ,content)
            (truncated . ,truncated)
            (size . ,size)))))))

(defun forj-tools--list-files (args _meta)
  "List files tool implementation."
  (let* ((directory (or (alist-get 'directory args) "."))
         (max-depth (or (alist-get 'max_depth args) 5))
         (include-hidden (alist-get 'include_hidden args))
         (follow-symlinks (alist-get 'follow_symlinks args)))
    (let ((normalized-dir (forj--normalize-path directory)))
      (unless (file-directory-p normalized-dir)
        (error "file-error: directory not found: %s" directory))
      (let ((files '())
            (visited-dirs (make-hash-table :test 'equal)))
        (cl-labels ((collect-files (dir depth)
                      (when (and (<= depth max-depth)
                                 (not (gethash (file-truename dir) visited-dirs)))
                        (puthash (file-truename dir) t visited-dirs)
                        (dolist (file (directory-files dir t))
                          (let ((basename (file-name-nondirectory file)))
                            (unless (or (string= basename ".") 
                                      (string= basename "..")
                                      (and (not include-hidden) 
                                           (string-prefix-p "." basename)))
                              (let* ((attrs (file-attributes file follow-symlinks))
                                     (is-dir (eq t (car attrs)))
                                     (size (nth 7 attrs))
                                     (mtime (time-convert (nth 5 attrs) 'integer)))
                                (push `((path . ,(file-relative-name file (forj--project-root)))
                                       (size . ,(if is-dir 0 (or size 0)))
                                       (mtime . ,mtime)
                                       (type . ,(if is-dir "directory" "file")))
                                     files)
                                (when (and is-dir (< depth max-depth))
                                  (collect-files file (1+ depth))))))))))
          (collect-files normalized-dir 1))
        files))))

(defun forj-tools--write-file (args meta)
  "Write file tool implementation."
  (let* ((path (alist-get 'path args))
         (content (alist-get 'content args))
         (no-backup (alist-get 'no_backup args))
         (append-mode (alist-get 'append args))
         (create-if-missing (or (alist-get 'create_if_missing args) t))
         (dry-run (alist-get 'dry_run args)))
    (unless path (error "validation-error: path required"))
    (unless content (error "validation-error: content required"))
    (let* ((normalized-path (forj--normalize-path path))
           (approval-error (and (not dry-run)
                               (forj--maybe-require-approval 'write_file meta))))
      (when approval-error (cl-return-from forj-tools--write-file approval-error))
      (when (and (not create-if-missing) (not (file-exists-p normalized-path)))
        (error "file-error: file does not exist: %s" path))
      ;; Validate .el files with parentheses checker if available
      (when (and (string-suffix-p ".el" path)
                 (fboundp 'forj-paren-checker))
        (with-temp-buffer
          (if append-mode
              (when (file-exists-p normalized-path)
                (insert-file-contents normalized-path))
            ;; For new/overwrite, start fresh
            )
          (goto-char (point-max))
          (insert content)
          (let ((check-result (forj-paren-checker)))
            (unless (string= check-result "Balanced")
              (error "validation-error: parentheses check failed: %s" check-result)))))
      (if dry-run
          `((path . ,path)
            (backup_path . nil)
            (bytes_written . 0)
            (dry_run . t)
            (preview . ,(substring content 0 (min 200 (length content)))))
        (let* ((backup-path (unless no-backup
                             (format "%s.backup.%d" normalized-path (truncate (float-time)))))
               (bytes-written 0))
          ;; Create backup if needed
          (when (and (not no-backup) (file-exists-p normalized-path))
            (copy-file normalized-path backup-path))
          ;; Write content
          (if append-mode
              (with-temp-buffer
                (insert content)
                (setq bytes-written (length content))
                (append-to-file (point-min) (point-max) normalized-path))
            (with-temp-file normalized-path
              (insert content)
              (setq bytes-written (length content))))
          `((path . ,path)
            (backup_path . ,backup-path)
            (bytes_written . ,bytes-written)
            (dry_run . nil)))))))

(defun forj-tools--search (args _meta)
  "Search tool implementation."
  (let* ((query (alist-get 'query args))
         (is-regex (alist-get 'is_regex args))
         (case-sensitive (alist-get 'case_sensitive args))
         (include-paths (alist-get 'include_paths args))
         (exclude-paths (alist-get 'exclude_paths args))
         (max-results (or (alist-get 'max_results args) forj-tools-max-results)))
    (unless query (error "validation-error: query required"))
    (let ((results '())
          (count 0)
          (case-fold-search (not case-sensitive)))
      (cl-block search-loop
        (dolist (file (forj-tools--list-files '((max_depth . 10)) nil))
          (when (>= count max-results) (cl-return-from search-loop))
        (let ((file-path (alist-get 'path file)))
          (when (and (string= (alist-get 'type file) "file")
                     (or (not include-paths) 
                         (cl-some (lambda (p) (string-match-p p file-path)) include-paths))
                     (not (cl-some (lambda (p) (string-match-p p file-path)) 
                                  (or exclude-paths '()))))
            (condition-case nil
                (with-temp-buffer
                  (insert-file-contents (expand-file-name file-path (forj--project-root)))
                  (goto-char (point-min))
                  (let ((line-num 1))
                    (while (and (< count max-results)
                               (if is-regex 
                                   (re-search-forward query nil t)
                                 (search-forward query nil t)))
                      (let* ((match-end (point))
                             (match-start (match-beginning 0))
                             (line-start (line-beginning-position))
                             (line-end (line-end-position))
                             (column (- match-start line-start))
                             (match-text (buffer-substring match-start match-end))
                             (before (buffer-substring line-start match-start))
                             (after (buffer-substring match-end line-end)))
                        (push `((path . ,file-path)
                               (line . ,line-num)
                               (column . ,column)
                               (match_text . ,match-text)
                               (before . ,before)
                               (after . ,after))
                              results)
                        (cl-incf count))
                      (forward-line 1)
                      (cl-incf line-num))))
              (error nil)))))) ; Ignore files that can't be read
      (nreverse results))))

(defun forj-tools--run-shell (args meta)
  "Run shell command tool implementation."
  (let* ((cmd (alist-get 'cmd args))
         (cwd (or (alist-get 'cwd args) (forj--project-root)))
         (timeout (or (alist-get 'timeout args) 30))
         (env (alist-get 'env args))
         (stdin (alist-get 'stdin args)))
    (unless cmd (error "validation-error: cmd required"))
    (unless (listp cmd) (error "validation-error: cmd must be array"))
    (let* ((approval-error (forj--maybe-require-approval 'run_shell meta))
           (normalized-cwd (forj--normalize-path cwd)))
      (when approval-error (cl-return-from forj-tools--run-shell approval-error))
      (let* ((default-directory normalized-cwd)
             (process-environment (if env
                                     (append (mapcar (lambda (pair) 
                                                      (format "%s=%s" (car pair) (cdr pair))) 
                                                    env)
                                            process-environment)
                                   process-environment))
             (start-time (float-time))
             (result (with-temp-buffer
                      (let ((exit-code (apply #'call-process 
                                             (car cmd) stdin t nil (cdr cmd))))
                        (list exit-code (buffer-string)))))
             (duration-ms (round (* 1000 (- (float-time) start-time)))))
        `((exit_code . ,(car result))
          (stdout . ,(cadr result))
          (stderr . "")  ; call-process combines stdout/stderr
          (duration_ms . ,duration-ms))))))

;; Simplified implementations for other tools
(defun forj-tools--edit-region (args meta)
  "Edit region tool implementation - simplified."
  (error "edit_region not implemented yet"))

(defun forj-tools--glob (args _meta)
  "Glob tool implementation - simplified."
  (error "glob not implemented yet"))

(defun forj-tools--todo (args _meta)
  "Todo write tool implementation - simplified."
  (error "todo_write not implemented yet"))

(defun forj-tools--task (args _meta)
  "Task tool implementation - simplified."
  (error "task not implemented yet"))

(defun forj-tools--multi-edit (args meta)
  "Multi edit tool implementation - simplified."
  (error "multi_edit not implemented yet"))

;; Dispatcher
(defun forj-tools-dispatch (json-call)
  "Dispatch a single tool-call JSON and return a tool-result JSON."
  (condition-case err
      (let* ((call (forj--json-decode json-call))
             (id (alist-get 'id call))
             (name (alist-get 'name call))
             (args (alist-get 'args call))
             (meta (alist-get 'meta call))
             (fn (gethash name forj-tools--registry)))
        (unless fn (error "validation-error: unknown tool: %s" name))
        (let ((payload (funcall fn args meta)))
          (if (and (listp payload) (plist-get payload :error))
              (forj--tool-result id name nil (plist-get payload :error))
            (forj--tool-result id name t payload))))
    (error
     (forj--tool-result (or (ignore-errors (alist-get 'id (forj--json-decode json-call))) "")
                        (or (ignore-errors (alist-get 'name (forj--json-decode json-call))) "")
                        nil
                        `((type . "exception")
                          (message . ,(error-message-string err)))))))

;; Special tool for getting current directory (for testing)
(defun forj-tools--get-current-directory (_args _meta)
  "Get current directory tool implementation."
  `((directory . ,(forj--project-root))
    (absolute_path . ,(expand-file-name (forj--project-root)))))

;; Register built-in tools
(forj-tools-register "read_file" #'forj-tools--read-file)
(forj-tools-register "list_files" #'forj-tools--list-files)
(forj-tools-register "write_file" #'forj-tools--write-file)
(forj-tools-register "search" #'forj-tools--search)
(forj-tools-register "run_shell" #'forj-tools--run-shell)
(forj-tools-register "edit_region" #'forj-tools--edit-region)
(forj-tools-register "glob" #'forj-tools--glob)
(forj-tools-register "todo_write" #'forj-tools--todo)
(forj-tools-register "task" #'forj-tools--task)
(forj-tools-register "multi_edit" #'forj-tools--multi-edit)
(forj-tools-register "get_current_directory" #'forj-tools--get-current-directory)

(provide 'forj-tools)

;;; forj-tools.el ends here