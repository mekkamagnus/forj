;;; forj-tools.el --- Tool dispatcher for Forj coding agent -*- lexical-binding: t -*-

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
  :type 'integer :group 'forj-tools)

(defcustom forj-tools-approve-destructive t
  "Whether destructive actions require explicit approval."
  :type 'boolean :group 'forj-tools)

(defcustom forj-tools-project-root nil
  "Project root. When nil, defaults to `default-directory`."
  :type '(choice (const :tag "Auto (default-directory)" nil) directory)
  :group 'forj-tools)

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
    (if (string-prefix-p root (file-name-as-directory (file-name-directory abs)))
        abs
      (error "validation-error: path outside project root: %s" path))))

(defun forj--maybe-require-approval (action meta)
  "Return plist error if ACTION requires approval and META lacks it."
  (when forj-tools-approve-destructive
    (let* ((approved (when (and meta (listp meta))
                       (alist-get 'approved meta)))
           (destructive (memq action '(write_file edit_region multi_edit run_shell_export todo_write_export task_export))))
      (when (and destructive (not approved))
        (list :ok nil :error (list :type 'approval-required
                                   :message (format "Approval required for %s" action))))))
  nil)

;; Registry
(defvar forj-tools--registry (make-hash-table :test 'equal))

(defun forj-tools-register (name fn)
  "Register a tool NAME handled by FN."
  (puthash name fn forj-tools--registry))

(defun forj--json-decode (s)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string s)))

(defun forj--json-encode (obj)
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

(defun forj--tool-result (id name ok &optional payload)
  (forj--json-encode
   (list (cons 'id id)
         (cons 'name name)
         (cons 'ok (and ok t))
         (cons (if ok 'result 'error) payload))))

;; Utilities
(defun forj--read-file-bytes (path &optional max-bytes)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path nil 0 max-bytes))
    (buffer-string)))

(defun forj--file-metadata (path)
  (let* ((attrs (file-attributes path))
         (size (nth 7 attrs))
         (mtime (float-time (nth 5 attrs)))
         (type (cond ((string-match-p "\\.el$" path) "elisp")
                     ((string-match-p "\\.md$" path) "markdown")
                     ((string-match-p "\\.org$" path) "org")
                     (t "unknown"))))
    (list (cons 'path path)
          (cons 'size size)
          (cons 'mtime (truncate mtime))
          (cons 'type type)))

;; Tool handlers
(defun forj-tools--read-file (args _meta)
  (let* ((path (forj--normalize-path (alist-get 'path args)))
         (max (or (alist-get 'max_bytes args) 100000))
         (start (alist-get 'start_line args))
         (end (alist-get 'end_line args))
         (content (forj--read-file-bytes path max))
         (total (nth 7 (file-attributes path)))
         (encoding "utf-8")
         (truncated (> total (length content)))
         (range nil))
    (when (and start end (> start 0) (>= end start))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (goto-char (point-min))
          (forward-line end)
          (setq content (buffer-substring-no-properties beg (point))))
        (setq range (list (cons 'start_line start) (cons 'end_line end)))))
    (list (cons 'path path)
          (cons 'encoding encoding)
          (cons 'content content)
          (cons 'truncated truncated)
          (cons 'size total)
          (cons 'range range)))

(defun forj-tools--list-files (args _meta)
  (let* ((dir (expand-file-name (or (alist-get 'directory args) (forj--project-root))))
         (_ (unless (string-prefix-p (forj--project-root) (file-name-as-directory dir))
              (error "validation-error: directory outside project root")))
         (max-depth (or (alist-get 'max_depth args) 5))
         (results '())
         (queue (list (cons dir 0)))
         (count 0))
    (while (and queue (< count forj-tools-max-results))
      (pcase-let ((`(,cur . ,depth) (pop queue)))
        (dolist (f (directory-files cur t "^[^.].*" t))
          (cond
           ((file-directory-p f)
            (when (< depth max-depth)
              (push (cons f (1+ depth)) queue)))
           (t
            (when (forj--within-project-p f)
              (push (forj--file-metadata f) results)
              (setq count (1+ count))))))))
    (nreverse results)))

(defun forj-tools--write-file (args meta)
  (let* ((path (forj--normalize-path (alist-get 'path args)))
         (content (or (alist-get 'content args) ""))
         (dry (alist-get 'dry_run args))
         (approval-error (and (not dry)
                              (forj--maybe-require-approval 'write_file meta))))
    (when approval-error (cl-return-from forj-tools--write-file approval-error))
    (if dry
        (list :preview (format "Would write %d bytes to %s" (length content) path)
              :dry_run t :path path :bytes_written 0)
      (if (fboundp 'forj-write-file)
          (let ((res (forj-write-file path content)))
            (list :path path
                  :backup_path (plist-get res :backup-path)
                  :bytes_written (length content)
                  :dry_run nil))
        (with-temp-file path (insert content))
        (list :path path :bytes_written (length content) :dry_run nil))))

(defun forj--read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun forj-tools--edit-region (args meta)
  (let* ((path (forj--normalize-path (alist-get 'path args)))
         (start (alist-get 'start_line args))
         (end (alist-get 'end_line args))
         (new (or (alist-get 'new_content args) ""))
         (expected (alist-get 'expected args))
         (dry (alist-get 'dry_run args))
         (approval-error (and (not dry)
                              (forj--maybe-require-approval 'edit_region meta))))
    (when approval-error (cl-return-from forj-tools--edit-region approval-error))
    (if dry
        (let* ((lines (forj--read-lines path))
               (orig (mapconcat #'identity (cl-subseq lines (1- start) end) "\n"))
               (changed (not (string= orig new))))
          (when (and expected (not (string= expected orig)))
            (error "validation-error: expected text mismatch"))
          (list :path path :start_line start :end_line end
                :backup_path nil :dry_run t :changed changed))
      (when (and expected)
        (let* ((lines (forj--read-lines path))
               (orig (mapconcat #'identity (cl-subseq lines (1- start) end) "\n")))
          (unless (string= expected orig)
            (error "validation-error: expected text mismatch")))
      (if (fboundp 'forj-edit-file-region)
          (let ((res (forj-edit-file-region path start end new)))
            (list :path path :start_line start :end_line end
                  :backup_path (plist-get res :backup-path)
                  :dry_run nil :changed t))
        ;; Fallback naive implementation
        (let ((all (forj--read-file-bytes path)))
          (with-temp-buffer
            (insert all)
            (goto-char (point-min))
            (forward-line (1- start))
            (let ((rbeg (point)))
              (goto-char (point-min))
              (forward-line end)
              (delete-region rbeg (point))
              (goto-char rbeg)
              (insert new)
              (write-region (point-min) (point-max) path)))
          (list :path path :start_line start :end_line end :backup_path nil :dry_run nil :changed t)))))

(defun forj-tools--search (args _meta)
  (let* ((query (or (alist-get 'query args) ""))
         (is-regex (and (alist-get 'is_regex args) t))
         (case-fold (not (alist-get 'case_sensitive args)))
         (include (alist-get 'include_paths args))
         (exclude (alist-get 'exclude_paths args))
         (max (or (alist-get 'max_results args) forj-tools-max-results))
         (files (or include (mapcar (lambda (m) (alist-get 'path m))
                                    (forj-tools--list-files (list (cons 'directory (forj--project-root))) nil))))
         (matches '())
         (count 0))
    (dolist (p files)
      (when (and (< count max) (forj--within-project-p p)
                 (not (and exclude (cl-some (lambda (ex) (string-match-p ex p)) exclude))))
        (when (file-readable-p p)
          (with-temp-buffer
            (insert-file-contents p)
            (let ((case-fold-search case-fold))
              (goto-char (point-min))
              (while (and (< count max)
                          (if is-regex (re-search-forward query nil t)
                            (search-forward query nil t)))
                (let* ((pos (match-beginning 0))
                       (line (line-number-at-pos pos))
                       (col (1+ (- pos (line-beginning-position))))
                       (match-text (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
                       (before (buffer-substring-no-properties (line-beginning-position) (match-beginning 0)))
                       (after (buffer-substring-no-properties (match-end 0) (line-end-position))))
                  (push (list (cons 'path p) (cons 'line line) (cons 'column col)
                              (cons 'match_text match-text) (cons 'before before) (cons 'after after)) matches)
                  (setq count (1+ count)))))))))
    (nreverse matches)))

(defun forj-tools--glob (args _meta)
  (let* ((patterns (alist-get 'patterns args))
         (max (or (alist-get 'max_results args) 1000))
         (include-hidden (alist-get 'include_hidden args))
         (root (forj--project-root))
         (out '()))
    (dolist (pat patterns)
      (let* ((abs-pat (expand-file-name pat root))
             (files (file-expand-wildcards abs-pat t)))
        (dolist (f files)
          (when (and (forj--within-project-p f)
                     (or include-hidden (not (string-match-p "/\\." f))))
            (push (forj--file-metadata f) out)))))
    (cl-subseq (nreverse out) 0 (min (length out) max)))

(defun forj-tools--run-shell (args meta)
  (let* ((cmd (alist-get 'cmd args))
         (cwd (expand-file-name (or (alist-get 'cwd args) (forj--project-root))))
         (timeout (or (alist-get 'timeout args) 10))
         (stdin (alist-get 'stdin args))
         (approval-error (forj--maybe-require-approval 'run_shell meta)))
    (when approval-error (cl-return-from forj-tools--run-shell approval-error))
    (unless (string-prefix-p (forj--project-root) (file-name-as-directory cwd))
      (error "validation-error: cwd outside project root"))
    (unless (and (listp cmd) (> (length cmd) 0))
      (error "validation-error: cmd must be a non-empty array"))
    (let* ((prog (car cmd))
           (argsv (cdr cmd))
           (stdout-file (make-temp-file "forj-shell-out"))
           (stderr-file (make-temp-file "forj-shell-err"))
           (default-directory cwd)
           (start (current-time))
           (exit (apply #'call-process prog nil (list stdout-file stderr-file) nil argsv))
           (duration (truncate (* 1000 (float-time (time-subtract (current-time) start)))))
           (stdout (with-temp-buffer (insert-file-contents stdout-file) (buffer-string)))
           (stderr (with-temp-buffer (insert-file-contents stderr-file) (buffer-string))))
      (ignore-errors (delete-file stdout-file))
      (ignore-errors (delete-file stderr-file))
      (list :exit_code (or exit -1) :stdout stdout :stderr stderr :duration_ms duration)))

;; Simple in-memory task stores
(defvar forj--todo-store (make-hash-table :test 'equal))
(defvar forj--task-store (make-hash-table :test 'equal))

(defun forj--gen-id () (format "%08x" (random (expt 16 8))))

(defun forj-tools--todo (args meta)
  (let ((action (alist-get 'action args)))
    (pcase action
      ('export
       (let* ((path (forj--normalize-path (or (alist-get 'export_path args)
                                              "docs/TODO.md")))
              (approval-error (forj--maybe-require-approval 'todo_write_export meta)))
         (when approval-error (cl-return-from forj-tools--todo approval-error))
         (let ((items '()))
           (maphash (lambda (k v) (push (cons k v) items)) forj--todo-store)
           (with-temp-buffer
             (insert "# TODO\n\n")
             (dolist (it items)
               (let* ((id (car it)) (obj (cdr it)))
                 (insert (format "- [ %s ] %s  (id:%s)\n"
                                 (if (equal (alist-get 'status obj) "completed") "x" " ")
                                 (alist-get 'title obj) id)))))
           (if (fboundp 'forj-write-file)
               (progn (forj-write-file path (buffer-string)) (list :path path :count (hash-table-count forj--todo-store)))
             (with-temp-file path (insert (buffer-string)))
             (list :path path :count (hash-table-count forj--todo-store)))))
      ('create
       (let* ((id (forj--gen-id))
              (title (alist-get 'title args))
              (note (alist-get 'note args))
              (loc (alist-get 'location args))
              (labels (alist-get 'labels args))
              (task `((id . ,id) (title . ,title) (note . ,note)
                      (status . "open") (location . ,loc) (labels . ,labels)
                      (created_at . ,(truncate (float-time))) (updated_at . ,(truncate (float-time))))))
         (puthash id task forj--todo-store)
         (list :task task)))
      ((or 'update 'complete 'reopen 'delete 'list)
       (let ((id (alist-get 'id args)))
         (pcase action
           ('list (let (acc) (maphash (lambda (_ v) (push v acc)) forj--todo-store) (list :tasks acc)))
           ('delete (remhash id forj--todo-store) (list :deleted t :id id))
           ('complete (let ((t (gethash id forj--todo-store))) (when t (setf (alist-get 'status t) "completed")) (list :task t)))
           ('reopen (let ((t (gethash id forj--todo-store))) (when t (setf (alist-get 'status t) "open")) (list :task t)))
           ('update (let ((t (gethash id forj--todo-store)))
                      (when t
                        (dolist (k '(title note labels location))
                          (let ((v (alist-get k args))) (when v (setf (alist-get k t) v))))
                        (setf (alist-get 'updated_at t) (truncate (float-time))))
                      (list :task t))))))
      (_ (error "validation-error: unsupported todo action")))))

(defun forj-tools--task (args meta)
  (let ((action (alist-get 'action args)))
    (pcase action
      ('export
       (let* ((path (forj--normalize-path (or (alist-get 'export_path args)
                                              "docs/roadmap.md")))
              (approval-error (forj--maybe-require-approval 'task_export meta)))
         (when approval-error (cl-return-from forj-tools--task approval-error))
         (let ((items '()))
           (maphash (lambda (k v) (push (cons k v) items)) forj--task-store)
           (with-temp-buffer
             (insert "# Tasks\n\n")
             (dolist (it items)
               (let* ((id (car it)) (obj (cdr it)))
                 (insert (format "- [%s] %s (id:%s)\n"
                                 (alist-get 'status obj) (alist-get 'title obj) id)))))
           (if (fboundp 'forj-write-file)
               (progn (forj-write-file path (buffer-string)) (list :path path :count (hash-table-count forj--task-store)))
             (with-temp-file path (insert (buffer-string)))
             (list :path path :count (hash-table-count forj--task-store)))))
      ('create
       (let* ((id (forj--gen-id))
              (task `((id . ,id)
                      (title . ,(alist-get 'title args))
                      (description . ,(alist-get 'description args))
                      (acceptance . ,(alist-get 'acceptance args))
                      (links . ,(alist-get 'links args))
                      (labels . ,(alist-get 'labels args))
                      (priority . ,(alist-get 'priority args))
                      (status . "open")
                      (created_at . ,(truncate (float-time)))
                      (updated_at . ,(truncate (float-time))))))
         (puthash id task forj--task-store)
         (list :task task)))
      ('update (let ((id (alist-get 'id args)))
                 (let ((t (gethash id forj--task-store)))
                   (when t
                     (dolist (k '(title description acceptance links labels priority))
                       (let ((v (alist-get k args))) (when v (setf (alist-get k t) v))))
                     (setf (alist-get 'updated_at t) (truncate (float-time))))
                   (list :task t))))
      ('set_status (let* ((id (alist-get 'id args))
                          (t (gethash id forj--task-store)))
                     (when t (setf (alist-get 'status t) (alist-get 'status args)))
                     (list :task t)))
      ('complete (let ((id (alist-get 'id args)))
                   (let ((t (gethash id forj--task-store)))
                     (when t (setf (alist-get 'status t) "completed"))
                     (list :task t))))
      ('reopen (let ((id (alist-get 'id args)))
                 (let ((t (gethash id forj--task-store)))
                   (when t (setf (alist-get 'status t) "open"))
                   (list :task t))))
      ('add_subtask (let* ((id (alist-get 'id args))
                           (sub (alist-get 'subtask args))
                           (t (gethash id forj--task-store))
                           (subs (or (alist-get 'subtasks t) '())))
                      (let* ((sid (or (alist-get 'id sub) (forj--gen-id)))
                             (entry `((id . ,sid) (title . ,(alist-get 'title sub)) (done . ,(alist-get 'done sub)))))
                        (push entry subs)
                        (setf (alist-get 'subtasks t) subs))
                      (list :task t)))
      ('update_subtask (let* ((id (alist-get 'id args))
                              (sub (alist-get 'subtask args))
                              (t (gethash id forj--task-store))
                              (subs (or (alist-get 'subtasks t) '())))
                         (let ((sid (alist-get 'id sub)))
                           (setf (alist-get 'subtasks t)
                                 (mapcar (lambda (e)
                                           (if (equal (alist-get 'id e) sid)
                                               (progn
                                                 (dolist (k '(title done))
                                                   (let ((v (alist-get k sub)))
                                                     (when v (setf (alist-get k e) v))))
                                                 e)
                                             e)) subs)))
                         (list :task t)))
      ('add_note (let* ((id (alist-get 'id args))
                        (note (alist-get 'note args))
                        (t (gethash id forj--task-store))
                        (hist (or (alist-get 'history t) '())))
                   (push (list (cons 'note note) (cons 'ts (truncate (float-time)))) hist)
                   (setf (alist-get 'history t) hist)
                   (list :task t)))
      ('list (let (acc) (maphash (lambda (_ v) (push v acc)) forj--task-store) (list :tasks acc)))
      ('delete (let ((id (alist-get 'id args))) (remhash id forj--task-store) (list :deleted t :id id)))
      (_ (error "validation-error: unsupported task action")))))

(defun forj-tools--multi-edit (args meta)
  (let* ((edits (alist-get 'edits args))
         (dry (alist-get 'dry_run args))
         (stop (or (alist-get 'stop_on_error args) t))
         (approval-error (and (not dry)
                              (forj--maybe-require-approval 'multi_edit meta)))
         (applied 0) (skipped 0) (details '()))
    (when approval-error (cl-return-from forj-tools--multi-edit approval-error))
    (dolist (e edits)
      (condition-case err
          (let* ((payload (forj-tools--edit-region e (unless dry meta)))
                 (changed (plist-get payload :changed)))
            (push (append (list :changed changed) payload) details)
            (setq applied (1+ applied)))
        (error
         (push (list :error (error-message-string err)) details)
         (setq skipped (1+ skipped))
         (when stop (cl-return)))))
    (list :applied applied :skipped skipped :dry_run (and dry t) :details (nreverse details)))

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
          (if (and (listp payload) (plist-get payload :ok) (eq (plist-get payload :ok) nil))
              (forj--tool-result id name nil (plist-get payload :error))
            (forj--tool-result id name t payload))))
    (error
     (forj--tool-result (or (ignore-errors (alist-get 'id (forj--json-decode json-call))) "")
                        (or (ignore-errors (alist-get 'name (forj--json-decode json-call))) "")
                        nil
                        (list (cons 'type 'exception)
                              (cons 'message (error-message-string err)))))))

;; Register built-ins
(forj-tools-register 'read_file #'forj-tools--read-file)
(forj-tools-register 'list_files #'forj-tools--list-files)
(forj-tools-register 'write_file #'forj-tools--write-file)
(forj-tools-register 'edit_region #'forj-tools--edit-region)
(forj-tools-register 'search #'forj-tools--search)
(forj-tools-register 'run_shell #'forj-tools--run-shell)
(forj-tools-register 'run_tests (lambda (args _meta)
                                  (let ((cmd (alist-get 'command args)))
                                    (if (and cmd (listp cmd))
                                        (forj-tools--run-shell (list (cons 'cmd cmd)) nil)
                                      (list :error "runner not configured")))))
(forj-tools-register 'glob #'forj-tools--glob)
(forj-tools-register 'todo_write #'forj-tools--todo)
(forj-tools-register 'task #'forj-tools--task)
(forj-tools-register 'multi_edit #'forj-tools--multi-edit)

(provide 'forj-tools)

;;; forj-tools.el ends here

