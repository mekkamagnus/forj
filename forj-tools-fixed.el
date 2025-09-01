;;; forj-tools-fixed.el --- Tool dispatcher for Forj coding agent -*- lexical-binding: t -*-

;;; Commentary:
;; Implements a minimal tool registry/dispatcher and handlers for core
;; coding-agent tools specified in specs/003-coding-agent-tools-specs.md.
;; Focus: safety, project sandboxing, and approval gating.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'forj-paren-checker)

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
  (let* ((root (forj--project-root))
         (abs (expand-file-name path root)))
    (let ((abs-dir (file-name-as-directory (file-name-directory abs))))
      (if (string-prefix-p root abs-dir)
          abs
        (error "validation-error: path outside project root: %s (root: %s, abs-dir: %s)" path root abs-dir)))))

(defun forj--validate-elisp-content (content path)
  "Validate CONTENT as Emacs Lisp if PATH is .el file.
Return nil if valid, error plist if invalid."
  (when (string-match-p "\\.el$" path)
    (let ((check-result (forj-paren-check content)))
      (unless (eq (plist-get check-result :status) 'valid)
        (list :ok nil
              :error (list :type 'validation-error
                          :message (format "Elisp validation failed: %s"
                                         (plist-get check-result :message))
                          :details (plist-get check-result :errors)))))))

(defun forj--maybe-require-approval (action meta)
  "Return plist error if ACTION requires approval and META lacks it."
  (when forj-tools-approve-destructive
    (let* ((approved (when (and meta (listp meta))
                       (alist-get 'approved meta)))
           (destructive (memq action '(write_file edit_region multi_edit run_shell todo_write_export task_export))))
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
         (cons 'ok ok)
         (cons (if ok 'result 'error) payload))))

;; Basic tool implementations for testing
(defun forj-tools--read-file (args _meta)
  "Read file tool implementation."
  (let* ((path (forj--normalize-path (alist-get 'path args)))
         (max-bytes (or (alist-get 'max_bytes args) 100000)))
    (if (file-exists-p path)
        (list (cons 'path path)
              (cons 'content (with-temp-buffer
                              (insert-file-contents path nil 0 max-bytes)
                              (buffer-string)))
              (cons 'size (nth 7 (file-attributes path))))
      (error "File not found: %s" path))))

(defun forj-tools--write-file (args meta)
  "Write file tool implementation."
  (let* ((path (forj--normalize-path (alist-get 'path args)))
         (content (or (alist-get 'content args) ""))
         (dry-run (alist-get 'dry_run args))
         (validation-error (forj--validate-elisp-content content path))
         (approval-error (and (not dry-run)
                              (forj--maybe-require-approval 'write_file meta))))
    (when validation-error 
      (cl-return-from forj-tools--write-file validation-error))
    (when approval-error 
      (cl-return-from forj-tools--write-file approval-error))
    (if dry-run
        (list (cons 'dry_run t)
              (cons 'preview (format "Would write %d bytes to %s" (length content) path)))
      (with-temp-file path
        (insert content))
      (list (cons 'path path)
            (cons 'bytes_written (length content))))))

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

;; Register basic tools
(forj-tools-register "read_file" #'forj-tools--read-file)
(forj-tools-register "write_file" #'forj-tools--write-file)

(provide 'forj-tools-fixed)

;;; forj-tools-fixed.el ends here