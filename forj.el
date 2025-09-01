;;; forj.el --- AI co-pilot for Emacs Lisp development -*- lexical-binding: t -*-

(require 'cl-lib)

;; Load centralized error handling system
(let ((error-file (expand-file-name "lib/forj-error-system.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (if (file-exists-p error-file)
      (progn
        (message "Loading forj-error-system.el...")
        (load-file error-file))
    ;; Fallback: try require if file not found
    (when (locate-library "forj-error-system")
      (require 'forj-error-system))))

;; Load API integration module - always reload to get latest changes
(let ((api-file (expand-file-name "lib/forj-api.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (if (file-exists-p api-file)
      (progn
        (message "Loading forj-api.el...")
        (load-file api-file))
    ;; Fallback: try require if file not found
    (when (locate-library "forj-api")
      (require 'forj-api))))

;; Load Tools dispatcher (coding agent tools)
(let ((tools-file (expand-file-name "lib/forj-tools.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p tools-file)
    (condition-case err
        (progn
          (message "Loading forj-tools.el...")
          (load-file tools-file))
      (error
       (message "Warning: Failed to load forj-tools.el: %s" (error-message-string err))))))

;; Load Natural Language Query Interpreter (Specification 004)
(let ((query-file (expand-file-name "lib/forj-query-interpreter.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p query-file)
    (condition-case err
        (progn
          (message "Loading forj-query-interpreter.el...")
          (load-file query-file))
      (error
       (message "Warning: Failed to load forj-query-interpreter.el: %s" (error-message-string err))))))

;; Load Context Management System (Specification 002) in dependency order
(let ((context-loaded nil)
      (context-available nil)
      (context-suggestions-available nil))
  
  ;; Load forj-context.el first (base dependency)
  (let ((context-file (expand-file-name "lib/forj-context.el" (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p context-file)
      (condition-case err
          (progn
            (message "Loading forj-context.el...")
            (load-file context-file)
            (setq context-available t))
        (error
         (message "Warning: Failed to load forj-context.el: %s" (error-message-string err))))))
  
  ;; Load forj-context-suggestions.el (depends on forj-context)
  (let ((suggestions-file (expand-file-name "lib/forj-context-suggestions.el" (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p suggestions-file)
      (condition-case err
          (progn
            (message "Loading forj-context-suggestions.el...")
            (load-file suggestions-file)
            (setq context-suggestions-available t))
        (error
         (message "Warning: Failed to load forj-context-suggestions.el: %s" (error-message-string err))))))
  
  ;; Load forj-prompt-interface.el (depends on both previous modules)
  (let ((interface-file (expand-file-name "ui/forj-prompt-interface.el" (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p interface-file)
      (condition-case err
          (progn
            (message "Loading forj-prompt-interface.el...")
            (load-file interface-file)
            ;; Set availability flags in the loaded module
            (when (boundp 'forj-context-available)
              (set 'forj-context-available context-available))
            (when (boundp 'forj-context-suggestions-available)
              (set 'forj-context-suggestions-available context-suggestions-available))
            (setq context-loaded t))
        (error
         (message "Warning: Failed to load forj-prompt-interface.el: %s" (error-message-string err))))))
  
  ;; If context system failed to load, provide fallback forj-start function
  (unless context-loaded
    (message "Context system unavailable, providing fallback forj-start function")
    (defun forj-start ()
      "Launch Forj application (fallback version without context management)."
      (interactive)
      (let ((buffer (forj-conversation-buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n\n=== Forj.el Started (Basic Mode) ===\n")
            (insert "Context management system not available.\n")
            (insert "Use M-x forj-prompt for basic AI assistance.\n\n")))
        (display-buffer buffer)
        (message "Forj started in basic mode. Use M-x forj-prompt for AI assistance.")))))

;; Load UI integration system (Phase 1.6 UI/UX Enhancement)
(let ((ui-file (expand-file-name "ui/forj-ui-integration.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p ui-file)
    (condition-case err
        (progn
          (message "Loading forj UI integration system...")
          (load-file ui-file)
          ;; Initialize UI system if auto-setup is enabled
          (when (and (featurep 'forj-ui-integration) 
                     (boundp 'forj-ui-auto-setup)
                     forj-ui-auto-setup)
            (run-with-idle-timer 0.5 nil #'forj-ui-setup-main-integration)))
      (error
       (message "Warning: Failed to load forj UI system: %s" (error-message-string err))))))

;; Package info
(defgroup forj nil "AI co-pilot for Emacs." :prefix "forj-" :group 'tools)
(defcustom forj-conversation-buffer "*forj*" "Buffer name." :type 'string :group 'forj)
(defcustom forj-max-file-size 50000 "Maximum file size to read in bytes." :type 'integer :group 'forj)
(defcustom forj-supported-extensions '("el" "md" "txt" "org" "lisp") "List of file extensions to consider for project context." :type '(repeat string) :group 'forj)
(defcustom forj-excluded-patterns '(".git" ".DS_Store" "*.elc" "*.log" "*~") "Patterns for files/directories to exclude from scanning." :type '(repeat string) :group 'forj)

;; Variables
(defvar forj-conversation-history nil "List storing conversation history.")
(defvar forj-current-activity nil "Current activity status for display.")

;; Simple syntax checker using Emacs native functions
(defun forj-check-syntax (&optional buffer)
  "Validate Emacs Lisp syntax using native Emacs functions."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (condition-case err
          (progn (check-parens) (list :status 'valid :message "Syntax is valid"))
        (error (list :status 'invalid :message (format "Syntax error: %s" (error-message-string err))))))))

;; Advanced parentheses checker for AI self-validation
(defun forj-paren-check (code-string)
  "Analyze CODE-STRING for parentheses balance and syntax errors.
Returns structured data with detailed error reporting for AI consumption."
  (let ((line 1)
        (col 1)
        (pos 0)
        (len (length code-string))
        (stack '())
        (in-string nil)
        (in-comment nil)
        (errors '())
        (status 'balanced))
    
    (while (< pos len)
      (let ((char (aref code-string pos)))
        (cond
         ;; Handle newlines (update line/col tracking)
         ((eq char ?\n)
          (setq line (1+ line))
          (setq col 1)
          (setq in-comment nil)) ; Comments end at newline
         
         ;; Handle comments
         ((and (eq char ?\;) (not in-string))
          (setq in-comment t))
         
         ;; Handle strings
         ((and (eq char ?\") (not in-comment))
          (if (and (> pos 0) (eq (aref code-string (1- pos)) ?\\))
              ;; Escaped quote, ignore
              nil
            ;; Toggle string state
            (setq in-string (not in-string))))
         
         ;; Handle opening parens/brackets (only when not in string or comment)
         ((and (not in-string) (not in-comment)
               (memq char '(?\( ?\[ ?\{)))
          (push (list char line col) stack))
         
         ;; Handle closing parens/brackets (only when not in string or comment)
         ((and (not in-string) (not in-comment)
               (memq char '(?\) ?\] ?\})))
          (let ((expected-close (cond
                                 ((eq char ?\)) ?\()
                                 ((eq char ?\]) ?\[)
                                 ((eq char ?\}) ?\{)))
                (opening-info (pop stack)))
            (if opening-info
                (unless (eq (car opening-info) expected-close)
                  ;; Mismatched closing
                  (push (list :line line
                              :col col
                              :type 'mismatched-closing
                              :message (format "Mismatched closing %c, expected %c"
                                               char
                                               (cond ((eq expected-close ?\() ?\))
                                                     ((eq expected-close ?\[) ?\])
                                                     ((eq expected-close ?\{) ?\}))))
                        errors)
                  (setq status 'unbalanced))
              ;; Unmatched closing
              (push (list :line line
                          :col col  
                          :type 'unmatched-closing
                          :message (format "Unmatched closing %c" char))
                    errors)
              (setq status 'unbalanced)))))
        
        (setq col (1+ col))
        (setq pos (1+ pos))))
    
    ;; Check for unclosed opening parens
    (dolist (opening stack)
      (push (list :line (nth 1 opening)
                  :col (nth 2 opening)
                  :type 'unclosed-opening
                  :message (format "Unclosed opening %c" (nth 0 opening)))
            errors)
      (setq status 'unbalanced))
    
    ;; Check for unclosed strings
    (when in-string
      (push (list :line line
                  :col col
                  :type 'unclosed-string
                  :message "Unclosed string literal")
            errors)
      (setq status 'unbalanced))
    
    ;; Return structured result
    (if (eq status 'balanced)
        (list :status status)
      (list :status status
            :error (list :type (plist-get (car errors) :type)
                         :line (plist-get (car errors) :line)
                         :col (plist-get (car errors) :col)
                         :message (plist-get (car errors) :message))))))

;; Activity tracking system
(defun forj-set-activity (status)
  "Set current activity STATUS and update conversation buffer display."
  (setq forj-current-activity status)
  (forj-update-activity-display))

(defun forj-update-activity-display ()
  "Update the activity display in the conversation buffer."
  (let ((buffer (get-buffer forj-conversation-buffer)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (when forj-current-activity
              (insert (format "\n🔄 %s\n" forj-current-activity)))))))))

(defun forj-add-to-history (role content)
  "Add a conversation turn to history with ROLE and CONTENT."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (entry (list :role role :content content :timestamp timestamp)))
    (setq forj-conversation-history (append forj-conversation-history (list entry)))
    (forj-display-conversation-entry entry)))

(defun forj-display-conversation-entry (entry)
  "Display a conversation ENTRY in the conversation buffer."
  (let ((buffer (get-buffer forj-conversation-buffer)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (role (plist-get entry :role))
              (content (plist-get entry :content))
              (timestamp (plist-get entry :timestamp)))
          ;; Use enhanced UI system if available
          (if (and (featurep 'forj-ui-integration) forj-ui-initialized)
              (forj-ui-display-message role content (current-time))
            ;; Fallback to basic display
            (save-excursion
              (goto-char (point-max))
              (insert (format "\n[%s] %s:\n%s\n"
                             timestamp
                             (upcase (symbol-name role))
                             content)))))))))

(defun forj-conversation-history ()
  "Return the current conversation history."
  forj-conversation-history)

;; Conversation mode keymap (must be defined before the mode)
(defvar forj-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'forj-prompt)
    map)
  "Keymap for forj-conversation-mode.")

;; Conversation system
(define-derived-mode forj-conversation-mode special-mode "Forj"
  "Major mode for the Forj conversation buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq word-wrap t)
  
  ;; Enable syntax highlighting and UI components
  (when (featurep 'forj-ui-integration)
    ;; Set up syntax highlighting for code blocks
    (when (featurep 'forj-syntax-highlight)
      (forj-setup-conversation-highlighting))
    
    ;; Set up markdown rendering  
    (when (featurep 'forj-markdown)
      (forj-setup-conversation-markdown))
    
    ;; Apply theme
    (when (featurep 'forj-theme)
      (forj-apply-theme))))

(defun forj-conversation-buffer ()
  "Create or switch to the Forj conversation buffer."
  (interactive)
  (let ((buffer (get-buffer-create forj-conversation-buffer)))
    (with-current-buffer buffer
      (forj-conversation-mode)
      (unless (> (buffer-size) 0)
        (let ((inhibit-read-only t))
          ;; Use enhanced UI system if available
          (if (and (featurep 'forj-ui-integration) forj-ui-initialized)
              (progn
                (forj-insert-buffer-header)
                (forj-setup-buffer-layout))
            ;; Fallback to basic UI
            (insert "Forj AI Assistant\n")
            (insert "================\n\n")
            (insert "Press 'p' to open prompt interface.\n\n")
            (insert "Commands:\n")
            (insert "- p: Open prompt interface\n")
            (insert "- M-x forj-clear-conversation: Clear this buffer\n\n")
            (insert "---\n\n")))))
    (display-buffer buffer)
    buffer))

(defun forj-clear-conversation ()
  "Clear the conversation buffer and history."
  (interactive)
  (setq forj-conversation-history nil)
  (setq forj-current-activity nil)
  (let ((buffer (get-buffer forj-conversation-buffer)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Forj AI Assistant\n")
          (insert "================\n\n")
          (insert "Press 'p' to open prompt interface.\n\n")
          (insert "Commands:\n")
          (insert "- p: Open prompt interface\n")
          (insert "- M-x forj-clear-conversation: Clear this buffer\n\n")
          (insert "---\n\n"))))))

;; File reading functions
(defun forj-read-file (file-path &optional max-size)
  "Read contents of FILE-PATH safely with optional MAX-SIZE limit.
If MAX-SIZE is specified, only read up to that many bytes.
Returns file contents as string, or signals file-error if file cannot be read."
  (let ((size-limit (or max-size forj-max-file-size)))
    (forj-with-error-handling 'file-error
      (with-temp-buffer
        (insert-file-contents file-path nil 0 size-limit)
        (buffer-string)))))

(defun forj-list-files (&optional directory pattern)
  "List files in DIRECTORY matching optional PATTERN.
DIRECTORY defaults to current directory.
PATTERN defaults to match all files (no filtering).
Returns list of file paths."
  (let* ((dir (or directory default-directory))
         (all-files (directory-files dir t)))
    ;; Filter out directories and apply pattern if specified
    (cl-remove-if 
     (lambda (file)
       (or (file-directory-p file)
           ;; Check if file matches excluded patterns
           (cl-some (lambda (excluded-pattern)
                      (string-match-p excluded-pattern (file-name-nondirectory file)))
                    forj-excluded-patterns)
           ;; Apply pattern filter if specified
           (and pattern (not (string-match-p pattern file)))))
     all-files)))

(defun forj-file-metadata (file-path)
  "Get metadata for FILE-PATH including size, modification time, and type.
Returns plist with :path, :size, :modified-time, :type, :readable, :writable."
  (when (file-exists-p file-path)
    (let* ((attributes (file-attributes file-path))
           (size (nth 7 attributes))
           (modified-time (nth 5 attributes))
           (file-type (cond
                       ((string-match-p "\\.el$" file-path) 'elisp)
                       ((string-match-p "\\.md$" file-path) 'markdown)
                       ((string-match-p "\\.txt$" file-path) 'text)
                       ((string-match-p "\\.org$" file-path) 'org)
                       ((string-match-p "\\.py$" file-path) 'python)
                       ((string-match-p "\\.js$" file-path) 'javascript)
                       ((string-match-p "\\.json$" file-path) 'json)
                       ((string-match-p "\\.ya?ml$" file-path) 'yaml)
                       (t 'unknown)))
           (readable (file-readable-p file-path))
           (writable (file-writable-p file-path)))
      (list :path file-path
            :size size
            :modified-time modified-time
            :type file-type
            :readable readable
            :writable writable))))

(defun forj-list-files-with-metadata (&optional directory pattern)
  "List files in DIRECTORY with metadata including size, modification time, and type.
DIRECTORY defaults to current directory.
PATTERN defaults to match all files (no filtering).
Returns list of metadata plists for each file."
  (let ((files (forj-list-files directory pattern)))
    (mapcar 'forj-file-metadata files)))

(defun forj-browse-files (&optional directory)
  "Interactive file browser for selecting project files.
DIRECTORY defaults to current directory.
Returns selected file path or nil if cancelled."
  (interactive)
  (let* ((dir (or directory default-directory))
         (files-with-metadata (forj-list-files-with-metadata dir))
         (file-choices (mapcar (lambda (file-meta)
                                 (let ((path (plist-get file-meta :path))
                                       (size (plist-get file-meta :size))
                                       (type (plist-get file-meta :type))
                                       (modified (plist-get file-meta :modified-time)))
                                   (cons (format "%s [%s, %d bytes, %s]"
                                                (file-name-nondirectory path)
                                                type
                                                size
                                                (format-time-string "%Y-%m-%d %H:%M" modified))
                                         path)))
                               files-with-metadata)))
    (if file-choices
        (let ((selection (completing-read "Select file: " file-choices nil t)))
          (cdr (assoc selection file-choices)))
      (message "No files found in directory: %s" dir)
      nil)))

(defun forj-scan-directory-recursive (&optional directory max-depth max-files)
  "Recursively scan DIRECTORY for project files with depth and file limits.
DIRECTORY defaults to current directory.
MAX-DEPTH defaults to 5 levels deep.
MAX-FILES defaults to 100 files total.
Returns list of file metadata plists."
  (let* ((dir (or directory default-directory))
         (depth-limit (or max-depth 5))
         (file-limit (or max-files 100))
         (result '())
         (file-count 0))
    
    (cl-labels ((scan-dir (current-dir current-depth)
                  (when (and (<= current-depth depth-limit)
                             (< file-count file-limit))
                    ;; Get files in current directory
                    (let ((files (forj-list-files current-dir)))
                      (dolist (file files)
                        (when (< file-count file-limit)
                          (let ((metadata (forj-file-metadata file)))
                            (when metadata
                              (push metadata result)
                              (setq file-count (1+ file-count)))))))
                    
                    ;; Recursively scan subdirectories
                    (when (< current-depth depth-limit)
                      (let ((subdirs (cl-remove-if-not 'file-directory-p
                                                      (directory-files current-dir t))))
                        (dolist (subdir subdirs)
                          (unless (or (string-match-p "/\\.\\.?$" subdir)
                                     (cl-some (lambda (pattern)
                                               (string-match-p pattern (file-name-nondirectory subdir)))
                                             forj-excluded-patterns))
                            (scan-dir subdir (1+ current-depth)))))))))
      
      (scan-dir dir 0)
      (nreverse result))))

(defun forj-browse-and-read-file (&optional directory)
  "Interactive file browser that reads the selected file and logs to conversation.
DIRECTORY defaults to current directory.
Returns file content or nil if cancelled."
  (interactive)
  (forj-set-activity "Browsing files...")
  (let ((selected-file (forj-browse-files directory)))
    (if selected-file
        (progn
          (forj-set-activity (format "Reading file: %s" (file-name-nondirectory selected-file)))
          (let ((content (forj-read-file selected-file)))
            (forj-add-to-history 'system 
                                (format "Read file: %s (%d bytes)" 
                                       (file-relative-name selected-file default-directory)
                                       (length content)))
            (forj-set-activity nil)
            content))
      (forj-set-activity nil)
      (message "File selection cancelled")
      nil)))

(defun forj-scan-and-display-project (&optional directory)
  "Scan project directory and display results in conversation buffer.
DIRECTORY defaults to current directory.
Shows project structure and file summary in conversation."
  (interactive)
  (let* ((dir (or directory default-directory))
         (start-time (current-time)))
    
    (forj-set-activity "Scanning project directory...")
    (forj-add-to-history 'system (format "Starting project scan in: %s" dir))
    
    (let ((files-metadata (forj-scan-directory-recursive dir)))
      (let* ((end-time (current-time))
             (scan-time (float-time (time-subtract end-time start-time)))
             (file-count (length files-metadata))
             (total-size (apply '+ (mapcar (lambda (meta) (or (plist-get meta :size) 0)) 
                                          files-metadata)))
             (file-types (let ((types (make-hash-table)))
                          (dolist (meta files-metadata)
                            (let ((type (plist-get meta :type)))
                              (puthash type (1+ (gethash type types 0)) types)))
                          types)))
        
        ;; Log summary to conversation
        (forj-add-to-history 'system 
                           (format "Project scan complete: %d files, %.2f KB total, %.3f seconds"
                                  file-count
                                  (/ total-size 1024.0)
                                  scan-time))
        
        ;; Log file type breakdown
        (let ((type-summary ""))
          (maphash (lambda (type count)
                    (setq type-summary 
                          (concat type-summary (format "%s: %d, " type count))))
                   file-types)
          (when (> (length type-summary) 0)
            (setq type-summary (substring type-summary 0 -2))) ; Remove trailing ", "
          (forj-add-to-history 'system (format "File types: %s" type-summary)))
        
        (forj-set-activity nil)
        files-metadata))))

(defun forj-read-project-files (&optional file-types max-files)
  "Read multiple project files for AI context.
FILE-TYPES is list of file extensions (default: forj-supported-extensions).
MAX-FILES is maximum number of files to read (default: 10).
Returns alist of (file-path . content) pairs."
  (let* ((extensions (or file-types forj-supported-extensions))
         (file-limit (or max-files 10))
         (pattern (concat "\\.\\(" (mapconcat 'identity extensions "\\|") "\\)$"))
         (files (forj-list-files nil pattern))
         (result '())
         (count 0))
    ;; Read up to max-files files
    (dolist (file files)
      (when (< count file-limit)
        (condition-case err
            (let ((content (forj-read-file file)))
              (push (cons (file-relative-name file default-directory) content) result)
              (setq count (1+ count)))
          (file-error
           ;; Skip files that cannot be read
           nil))))
    (nreverse result)))

;; File writing functions
(defun forj-write-file (file-path content &optional no-backup)
  "Write CONTENT to FILE-PATH with backup unless NO-BACKUP.
Returns plist with :success, :backup-path, :error-message, and :modified-time.
Creates timestamped backup for existing files.
Validates Emacs Lisp syntax for .el files before writing.
Uses atomic operations to prevent file corruption."
  (let* ((absolute-path (expand-file-name file-path))
         (is-elisp (string-match-p "\\.el$" absolute-path))
         (file-exists (file-exists-p absolute-path))
         (backup-path nil)
         (temp-file nil)
         (result (list :success nil :backup-path nil :error-message nil :modified-time nil)))
    
    (condition-case err
        (progn
          ;; Security check: Validate file path and permissions
          (unless (file-name-absolute-p absolute-path)
            (signal 'file-error (list "Path must be absolute" absolute-path)))
          
          ;; Check directory permissions
          (let ((dir (file-name-directory absolute-path)))
            (unless (file-writable-p dir)
              (signal 'file-error (list "Directory not writable" dir))))
          
          ;; Validate Emacs Lisp syntax before writing
          (when is-elisp
            (let ((validation-result (forj-paren-check content)))
              (unless (eq (plist-get validation-result :status) 'balanced)
                (let ((error-info (plist-get validation-result :error)))
                  (forj-validation-error "Invalid Emacs Lisp syntax before file write"
                                       :context "Pre-write syntax validation"
                                       :file absolute-path
                                       :details error-info
                                       :recovery '("Fix syntax errors in content"
                                                  "Check parentheses balance"
                                                  "Verify code structure"))))))
          
          ;; Create backup if file exists and backup is not disabled
          (when (and file-exists (not no-backup))
            (setq backup-path (concat absolute-path ".bak." 
                                    (format-time-string "%Y%m%d-%H%M%S")))
            (copy-file absolute-path backup-path)
            (setf (plist-get result :backup-path) backup-path))
          
          ;; Atomic write: Write to temporary file first, then rename
          (setq temp-file (concat absolute-path ".tmp." (format "%d" (emacs-pid))))
          (with-temp-file temp-file
            (insert content))
          
          ;; Atomic rename
          (rename-file temp-file absolute-path t)
          
          ;; Success
          (setf (plist-get result :success) t)
          (setf (plist-get result :modified-time) (current-time))
          result)
      
      ;; Error handling
      (file-error
       (setf (plist-get result :error-message) 
             (format "File operation error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))
      
    ;; Cleanup temporary file if it still exists
    (when (and temp-file (file-exists-p temp-file))
      (ignore-errors (delete-file temp-file)))
    
    result))

;; Advanced file operations
(defun forj-edit-file-region (file-path start-line end-line new-content &optional no-backup)
  "Edit region in FILE-PATH from START-LINE to END-LINE with NEW-CONTENT.
Creates backup and validates syntax for .el files unless NO-BACKUP.
Returns plist with :success, :backup-path, :error-message, :lines-changed."
  (let* ((absolute-path (expand-file-name file-path))
         (is-elisp (string-match-p "\\.el$" absolute-path))
         (backup-path nil)
         (result (list :success nil :backup-path nil :error-message nil :lines-changed 0)))
    
    (condition-case err
        (progn
          ;; Security check: Validate file path and permissions
          (unless (file-exists-p absolute-path)
            (signal 'file-error (list "File does not exist" absolute-path)))
          (unless (file-readable-p absolute-path)
            (signal 'file-error (list "File not readable" absolute-path)))
          (unless (file-writable-p absolute-path)
            (signal 'file-error (list "File not writable" absolute-path)))
          
          ;; Read current file content
          (let ((original-content (forj-read-file absolute-path))
                (lines-changed 0))
            
            ;; Create backup if not disabled
            (unless no-backup
              (setq backup-path (concat absolute-path ".bak." 
                                      (format-time-string "%Y%m%d-%H%M%S")))
              (copy-file absolute-path backup-path)
              (setf (plist-get result :backup-path) backup-path))
            
            ;; Split content into lines
            (let* ((lines (split-string original-content "\n"))
                   (total-lines (length lines))
                   (start-idx (max 0 (1- start-line))) ; Convert to 0-based index
                   (end-idx (min (1- total-lines) (1- end-line))) ; Convert to 0-based index
                   (new-lines (split-string new-content "\n")))
              
              ;; Validate line numbers
              (when (> start-line total-lines)
                (signal 'file-error (list "Start line beyond file end" start-line total-lines)))
              (when (> end-line total-lines)
                (signal 'file-error (list "End line beyond file end" end-line total-lines)))
              (when (> start-line end-line)
                (signal 'file-error (list "Start line after end line" start-line end-line)))
              
              ;; Replace the region
              (let ((before-lines (cl-subseq lines 0 start-idx))
                    (after-lines (cl-subseq lines (1+ end-idx))))
                (setq lines-changed (+ (- end-idx start-idx) 1 (- (length new-lines))))
                (setf (plist-get result :lines-changed) lines-changed)
                
                ;; Construct new file content
                (let ((new-file-content (mapconcat 'identity
                                                 (append before-lines new-lines after-lines)
                                                 "\n")))
                  
                  ;; Validate Emacs Lisp syntax if needed
                  (when is-elisp
                    (let ((validation-result (forj-paren-check new-file-content)))
                      (unless (eq (plist-get validation-result :status) 'balanced)
                        (let ((error-info (plist-get validation-result :error)))
                          (signal 'file-error 
                                 (list "Invalid Emacs Lisp syntax after edit"
                                       (plist-get error-info :message)
                                       (format "Line %d, Column %d"
                                              (plist-get error-info :line)
                                              (plist-get error-info :col))))))))
                  
                  ;; Write the modified content using atomic operations
                  (let ((temp-file (concat absolute-path ".tmp." (format "%d" (emacs-pid)))))
                    (unwind-protect
                        (progn
                          (with-temp-file temp-file
                            (insert new-file-content))
                          (rename-file temp-file absolute-path t))
                      ;; Cleanup temp file if it still exists
                      (when (file-exists-p temp-file)
                        (ignore-errors (delete-file temp-file)))))))))
          
          ;; Success
          (setf (plist-get result :success) t)
          result)
      
      ;; Error handling
      (file-error
       (setf (plist-get result :error-message) 
             (format "File operation error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))))

;; File modification tracking
(defvar forj-file-modifications (make-hash-table :test 'equal)
  "Hash table tracking file modification timestamps and changes.")

(defun forj-track-file-modification (file-path operation-type)
  "Track FILE-PATH modification with OPERATION-TYPE and timestamp.
OPERATION-TYPE can be 'write, 'edit-region, 'backup, 'restore."
  (let* ((absolute-path (expand-file-name file-path))
         (timestamp (current-time))
         (modification-info (list :timestamp timestamp
                                 :operation operation-type
                                 :size (when (file-exists-p absolute-path)
                                        (nth 7 (file-attributes absolute-path))))))
    (puthash absolute-path 
             (cons modification-info 
                   (gethash absolute-path forj-file-modifications '()))
             forj-file-modifications)
    modification-info))

(defun forj-get-file-modification-history (file-path)
  "Get modification history for FILE-PATH.
Returns list of modification info plists in chronological order (newest first)."
  (let ((absolute-path (expand-file-name file-path)))
    (gethash absolute-path forj-file-modifications '())))

;; Backup and restore utilities
(defun forj-backup-file (file-path &optional backup-name)
  "Create backup of FILE-PATH with optional custom BACKUP-NAME.
Returns plist with :success, :backup-path, :error-message."
  (let* ((absolute-path (expand-file-name file-path))
         (backup-path (or backup-name
                         (concat absolute-path ".bak." 
                                (format-time-string "%Y%m%d-%H%M%S"))))
         (result (list :success nil :backup-path nil :error-message nil)))
    
    (condition-case err
        (progn
          ;; Security checks
          (unless (file-exists-p absolute-path)
            (signal 'file-error (list "File does not exist" absolute-path)))
          (unless (file-readable-p absolute-path)
            (signal 'file-error (list "File not readable" absolute-path)))
          
          ;; Create backup
          (copy-file absolute-path backup-path)
          (forj-track-file-modification backup-path 'backup)
          
          ;; Success
          (setf (plist-get result :success) t)
          (setf (plist-get result :backup-path) backup-path)
          result)
      
      (file-error
       (setf (plist-get result :error-message) 
             (format "Backup error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))))

(defun forj-restore-backup (backup-path original-path)
  "Restore BACKUP-PATH to ORIGINAL-PATH.
Returns plist with :success, :error-message, :restored-size."
  (let* ((absolute-backup (expand-file-name backup-path))
         (absolute-original (expand-file-name original-path))
         (result (list :success nil :error-message nil :restored-size nil)))
    
    (condition-case err
        (progn
          ;; Security checks
          (unless (file-exists-p absolute-backup)
            (signal 'file-error (list "Backup file does not exist" absolute-backup)))
          (unless (file-readable-p absolute-backup)
            (signal 'file-error (list "Backup file not readable" absolute-backup)))
          
          ;; Check if we can write to original location
          (let ((original-dir (file-name-directory absolute-original)))
            (unless (file-writable-p original-dir)
              (signal 'file-error (list "Cannot write to directory" original-dir))))
          
          ;; Restore backup
          (copy-file absolute-backup absolute-original t)
          (let ((restored-size (nth 7 (file-attributes absolute-original))))
            (forj-track-file-modification absolute-original 'restore)
            (setf (plist-get result :restored-size) restored-size))
          
          ;; Success
          (setf (plist-get result :success) t)
          result)
      
      (file-error
       (setf (plist-get result :error-message) 
             (format "Restore error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))))

;; File locking for concurrent access protection
(defvar forj-locked-files (make-hash-table :test 'equal)
  "Hash table tracking currently locked files.")

(defun forj-lock-file (file-path &optional timeout)
  "Lock FILE-PATH for exclusive access with optional TIMEOUT in seconds.
Returns plist with :success, :lock-id, :error-message."
  (let* ((absolute-path (expand-file-name file-path))
         (lock-file (concat absolute-path ".forj-lock"))
         (lock-id (format "%d-%d" (emacs-pid) (floor (float-time))))
         (timeout-seconds (or timeout 30))
         (start-time (current-time))
         (result (list :success nil :lock-id nil :error-message nil)))
    
    (condition-case err
        (progn
          ;; Wait for any existing locks to be released
          (while (and (file-exists-p lock-file)
                     (< (float-time (time-subtract (current-time) start-time)) timeout-seconds))
            (sleep-for 0.1))
          
          ;; Check if we timed out waiting
          (when (file-exists-p lock-file)
            (signal 'file-error (list "File is locked by another process" absolute-path lock-file)))
          
          ;; Create lock file
          (with-temp-file lock-file
            (insert (format "forj-lock:%s:%s:%s\n" 
                           lock-id 
                           (emacs-pid)
                           (format-time-string "%Y-%m-%d %H:%M:%S"))))
          
          ;; Store in memory
          (puthash absolute-path lock-id forj-locked-files)
          
          ;; Success
          (setf (plist-get result :success) t)
          (setf (plist-get result :lock-id) lock-id)
          result)
      
      (file-error
       (setf (plist-get result :error-message) 
             (format "Lock error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))))

(defun forj-unlock-file (file-path lock-id)
  "Unlock FILE-PATH using LOCK-ID.
Returns plist with :success, :error-message."
  (let* ((absolute-path (expand-file-name file-path))
         (lock-file (concat absolute-path ".forj-lock"))
         (result (list :success nil :error-message nil)))
    
    (condition-case err
        (progn
          ;; Verify we own the lock
          (let ((stored-lock-id (gethash absolute-path forj-locked-files)))
            (unless (string= lock-id stored-lock-id)
              (signal 'file-error (list "Lock ID mismatch" lock-id stored-lock-id))))
          
          ;; Remove lock file
          (when (file-exists-p lock-file)
            (delete-file lock-file))
          
          ;; Remove from memory
          (remhash absolute-path forj-locked-files)
          
          ;; Success
          (setf (plist-get result :success) t)
          result)
      
      (file-error
       (setf (plist-get result :error-message) 
             (format "Unlock error: %s" (error-message-string err)))
       result)
      
      (error
       (setf (plist-get result :error-message) 
             (format "Unexpected error: %s" (error-message-string err)))
       result))))

(defmacro forj-with-file-lock (file-path &rest body)
  "Execute BODY with FILE-PATH locked, automatically unlocking when done."
  (declare (indent 1))
  `(let* ((lock-result (forj-lock-file ,file-path))
          (lock-id (plist-get lock-result :lock-id)))
     (if (plist-get lock-result :success)
         (unwind-protect
             (progn ,@body)
           (forj-unlock-file ,file-path lock-id))
       (error "Failed to lock file: %s" (plist-get lock-result :error-message)))))

;; Confirmation prompts for destructive operations
(defcustom forj-confirm-destructive-operations t
  "Whether to prompt for confirmation before destructive operations."
  :type 'boolean
  :group 'forj)

(defun forj-confirm-operation (operation-description file-path &optional details)
  "Prompt user to confirm OPERATION-DESCRIPTION on FILE-PATH with optional DETAILS.
Returns t if user confirms, nil otherwise."
  (if forj-confirm-destructive-operations
      (let ((prompt (format "%s: %s%s\nContinue? (y/n): " 
                           operation-description 
                           (file-name-nondirectory file-path)
                           (if details (format " (%s)" details) ""))))
        (y-or-n-p prompt))
    t))

(defun forj-write-file-with-confirmation (file-path content &optional no-backup)
  "Write CONTENT to FILE-PATH with user confirmation for existing files.
Shows diff preview and asks for confirmation if file exists."
  (let* ((absolute-path (expand-file-name file-path))
         (file-exists (file-exists-p absolute-path)))
    
    (when (and file-exists forj-confirm-destructive-operations)
      ;; Show diff preview
      (let ((original-content (forj-read-file absolute-path)))
        (with-temp-buffer
          (insert "=== DIFF PREVIEW ===\n")
          (insert (format "File: %s\n" (file-name-nondirectory absolute-path)))
          (insert (format "Original size: %d bytes\n" (length original-content)))
          (insert (format "New size: %d bytes\n" (length content)))
          (insert "\n--- Original\n+++ New\n")
          ;; Simple line-by-line diff
          (let ((orig-lines (split-string original-content "\n"))
                (new-lines (split-string content "\n")))
            (dotimes (i (max (length orig-lines) (length new-lines)))
              (let ((orig-line (when (< i (length orig-lines)) (nth i orig-lines)))
                    (new-line (when (< i (length new-lines)) (nth i new-lines))))
                (cond
                 ((and orig-line new-line (string= orig-line new-line))
                  (insert (format "  %s\n" orig-line)))
                 ((and orig-line (not new-line))
                  (insert (format "- %s\n" orig-line)))
                 ((and (not orig-line) new-line)
                  (insert (format "+ %s\n" new-line)))
                 ((and orig-line new-line)
                  (insert (format "- %s\n" orig-line))
                  (insert (format "+ %s\n" new-line)))))))
          (display-buffer (current-buffer)))
        
        ;; Ask for confirmation
        (unless (forj-confirm-operation "Overwrite file" absolute-path 
                                       (format "%d -> %d bytes" 
                                              (length original-content) 
                                              (length content)))
          (signal 'quit "Operation cancelled by user"))))
    
    ;; Proceed with write
    (forj-write-file absolute-path content no-backup)))

;; Git repository integration
(defun forj-in-git-repo-p (&optional directory)
  "Check if DIRECTORY (or current directory) is in a Git repository.
Returns git root directory if in repo, nil otherwise."
  (let ((dir (or directory default-directory)))
    (when (file-directory-p dir)
      (let ((git-dir (locate-dominating-file dir ".git")))
        (when git-dir
          (expand-file-name git-dir))))))

(defcustom forj-enable-git-integration t
  "Whether to enable Git repository integration features."
  :type 'boolean
  :group 'forj)

(defun forj-git-file-status (file-path)
  "Get Git status for FILE-PATH.
Returns plist with :status, :staged, :modified, :untracked."
  (if (and forj-enable-git-integration (forj-in-git-repo-p))
      (let* ((absolute-path (expand-file-name file-path))
             (relative-path (file-relative-name absolute-path (forj-in-git-repo-p)))
             (git-status-output "")
             (result (list :status 'unknown :staged nil :modified nil :untracked nil)))
        
        (condition-case err
            (progn
              ;; Run git status --porcelain on the file
              (with-temp-buffer
                (when (zerop (call-process "git" nil t nil "status" "--porcelain" "--" relative-path))
                  (setq git-status-output (buffer-string))))
              
              ;; Parse git status output
              (when (> (length git-status-output) 0)
                (let ((status-line (string-trim git-status-output)))
                  (when (>= (length status-line) 2)
                    (let ((staged-status (aref status-line 0))
                          (modified-status (aref status-line 1)))
                      ;; Parse staged status
                      (setf (plist-get result :staged)
                            (memq staged-status '(?M ?A ?D ?R ?C)))
                      
                      ;; Parse modified status
                      (setf (plist-get result :modified)
                            (eq modified-status ?M))
                      
                      ;; Parse untracked status
                      (setf (plist-get result :untracked)
                            (and (eq staged-status ??) (eq modified-status ??)))
                      
                      ;; Set overall status
                      (setf (plist-get result :status)
                            (cond
                             ((plist-get result :untracked) 'untracked)
                             ((and (plist-get result :staged) (plist-get result :modified)) 'staged-and-modified)
                             ((plist-get result :staged) 'staged)
                             ((plist-get result :modified) 'modified)
                             (t 'clean)))))))
              
              ;; If no output, file is clean or not in repo
              (when (= (length git-status-output) 0)
                (setf (plist-get result :status) 'clean))
              
              result)
          
          (error
           (setf (plist-get result :status) 'error)
           result)))
    ;; Git integration disabled or not in repo
    (list :status 'no-git :staged nil :modified nil :untracked nil)))

(defun forj-git-has-uncommitted-changes-p (file-path)
  "Check if FILE-PATH has uncommitted changes in Git.
Returns t if file has unstaged or staged changes, nil otherwise."
  (let ((git-status (forj-git-file-status file-path)))
    (or (plist-get git-status :modified)
        (plist-get git-status :staged))))

(defcustom forj-warn-uncommitted-changes t
  "Whether to warn before modifying files with uncommitted Git changes."
  :type 'boolean
  :group 'forj)

(defun forj-check-git-status-before-edit (file-path)
  "Check Git status of FILE-PATH and warn if there are uncommitted changes.
Returns t if it's safe to proceed, signals error if user cancels."
  (when (and forj-warn-uncommitted-changes 
             forj-enable-git-integration
             (forj-in-git-repo-p))
    (let ((git-status (forj-git-file-status file-path)))
      (when (memq (plist-get git-status :status) '(modified staged staged-and-modified))
        (let ((status-desc (pcase (plist-get git-status :status)
                            ('modified "has unstaged changes")
                            ('staged "has staged changes")
                            ('staged-and-modified "has both staged and unstaged changes"))))
          (unless (forj-confirm-operation 
                   (format "File %s" status-desc) 
                   file-path
                   "Modifying will create additional changes")
            (signal 'quit "Operation cancelled due to Git status"))))))
  t)

(defcustom forj-auto-stage-changes nil
  "Whether to automatically stage changes after successful edits in Git repo."
  :type 'boolean
  :group 'forj)

(defun forj-auto-stage-file-if-enabled (file-path)
  "Automatically stage FILE-PATH if auto-staging is enabled and we're in a Git repo."
  (when (and forj-auto-stage-changes 
             forj-enable-git-integration
             (forj-in-git-repo-p))
    (condition-case err
        (let ((relative-path (file-relative-name (expand-file-name file-path) 
                                               (forj-in-git-repo-p))))
          (when (zerop (call-process "git" nil nil nil "add" relative-path))
            (forj-add-to-history 'system 
                               (format "Auto-staged file: %s" 
                                      (file-name-nondirectory file-path)))))
      (error
       (message "Warning: Failed to auto-stage file: %s" (error-message-string err))))))

;; Enhanced file operations with Git integration
(defun forj-write-file-with-git-awareness (file-path content &optional no-backup)
  "Write CONTENT to FILE-PATH with Git status awareness and logging.
Checks Git status, warns about uncommitted changes, and optionally auto-stages."
  (let* ((absolute-path (expand-file-name file-path))
         (operation-start (current-time)))
    
    ;; Check Git status before proceeding
    (forj-check-git-status-before-edit absolute-path)
    
    ;; Perform the write operation
    (let ((result (forj-write-file absolute-path content no-backup)))
      (when (plist-get result :success)
        ;; Track the modification
        (forj-track-file-modification absolute-path 'write)
        
        ;; Auto-stage if enabled
        (forj-auto-stage-file-if-enabled absolute-path)
        
        ;; Log to conversation
        (let ((operation-time (float-time (time-subtract (current-time) operation-start))))
          (forj-add-to-history 'system 
                             (format "Wrote file: %s (%d bytes, %.3fs)%s%s"
                                    (file-name-nondirectory absolute-path)
                                    (length content)
                                    operation-time
                                    (if (plist-get result :backup-path) ", backup created" "")
                                    (if forj-auto-stage-changes ", auto-staged" "")))))
      result)))

(defun forj-edit-file-region-with-git-awareness (file-path start-line end-line new-content &optional no-backup)
  "Edit region with Git awareness and conversation logging.
Enhanced version of forj-edit-file-region with Git integration."
  (let* ((absolute-path (expand-file-name file-path))
         (operation-start (current-time)))
    
    ;; Check Git status before proceeding
    (forj-check-git-status-before-edit absolute-path)
    
    ;; Perform the edit operation
    (let ((result (forj-edit-file-region absolute-path start-line end-line new-content no-backup)))
      (when (plist-get result :success)
        ;; Track the modification
        (forj-track-file-modification absolute-path 'edit-region)
        
        ;; Auto-stage if enabled
        (forj-auto-stage-file-if-enabled absolute-path)
        
        ;; Log to conversation
        (let ((operation-time (float-time (time-subtract (current-time) operation-start)))
              (lines-changed (plist-get result :lines-changed)))
          (forj-add-to-history 'system 
                             (format "Edited region: %s (lines %d-%d, %+d lines, %.3fs)%s%s"
                                    (file-name-nondirectory absolute-path)
                                    start-line end-line
                                    lines-changed
                                    operation-time
                                    (if (plist-get result :backup-path) ", backup created" "")
                                    (if forj-auto-stage-changes ", auto-staged" "")))))
      result)))

;; Directory Operations (1.4.3)
(defun forj-detect-project-type (&optional directory)
  "Detect the type of project in DIRECTORY.
Returns 'elisp-package, 'general, or 'unknown."
  (let* ((dir (or directory default-directory))
         (files (directory-files dir nil nil t)))
    (cond
     ;; Check for elisp package indicators
     ((or (cl-some (lambda (f) (and (string-match-p "\\.el$" f)
                                   (not (string-match-p "^\\." f))))
                  files)
          (member "package.el" files)
          (cl-some (lambda (f) (string-match-p "^.*-pkg\\.el$" f)) files))
      'elisp-package)
     ;; Check for any recognizable project files
     ((or (member "README.md" files)
          (member "README.txt" files)
          (member "README" files)
          (cl-some (lambda (f) (string-match-p "\\.(py|js|ts|java|cpp|c|rs)$" f)) files))
      'general)
     ;; Unknown project type
     (t 'unknown))))

(defun forj-directory-operations (&optional directory)
  "Analyze current directory context for AI coding operations.
DIRECTORY defaults to current working directory.
Returns project context as a plist with:
  :directory - absolute path of analyzed directory
  :files - list of project-relevant file paths  
  :project-type - detected project type (elisp-package, general, unknown)
  :file-count - total number of relevant files
  :total-size - combined size of all files in bytes
  :file-types - hash table of extension -> count
  :excluded-count - number of excluded files/directories"
  (let* ((dir (or directory default-directory))
         (absolute-dir (expand-file-name dir))
         (scan-result (forj-scan-directory-recursive dir))
         (project-type (forj-detect-project-type dir))
         (file-paths (mapcar (lambda (meta) (plist-get meta :path)) scan-result))
         (file-count (length scan-result))
         (total-size (apply '+ (mapcar (lambda (meta) (or (plist-get meta :size) 0)) scan-result)))
         (file-types (make-hash-table :test 'equal))
         (all-files (ignore-errors (directory-files-and-attributes dir t nil t)))
         (all-count (length (cl-remove-if (lambda (entry) 
                                           (member (file-name-nondirectory (car entry)) '("." "..")))
                                         all-files)))
         (excluded-count (max 0 (- all-count file-count))))
    
    ;; Build file type statistics
    (dolist (meta scan-result)
      (let ((ext (plist-get meta :extension)))
        (when ext
          (puthash ext (1+ (gethash ext file-types 0)) file-types))))
    
    ;; Return comprehensive directory context
    (list :directory absolute-dir
          :files file-paths
          :project-type project-type  
          :file-count file-count
          :total-size total-size
          :file-types file-types
          :excluded-count excluded-count
          :scan-timestamp (current-time))))

(provide 'forj)
;;; forj.el ends here
