;;; forj-context.el --- Context Management System for Forj.el -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive context management system that leverages native Emacs concepts
;; (buffers, regions, marks, compilation results, Dired) to provide intelligent,
;; contextual AI assistance within the Forj.el framework.
;;
;; This implements the Context Management System from Specification 002.

;;; Code:

(require 'cl-lib)

;; NOTE: Dependencies are managed by the central forj.el loader.
;; Do not use require/load-file directly in individual modules.

;;; Configuration Group

(defgroup forj-context nil
  "Context management system for Forj.el"
  :group 'forj
  :prefix "forj-context-")

(defcustom forj-context-max-size 50000
  "Maximum size of context in characters."
  :type 'integer
  :group 'forj-context)

(defcustom forj-context-auto-suggestions t
  "Enable automatic context suggestions based on prompt analysis."
  :type 'boolean
  :group 'forj-context)

(defcustom forj-context-cache-enabled t
  "Enable caching of context content to avoid repeated reads."
  :type 'boolean
  :group 'forj-context)

(defcustom forj-context-performance-monitoring t
  "Enable performance monitoring for context operations."
  :type 'boolean
  :group 'forj-context)

;;; Context State Variables

(defvar forj-context-selected '()
  "Currently selected context items.")

(defvar forj-context-content-cache (make-hash-table :test 'equal)
  "Cache for context content to avoid repeated reads.")

(defvar forj-context-performance-metrics (make-hash-table :test 'equal)
  "Performance metrics for context operations.")

;;; Core Context Collection Functions

(defun forj-collect-buffer-context (buffer-or-name &optional region)
  "Collect context from buffer, optionally limited to region.
BUFFER-OR-NAME can be a buffer object or buffer name string.
REGION is an optional cons cell (START . END) to limit context to that region."
  (let ((start-time (when forj-context-performance-monitoring (current-time)))
        (buffer (get-buffer buffer-or-name))
        result)
    
    (unless buffer
      (if (featurep 'forj-error-system)
          (forj-context-error "Buffer not found: %s" buffer-or-name)
        (error "Buffer not found: %s" buffer-or-name)))
    
    (with-current-buffer buffer
      (let* ((content (if region
                          (buffer-substring-no-properties
                           (car region) (cdr region))
                        (buffer-string)))
             (buffer-info (list :buffer (buffer-name)
                                :mode major-mode
                                :file (buffer-file-name)
                                :size (buffer-size)
                                :modified (buffer-modified-p)
                                :read-only buffer-read-only))
             (relevance (forj-calculate-buffer-relevance buffer)))
        
        (setq result
              `(:type buffer
                :content ,content
                :metadata ,buffer-info
                :relevance ,relevance
                :collected-at ,(current-time)))))
    
    ;; Record performance metrics
    (when forj-context-performance-monitoring
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (puthash 'buffer-collection elapsed forj-context-performance-metrics)))
    
    result))

(defun forj-collect-file-context (file-path)
  "Collect context from a specific file.
FILE-PATH should be the absolute or relative path to the file."
  (let ((start-time (when forj-context-performance-monitoring (current-time)))
        (absolute-path (expand-file-name file-path))
        result)
    
    (unless (file-exists-p absolute-path)
      (if (featurep 'forj-error-system)
          (forj-context-error "File not found: %s" absolute-path)
        (error "File not found: %s" absolute-path)))
    
    (unless (file-readable-p absolute-path)
      (if (featurep 'forj-error-system)
          (forj-context-error "File not readable: %s" absolute-path)
        (error "File not readable: %s" absolute-path)))
    
    ;; Check cache first
    (let ((cache-key (format "file:%s:%s" absolute-path 
                            (format-time-string "%Y%m%d%H%M%S" 
                                              (nth 5 (file-attributes absolute-path))))))
      (if (and forj-context-cache-enabled
               (gethash cache-key forj-context-content-cache))
          (gethash cache-key forj-context-content-cache)
        
        (let* ((content (forj-read-file-safely absolute-path forj-context-max-size))
               (file-info (list :path absolute-path
                               :size (nth 7 (file-attributes absolute-path))
                               :modified (nth 5 (file-attributes absolute-path))
                               :type (forj-detect-file-type absolute-path)
                               :readable (file-readable-p absolute-path)
                               :writable (file-writable-p absolute-path)))
               (relevance (forj-calculate-file-relevance absolute-path)))
          
          (setq result
                `(:type file
                  :path ,absolute-path
                  :content ,content
                  :metadata ,file-info
                  :relevance ,relevance
                  :collected-at ,(current-time)))
          
          ;; Cache the result
          (when forj-context-cache-enabled
            (puthash cache-key result forj-context-content-cache))
          
          ;; Record performance metrics
          (when forj-context-performance-monitoring
            (let ((elapsed (float-time (time-subtract (current-time) start-time))))
              (puthash 'file-collection elapsed forj-context-performance-metrics)))
          
          result)))))

(defun forj-collect-compilation-context (&optional include-warnings)
  "Collect context from compilation buffer and error locations.
If INCLUDE-WARNINGS is non-nil, include warning messages as well as errors."
  (let ((start-time (when forj-context-performance-monitoring (current-time)))
        (compilation-buffer (get-buffer "*compilation*"))
        result)
    
    (if (and compilation-buffer
             (with-current-buffer compilation-buffer
               (> (buffer-size) 0)))
        (with-current-buffer compilation-buffer
          (let* ((content (buffer-string))
                 (errors (forj-extract-compilation-errors content include-warnings))
                 (compilation-info (list :buffer-name (buffer-name)
                                        :mode major-mode
                                        :error-count (length (cl-remove-if-not 
                                                            (lambda (err) 
                                                              (eq (plist-get err :severity) 'error))
                                                            errors))
                                        :warning-count (if include-warnings
                                                         (length (cl-remove-if-not 
                                                                (lambda (err) 
                                                                  (eq (plist-get err :severity) 'warning))
                                                                errors))
                                                       0)))
                 (relevance (forj-calculate-compilation-relevance errors)))
            
            (setq result
                  `(:type compilation
                    :content ,content
                    :errors ,errors
                    :metadata ,compilation-info
                    :relevance ,relevance
                    :collected-at ,(current-time)))))
      
      ;; No compilation buffer or empty
      (setq result
            `(:type compilation
              :content ""
              :errors nil
              :metadata (:buffer-name "none" :error-count 0 :warning-count 0)
              :relevance 0.0
              :collected-at ,(current-time))))
    
    ;; Record performance metrics
    (when forj-context-performance-monitoring
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (puthash 'compilation-collection elapsed forj-context-performance-metrics)))
    
    result))

(defun forj-collect-context-from-buffers (buffer-list)
  "Collect context from multiple buffers with relevance scoring.
BUFFER-LIST is a list of buffer names or buffer objects."
  (let ((start-time (when forj-context-performance-monitoring (current-time)))
        (contexts '())
        (total-size 0))
    
    (dolist (buffer-spec buffer-list)
      (when (< total-size forj-context-max-size)
        (let ((context (forj-collect-buffer-context buffer-spec)))
          (when context
            (let ((content-size (length (plist-get context :content))))
              (when (< (+ total-size content-size) forj-context-max-size)
                (push context contexts)
                (setq total-size (+ total-size content-size))))))))
    
    ;; Sort by relevance (highest first)
    (setq contexts (sort contexts
                        (lambda (a b)
                          (> (plist-get a :relevance)
                             (plist-get b :relevance)))))
    
    ;; Record performance metrics
    (when forj-context-performance-monitoring
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (puthash 'multi-buffer-collection elapsed forj-context-performance-metrics)))
    
    contexts))

(defun forj-collect-project-context (project-root &optional file-patterns)
  "Collect context from entire project with filtering.
PROJECT-ROOT is the root directory of the project.
FILE-PATTERNS is an optional list of patterns to match files."
  (let ((start-time (when forj-context-performance-monitoring (current-time)))
        (absolute-root (expand-file-name project-root))
        (contexts '())
        (total-size 0))
    
    (unless (file-directory-p absolute-root)
      (if (featurep 'forj-error-system)
          (forj-context-error "Project directory not found: %s" absolute-root)
        (error "Project directory not found: %s" absolute-root)))
    
    ;; Get project files using existing forj functions
    (let ((files (if (fboundp 'forj-scan-directory-recursive)
                     (forj-scan-directory-recursive absolute-root 5 100)
                   (forj-simple-file-scan absolute-root))))
      
      ;; Filter by patterns if specified
      (when file-patterns
        (setq files (cl-remove-if-not 
                    (lambda (file-meta)
                      (let ((path (plist-get file-meta :path)))
                        (cl-some (lambda (pattern)
                                  (string-match-p pattern path))
                                file-patterns)))
                    files)))
      
      ;; Collect context from files
      (dolist (file-meta files)
        (when (< total-size forj-context-max-size)
          (let* ((file-path (plist-get file-meta :path))
                 (context (condition-case err
                             (forj-collect-file-context file-path)
                           (error 
                            (message "Warning: Failed to collect context from %s: %s" 
                                   file-path (error-message-string err))
                            nil))))
            (when context
              (let ((content-size (length (plist-get context :content))))
                (when (< (+ total-size content-size) forj-context-max-size)
                  (push context contexts)
                  (setq total-size (+ total-size content-size)))))))))
    
    ;; Sort by relevance
    (setq contexts (sort contexts
                        (lambda (a b)
                          (> (plist-get a :relevance)
                             (plist-get b :relevance)))))
    
    ;; Record performance metrics
    (when forj-context-performance-monitoring
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (puthash 'project-collection elapsed forj-context-performance-metrics)))
    
    contexts))

;;; Utility Functions

(defun forj-read-file-safely (file-path max-size)
  "Read file content safely with size limit.
FILE-PATH is the path to read. MAX-SIZE is the maximum number of characters."
  (with-temp-buffer
    (insert-file-contents file-path nil 0 max-size)
    (buffer-string)))

(defun forj-detect-file-type (file-path)
  "Detect the type of file at FILE-PATH."
  (let ((ext (file-name-extension file-path)))
    (cond
     ((string= ext "el") 'elisp)
     ((member ext '("md" "markdown")) 'markdown)
     ((string= ext "org") 'org-mode)
     ((member ext '("txt" "text")) 'text)
     ((member ext '("py" "python")) 'python)
     ((member ext '("js" "javascript")) 'javascript)
     ((member ext '("ts" "typescript")) 'typescript)
     ((member ext '("html" "htm")) 'html)
     ((member ext '("css" "scss" "sass")) 'css)
     ((member ext '("json")) 'json)
     ((member ext '("yaml" "yml")) 'yaml)
     ((member ext '("xml")) 'xml)
     (t 'unknown))))

(defun forj-calculate-buffer-relevance (buffer)
  "Calculate relevance score for BUFFER (0.0 to 1.0)."
  (with-current-buffer buffer
    (let ((base-score 0.5)
          (size-factor 0.0)
          (mode-factor 0.0)
          (activity-factor 0.0))
      
      ;; Size factor (larger buffers slightly more relevant, up to a point)
      (let ((size (buffer-size)))
        (setq size-factor (min 0.2 (/ size 10000.0))))
      
      ;; Mode factor (code modes more relevant)
      (setq mode-factor
            (cond
             ((derived-mode-p 'prog-mode) 0.3)
             ((derived-mode-p 'text-mode) 0.2)
             ((derived-mode-p 'dired-mode) 0.1)
             (t 0.0)))
      
      ;; Activity factor (current buffer, modified buffers more relevant)
      (setq activity-factor
            (+ (if (eq (current-buffer) buffer) 0.2 0.0)
               (if (buffer-modified-p) 0.1 0.0)))
      
      (min 1.0 (+ base-score size-factor mode-factor activity-factor)))))

(defun forj-calculate-file-relevance (file-path)
  "Calculate relevance score for FILE-PATH (0.0 to 1.0)."
  (let ((base-score 0.4)
        (type-factor 0.0)
        (size-factor 0.0)
        (age-factor 0.0))
    
    ;; File type factor
    (let ((file-type (forj-detect-file-type file-path)))
      (setq type-factor
            (cond
             ((eq file-type 'elisp) 0.4)
             ((memq file-type '(python javascript typescript)) 0.3)
             ((memq file-type '(markdown org-mode)) 0.2)
             ((eq file-type 'text) 0.1)
             (t 0.0))))
    
    ;; Size factor (prefer reasonably sized files)
    (let ((size (nth 7 (file-attributes file-path))))
      (when size
        (setq size-factor
              (cond
               ((< size 1000) 0.0)      ; Too small
               ((< size 10000) 0.2)     ; Good size
               ((< size 50000) 0.1)     ; Large but manageable
               (t -0.1)))))             ; Too large
    
    ;; Age factor (recently modified files more relevant)
    (let* ((mod-time (nth 5 (file-attributes file-path)))
           (age-days (/ (float-time (time-subtract (current-time) mod-time)) 86400)))
      (setq age-factor
            (cond
             ((< age-days 1) 0.2)       ; Modified today
             ((< age-days 7) 0.1)       ; Modified this week
             ((< age-days 30) 0.0)      ; Modified this month
             (t -0.1))))                ; Older
    
    (max 0.0 (min 1.0 (+ base-score type-factor size-factor age-factor)))))

(defun forj-calculate-compilation-relevance (errors)
  "Calculate relevance score for compilation context based on ERRORS."
  (let ((error-count (length (cl-remove-if-not 
                             (lambda (err) (eq (plist-get err :severity) 'error))
                             errors)))
        (warning-count (length (cl-remove-if-not 
                               (lambda (err) (eq (plist-get err :severity) 'warning))
                               errors))))
    (cond
     ((> error-count 0) 0.9)           ; Errors are highly relevant
     ((> warning-count 5) 0.7)         ; Many warnings quite relevant
     ((> warning-count 0) 0.5)         ; Some warnings moderately relevant
     (t 0.3))))                        ; Clean compilation less relevant

(defun forj-extract-compilation-errors (content &optional include-warnings)
  "Extract error and optionally warning information from compilation CONTENT."
  (let ((errors '())
        (lines (split-string content "\n")))
    
    (dolist (line lines)
      ;; Match common error patterns
      (cond
       ;; GCC/Clang style: file:line:col: error: message
       ((string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): error: \\(.+\\)$" line)
        (push `(:file ,(match-string 1 line)
                :line ,(string-to-number (match-string 2 line))
                :column ,(string-to-number (match-string 3 line))
                :severity error
                :message ,(match-string 4 line))
              errors))
       
       ;; Warning pattern (if requested)
       ((and include-warnings
             (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): warning: \\(.+\\)$" line))
        (push `(:file ,(match-string 1 line)
                :line ,(string-to-number (match-string 2 line))
                :column ,(string-to-number (match-string 3 line))
                :severity warning
                :message ,(match-string 4 line))
              errors))
       
       ;; Emacs Lisp error pattern
       ((string-match "^.*\\.el:\\([0-9]+\\):\\([0-9]+\\):.*Error:\\s-*\\(.+\\)$" line)
        (push `(:file ,(match-string 1 line)
                :line ,(string-to-number (match-string 1 line))
                :column ,(string-to-number (match-string 2 line))
                :severity error
                :message ,(match-string 3 line))
              errors))))
    
    (nreverse errors)))

(defun forj-simple-file-scan (directory)
  "Simple file scanner fallback when forj-scan-directory-recursive unavailable."
  (let ((files '()))
    (dolist (file (directory-files directory t))
      (when (and (file-regular-p file)
                 (not (string-match-p "^\\." (file-name-nondirectory file))))
        (push `(:path ,file
                :size ,(nth 7 (file-attributes file))
                :type ,(forj-detect-file-type file))
              files)))
    (nreverse files)))

;;; Error Handling

(defun forj-context-error (message &rest args)
  "Report context-related errors through centralized system."
  (if (featurep 'forj-error-system)
      (apply #'forj-user-error
             (format "Context Error: %s" message)
             :context "Context Management"
             :recovery '("Check context sources are accessible"
                        "Verify file permissions"
                        "Try reducing context size")
             args)
    (apply #'error (format "Context Error: %s" message) args)))

;;; Performance Monitoring

(defun forj-context-get-performance-metrics ()
  "Get performance metrics for context operations."
  (when forj-context-performance-monitoring
    (let ((metrics '()))
      (maphash (lambda (operation time)
                 (push (cons operation time) metrics))
               forj-context-performance-metrics)
      metrics)))

(defun forj-context-clear-performance-metrics ()
  "Clear performance metrics."
  (when forj-context-performance-monitoring
    (clrhash forj-context-performance-metrics)))

;;; Cache Management

(defun forj-context-clear-cache ()
  "Clear the context content cache."
  (when forj-context-cache-enabled
    (clrhash forj-context-content-cache)))

(defun forj-context-cache-size ()
  "Get the current size of the context cache."
  (if forj-context-cache-enabled
      (hash-table-count forj-context-content-cache)
    0))

(provide 'forj-context)
;;; forj-context.el ends here