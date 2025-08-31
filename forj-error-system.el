;;; forj-error-system.el --- Centralized Error Handling System for Forj -*- lexical-binding: t -*-

;;; Commentary:
;; 
;; This module provides a comprehensive, centralized error handling system for
;; Forj.el that addresses inconsistencies in error reporting and adds enhanced
;; capabilities for both human developers and AI coding agents.
;;
;; Key Features:
;; - Unified error handling patterns across all modules
;; - Structured error messages with comprehensive context
;; - Dual-format error output (human-readable + machine-readable)
;; - Error classification and categorization system
;; - Recovery suggestions and automated recovery mechanisms
;; - Comprehensive error logging with multiple output targets
;; - Integration with existing condition-case patterns
;;
;; Usage:
;;   (forj-error 'user-error "Invalid input" :context "file validation")
;;   (forj-user-error "Configuration missing" :recovery "Run forj-setup")
;;   (forj-system-error "Permission denied" :details '(:file "/path/to/file"))
;;

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Error Classification System

(defconst forj-error-types
  '((user-error . "User input or configuration error")
    (system-error . "System or environment error")
    (network-error . "Network connectivity or API error")
    (validation-error . "Data validation or syntax error")
    (file-error . "File system operation error")
    (api-error . "External API interaction error"))
  "Error type classification with descriptions.")

(defconst forj-error-severities
  '((critical . "System cannot function")
    (high . "Major functionality impaired")
    (medium . "Functionality degraded")
    (low . "Minor issue or warning")
    (info . "Informational message"))
  "Error severity levels with descriptions.")

;;; Error Context Structure

(cl-defstruct forj-error-context
  "Structure for comprehensive error context information."
  type                    ; Error type symbol (user-error, system-error, etc.)
  severity               ; Severity level (critical, high, medium, low, info)
  message               ; Human-readable error message
  details               ; Additional details as plist or alist
  function-name         ; Function where error occurred
  file-path             ; File path related to error (if applicable)
  operation-context     ; Context of the operation being performed
  recovery-suggestions  ; List of suggested recovery actions
  timestamp             ; When error occurred
  machine-readable-data ; Structured data for automated processing
  stack-trace          ; Function call stack (if available)
  related-errors)      ; List of related error contexts

;;; Configuration

(defgroup forj-error nil
  "Error handling configuration for Forj."
  :group 'forj
  :prefix "forj-error-")

(defcustom forj-error-log-targets '(messages-buffer conversation-buffer)
  "List of targets for error logging output.
Available targets: messages-buffer, conversation-buffer, file, stdout, stderr"
  :type '(repeat (choice (const messages-buffer)
                        (const conversation-buffer)
                        (const file)
                        (const stdout)
                        (const stderr)))
  :group 'forj-error)

(defcustom forj-error-log-file
  (expand-file-name "forj-errors.log" user-emacs-directory)
  "File path for error logging when 'file is in forj-error-log-targets."
  :type 'file
  :group 'forj-error)

(defcustom forj-error-enable-recovery-suggestions t
  "Whether to provide automated recovery suggestions for errors."
  :type 'boolean
  :group 'forj-error)

(defcustom forj-error-enable-stack-trace nil
  "Whether to capture function call stack traces for errors.
Warning: This may impact performance."
  :type 'boolean
  :group 'forj-error)

(defcustom forj-error-max-history 100
  "Maximum number of errors to keep in history."
  :type 'integer
  :group 'forj-error)

;;; Error History and Tracking

(defvar forj-error-history nil
  "List of recent error contexts for debugging and pattern analysis.")

(defvar forj-error-metrics (make-hash-table :test 'equal)
  "Hash table tracking error frequency and patterns.")

;;; Core Error Handling Functions

(defun forj-error (type message &rest args)
  "Create and handle error with centralized system.

TYPE is the error type symbol (user-error, system-error, etc.).
MESSAGE is the human-readable error description.

Optional keyword arguments:
  :details - Additional details as plist or string
  :context - Operation context description
  :function - Function name where error occurred
  :file - File path related to error
  :recovery - Recovery suggestions (string or list)
  :severity - Error severity level
  :machine-data - Structured data for automated processing
  :related - List of related error contexts"
  (let* ((error-context (forj-create-error-context type message args))
         (should-signal (memq type '(user-error system-error file-error))))
    
    ;; Log the error to configured targets
    (forj-log-error-to-targets error-context)
    
    ;; Add to error history
    (forj-add-to-error-history error-context)
    
    ;; Update error metrics
    (forj-update-error-metrics error-context)
    
    ;; Signal the error if it should be signaled
    (when should-signal
      (let ((error-symbol (pcase type
                           ('user-error 'user-error)
                           ('file-error 'file-error)
                           (_ 'error)))
            (formatted-message (forj-format-human-error error-context)))
        (signal error-symbol (list formatted-message))))
    
    ;; Return the error context for further processing
    error-context))

(defun forj-user-error (message &rest args)
  "Handle user errors with recovery guidance.
MESSAGE is the error description.
Optional :recovery argument provides recovery suggestions."
  (apply #'forj-error 'user-error message 
         :severity 'medium
         args))

(defun forj-system-error (message &rest args)
  "Handle system errors with environment details.
MESSAGE is the error description.
Optional :context argument provides system context."
  (apply #'forj-error 'system-error message 
         :severity 'high
         args))

(defun forj-network-error (message &rest args)
  "Handle network errors with request details.
MESSAGE is the error description.
Optional :details argument provides request context."
  (apply #'forj-error 'network-error message 
         :severity 'medium
         args))

(defun forj-validation-error (message &rest args)
  "Handle validation errors with data context.
MESSAGE is the error description.
Optional :details argument provides validation context."
  (apply #'forj-error 'validation-error message 
         :severity 'low
         args))

(defun forj-api-error (message &rest args)
  "Handle API errors with request/response context.
MESSAGE is the error description.
Optional :details argument provides API context."
  (apply #'forj-error 'api-error message 
         :severity 'medium
         args))

;;; Error Context Creation and Enrichment

(defun forj-create-error-context (type message args)
  "Create comprehensive error context from TYPE, MESSAGE, and ARGS."
  (let* ((details (plist-get args :details))
         (context (plist-get args :context))
         (function-name (or (plist-get args :function) 
                           (forj-get-calling-function)))
         (file-path (plist-get args :file))
         (recovery (plist-get args :recovery))
         (severity (or (plist-get args :severity) 'medium))
         (machine-data (plist-get args :machine-data))
         (related (plist-get args :related))
         (timestamp (current-time)))
    
    ;; Auto-generate recovery suggestions if enabled and not provided
    (unless recovery
      (when forj-error-enable-recovery-suggestions
        (setq recovery (forj-get-error-recovery-suggestions type))))
    
    ;; Enrich context with environment information
    (let ((enriched-context (forj-enrich-error-context 
                            context type function-name file-path)))
      
      (make-forj-error-context
       :type type
       :severity severity
       :message message
       :details details
       :function-name function-name
       :file-path file-path
       :operation-context enriched-context
       :recovery-suggestions recovery
       :timestamp timestamp
       :machine-readable-data machine-data
       :stack-trace (when forj-error-enable-stack-trace
                     (forj-get-stack-trace))
       :related-errors related))))

(defun forj-enrich-error-context (context error-type function-name file-path)
  "Add contextual information to error CONTEXT based on ERROR-TYPE, FUNCTION-NAME, and FILE-PATH."
  (let ((enriched-context (or context "")))
    
    ;; Add function context
    (when function-name
      (setq enriched-context 
            (format "%s | Function: %s" enriched-context function-name)))
    
    ;; Add file context
    (when file-path
      (setq enriched-context 
            (format "%s | File: %s" enriched-context file-path)))
    
    ;; Add error type specific context
    (let ((type-context (pcase error-type
                         ('user-error "User action required")
                         ('system-error "System environment issue")
                         ('network-error "Network connectivity problem")
                         ('validation-error "Input validation failed")
                         ('file-error "File system operation failed")
                         ('api-error "External API interaction failed")
                         (_ "General error"))))
      (setq enriched-context 
            (format "%s | Type: %s" enriched-context type-context)))
    
    ;; Add timestamp and session info
    (format "%s | Time: %s | Session: %s" 
            enriched-context
            (format-time-string "%H:%M:%S")
            (or (bound-and-true-p forj-session-id) "default"))))

(defun forj-get-calling-function ()
  "Get the name of the function that called the error handler."
  (condition-case nil
      (let ((frame (backtrace-frame 4))) ; Skip error handling frames
        (when frame
          (let ((func (cadr frame)))
            (cond
             ((symbolp func) (symbol-name func))
             ((and (listp func) (eq (car func) 'lambda))
              "lambda")
             (t "unknown")))))
    (error "unknown")))

(defun forj-get-stack-trace ()
  "Get current function call stack trace."
  (condition-case nil
      (let ((frames '())
            (frame-index 5)) ; Skip error handling frames
        (while (backtrace-frame frame-index)
          (let* ((frame (backtrace-frame frame-index))
                 (func (cadr frame)))
            (when func
              (push (cond
                     ((symbolp func) (symbol-name func))
                     ((and (listp func) (eq (car func) 'lambda))
                      "lambda")
                     (t (format "%s" func)))
                    frames)))
          (setq frame-index (1+ frame-index))
          (when (> frame-index 20) ; Limit stack depth
            (push "..." frames)
            (setq frame-index nil)))
        (nreverse frames))
    (error '("stack-trace-unavailable"))))

;;; Recovery Suggestion System

(defun forj-get-error-recovery-suggestions (error-type)
  "Get automated recovery suggestions for ERROR-TYPE."
  (pcase error-type
    ('user-error 
     '("Check your input parameters"
       "Verify configuration settings"
       "Consult the documentation"))
    
    ('system-error 
     '("Check file permissions"
       "Verify system requirements"
       "Restart Emacs if necessary"))
    
    ('network-error 
     '("Check internet connectivity"
       "Verify API endpoint availability"
       "Check firewall settings"
       "Try again in a few moments"))
    
    ('validation-error 
     '("Check input format and syntax"
       "Verify required fields are provided"
       "Review validation rules"))
    
    ('file-error 
     '("Check file path exists"
       "Verify read/write permissions"
       "Ensure sufficient disk space"
       "Check file is not locked by another process"))
    
    ('api-error 
     '("Verify API key is correct"
       "Check API service status"
       "Review request parameters"
       "Check rate limiting"))
    
    (_ '("Review the error details"
         "Check system logs"
         "Try the operation again"))))

;;; Error Formatting Functions

(defun forj-format-human-error (error-context)
  "Format ERROR-CONTEXT for human-readable display."
  (let* ((type (forj-error-context-type error-context))
         (severity (forj-error-context-severity error-context))
         (message (forj-error-context-message error-context))
         (context (forj-error-context-operation-context error-context))
         (recovery (forj-error-context-recovery-suggestions error-context))
         (function-name (forj-error-context-function-name error-context))
         (file-path (forj-error-context-file-path error-context)))
    
    (let ((formatted-message (format "[%s] %s" 
                                    (upcase (symbol-name severity))
                                    message)))
      
      ;; Add context if available
      (when context
        (setq formatted-message 
              (format "%s\nContext: %s" formatted-message context)))
      
      ;; Add location information
      (when (or function-name file-path)
        (let ((location ""))
          (when function-name
            (setq location (format "Function: %s" function-name)))
          (when file-path
            (setq location (if (string-empty-p location)
                              (format "File: %s" (file-name-nondirectory file-path))
                            (format "%s, File: %s" location (file-name-nondirectory file-path)))))
          (setq formatted-message 
                (format "%s\nLocation: %s" formatted-message location))))
      
      ;; Add recovery suggestions
      (when recovery
        (let ((suggestions (if (listp recovery)
                              (mapconcat (lambda (s) (format "  • %s" s)) recovery "\n")
                            (format "  • %s" recovery))))
          (setq formatted-message 
                (format "%s\nRecovery:\n%s" formatted-message suggestions))))
      
      formatted-message)))

(defun forj-format-machine-error (error-context)
  "Format ERROR-CONTEXT for machine-readable processing."
  (let ((machine-data 
         `((type . ,(forj-error-context-type error-context))
           (severity . ,(forj-error-context-severity error-context))
           (message . ,(forj-error-context-message error-context))
           (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S" 
                                           (forj-error-context-timestamp error-context)))
           (context . ,(forj-error-context-operation-context error-context))
           (function . ,(forj-error-context-function-name error-context))
           (file . ,(forj-error-context-file-path error-context))
           (recovery . ,(forj-error-context-recovery-suggestions error-context))
           (details . ,(forj-error-context-details error-context)))))
    
    ;; Add stack trace if available
    (when (forj-error-context-stack-trace error-context)
      (setq machine-data 
            (append machine-data 
                    `((stack-trace . ,(forj-error-context-stack-trace error-context))))))
    
    ;; Add any additional machine-readable data
    (when (forj-error-context-machine-readable-data error-context)
      (setq machine-data 
            (append machine-data 
                    (forj-error-context-machine-readable-data error-context))))
    
    (condition-case nil
        (json-encode machine-data)
      (error (format "%S" machine-data)))))

;;; Error Logging System

(defun forj-log-error-to-targets (error-context)
  "Log ERROR-CONTEXT to all configured output targets."
  (dolist (target forj-error-log-targets)
    (condition-case err
        (pcase target
          ('messages-buffer (forj-log-to-messages-buffer error-context))
          ('conversation-buffer (forj-log-to-conversation-buffer error-context))
          ('file (forj-log-to-file error-context))
          ('stdout (forj-log-to-stdout error-context))
          ('stderr (forj-log-to-stderr error-context)))
      (error 
       (message "Error logging to target %s: %s" 
                target (error-message-string err))))))

(defun forj-log-to-messages-buffer (error-context)
  "Log ERROR-CONTEXT to the *Messages* buffer."
  (let ((formatted-message (forj-format-human-error error-context)))
    (message "Forj Error: %s" formatted-message)))

(defun forj-log-to-conversation-buffer (error-context)
  "Log ERROR-CONTEXT to the conversation buffer."
  (when (and (boundp 'forj-conversation-buffer) 
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n🚨 %s\n" 
                         (forj-format-human-error error-context))))))))

(defun forj-log-to-file (error-context)
  "Log ERROR-CONTEXT to the configured log file."
  (let ((log-entry (format "[%s] %s\nMachine: %s\n---\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S" 
                                            (forj-error-context-timestamp error-context))
                          (forj-format-human-error error-context)
                          (forj-format-machine-error error-context))))
    (with-temp-buffer
      (when (file-exists-p forj-error-log-file)
        (insert-file-contents forj-error-log-file))
      (goto-char (point-max))
      (insert log-entry)
      (write-file forj-error-log-file))))

(defun forj-log-to-stdout (error-context)
  "Log ERROR-CONTEXT to stdout for CLI/testing compatibility."
  (princ (format "FORJ_ERROR: %s\n" (forj-format-machine-error error-context))))

(defun forj-log-to-stderr (error-context)
  "Log ERROR-CONTEXT to stderr for CLI/testing compatibility."
  (with-temp-buffer
    (insert (format "FORJ_ERROR: %s\n" (forj-format-machine-error error-context)))
    (call-process-region (point-min) (point-max) "cat" nil nil nil)))

;;; Error History and Analysis

(defun forj-add-to-error-history (error-context)
  "Add ERROR-CONTEXT to error history with size limiting."
  (push error-context forj-error-history)
  (when (> (length forj-error-history) forj-error-max-history)
    (setq forj-error-history (cl-subseq forj-error-history 0 forj-error-max-history))))

(defun forj-update-error-metrics (error-context)
  "Update error frequency metrics for ERROR-CONTEXT."
  (let* ((type (forj-error-context-type error-context))
         (function-name (forj-error-context-function-name error-context))
         (current-count (gethash type forj-error-metrics 0)))
    
    ;; Update type frequency
    (puthash type (1+ current-count) forj-error-metrics)
    
    ;; Update function-specific frequency
    (when function-name
      (let* ((function-key (format "%s:%s" type function-name))
             (function-count (gethash function-key forj-error-metrics 0)))
        (puthash function-key (1+ function-count) forj-error-metrics)))))

(defun forj-get-error-history (&optional type limit)
  "Get error history, optionally filtered by TYPE and limited by LIMIT."
  (let ((filtered-history (if type
                             (cl-remove-if-not 
                              (lambda (ctx) (eq (forj-error-context-type ctx) type))
                              forj-error-history)
                           forj-error-history)))
    (if limit
        (cl-subseq filtered-history 0 (min limit (length filtered-history)))
      filtered-history)))

(defun forj-get-error-metrics (&optional type)
  "Get error frequency metrics, optionally filtered by TYPE."
  (if type
      (gethash type forj-error-metrics 0)
    (let ((metrics '()))
      (maphash (lambda (key value) (push (cons key value) metrics)) 
               forj-error-metrics)
      metrics)))

;;; Error Analysis and Reporting

(defun forj-analyze-error-patterns ()
  "Analyze error patterns and return insights."
  (let* ((total-errors (length forj-error-history))
         (type-counts (make-hash-table :test 'equal))
         (function-counts (make-hash-table :test 'equal))
         (recent-errors (cl-subseq forj-error-history 0 (min 10 total-errors))))
    
    ;; Count by type and function
    (dolist (error-ctx forj-error-history)
      (let ((type (forj-error-context-type error-ctx))
            (func (forj-error-context-function-name error-ctx)))
        (puthash type (1+ (gethash type type-counts 0)) type-counts)
        (when func
          (puthash func (1+ (gethash func function-counts 0)) function-counts))))
    
    ;; Generate analysis report
    (list :total-errors total-errors
          :type-distribution (forj-hash-to-alist type-counts)
          :function-distribution (forj-hash-to-alist function-counts)
          :recent-errors recent-errors
          :most-common-type (forj-find-max-key type-counts)
          :most-problematic-function (forj-find-max-key function-counts))))

(defun forj-hash-to-alist (hash-table)
  "Convert HASH-TABLE to association list."
  (let ((alist '()))
    (maphash (lambda (key value) (push (cons key value) alist)) hash-table)
    alist))

(defun forj-find-max-key (hash-table)
  "Find the key with maximum value in HASH-TABLE."
  (let ((max-key nil)
        (max-value 0))
    (maphash (lambda (key value)
               (when (> value max-value)
                 (setq max-key key
                       max-value value)))
             hash-table)
    max-key))

;;; Integration Helpers for Existing Code

(defun forj-wrap-existing-error (original-error-data &rest args)
  "Wrap an existing error with centralized error handling.
ORIGINAL-ERROR-DATA should be the original error information.
ARGS are additional keyword arguments for error context."
  (let* ((error-type (or (plist-get args :type) 'system-error))
         (message (if (stringp original-error-data)
                     original-error-data
                   (format "%s" original-error-data))))
    (apply #'forj-error error-type message args)))

(defmacro forj-with-error-handling (error-type &rest body)
  "Execute BODY with centralized error handling for ERROR-TYPE.
Any errors thrown in BODY will be caught and processed through
the centralized error system."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (forj-wrap-existing-error (error-message-string err)
                               :type ,error-type
                               :details (list :original-error err)))))

;;; Error Recovery and Automation

(defun forj-attempt-error-recovery (error-context)
  "Attempt automated recovery for ERROR-CONTEXT.
Returns t if recovery was successful, nil otherwise."
  (let ((recovery-suggestions (forj-error-context-recovery-suggestions error-context))
        (error-type (forj-error-context-type error-context)))
    
    (when recovery-suggestions
      (pcase error-type
        ('file-error (forj-attempt-file-error-recovery error-context))
        ('network-error (forj-attempt-network-error-recovery error-context))
        ('api-error (forj-attempt-api-error-recovery error-context))
        (_ nil)))))

(defun forj-attempt-file-error-recovery (error-context)
  "Attempt recovery for file errors in ERROR-CONTEXT."
  (let ((file-path (forj-error-context-file-path error-context)))
    (when file-path
      (condition-case nil
          (progn
            ;; Try to create parent directory if it doesn't exist
            (let ((dir (file-name-directory file-path)))
              (unless (file-exists-p dir)
                (make-directory dir t)
                t)))
        (error nil)))))

(defun forj-attempt-network-error-recovery (error-context)
  "Attempt recovery for network errors in ERROR-CONTEXT."
  ;; For now, just return nil - network recovery is complex
  ;; Future: implement retry logic, connection testing, etc.
  nil)

(defun forj-attempt-api-error-recovery (error-context)
  "Attempt recovery for API errors in ERROR-CONTEXT."
  ;; For now, just return nil - API recovery depends on specific errors
  ;; Future: implement retry logic, token refresh, etc.
  nil)

;;; Public API for Error System Management

(defun forj-clear-error-history ()
  "Clear the error history and reset metrics."
  (interactive)
  (setq forj-error-history nil)
  (clrhash forj-error-metrics)
  (message "Forj error history and metrics cleared"))

(defun forj-show-error-report ()
  "Display comprehensive error report."
  (interactive)
  (let* ((analysis (forj-analyze-error-patterns))
         (buffer-name "*Forj Error Report*")
         (buffer (get-buffer-create buffer-name)))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Forj Error Analysis Report\n")
      (insert "=========================\n\n")
      
      (insert (format "Total Errors: %d\n" (plist-get analysis :total-errors)))
      (insert (format "Most Common Type: %s\n" (plist-get analysis :most-common-type)))
      (insert (format "Most Problematic Function: %s\n\n" (plist-get analysis :most-problematic-function)))
      
      (insert "Error Type Distribution:\n")
      (dolist (type-count (plist-get analysis :type-distribution))
        (insert (format "  %s: %d\n" (car type-count) (cdr type-count))))
      
      (insert "\nRecent Errors:\n")
      (dolist (error-ctx (plist-get analysis :recent-errors))
        (insert (format "  [%s] %s: %s\n"
                        (format-time-string "%H:%M:%S" 
                                          (forj-error-context-timestamp error-ctx))
                        (forj-error-context-type error-ctx)
                        (forj-error-context-message error-ctx))))
      
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(provide 'forj-error-system)
;;; forj-error-system.el ends here