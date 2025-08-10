;;; forj-api.el --- Minimal API Integration for Forj -*- lexical-binding: t -*-

;;; Commentary:
;; Minimal API integration for immediate testing of implemented features
;; Part of Phase 1.4.5 - enables basic AI interaction

;;; Code:

(require 'json)
(require 'url)

;; Forward declarations for functions from main forj.el
(declare-function forj-scan-directory-recursive "forj" (&optional directory max-depth max-files))
(declare-function forj-paren-check "forj" (code-string))

;; Backward compatibility alias for API integration
(defun forj-scan-directory (&optional directory max-depth max-files)
  "Alias for forj-scan-directory-recursive for API integration compatibility."
  (when (fboundp 'forj-scan-directory-recursive)
    (forj-scan-directory-recursive directory max-depth max-files)))

(defcustom forj-api-model "gemini-2.0-flash-exp"
  "Gemini model to use for API calls."
  :type 'string
  :group 'forj)

(defcustom forj-api-timeout 30
  "Timeout for API requests in seconds."
  :type 'integer
  :group 'forj)

(defun forj-get-api-key ()
  "Retrieve Gemini API key from environment variable GEMINI_API_KEY."
  (or (getenv "GEMINI_API_KEY")
      (error "GEMINI_API_KEY environment variable not set")))

(defun forj-build-api-url ()
  "Build the Gemini API URL for the configured model."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
          forj-api-model))

(defun forj-build-payload (prompt &optional context)
  "Build JSON payload for API request with PROMPT and optional CONTEXT."
  (let ((full-prompt (if context
                         (format "Context: %s\n\nUser: %s" context prompt)
                       prompt)))
    ;; Encode to UTF-8 bytes to handle multibyte characters
    (encode-coding-string 
     (json-encode
      `((contents . [((parts . [((text . ,full-prompt))]))])))
     'utf-8)))

(defun forj-api-request (prompt &optional context)
  "Send PROMPT to Gemini API with optional CONTEXT and return response."
  (condition-case err
      (let* ((api-key (forj-get-api-key))
             (url (forj-build-api-url))
             (payload (forj-build-payload prompt context))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("x-goog-api-key" . ,api-key)))
             (url-request-data payload)
             (response-buffer (url-retrieve-synchronously url nil nil forj-api-timeout)))
        (if response-buffer
            (with-current-buffer response-buffer
              (goto-char (point-min))
              (if (re-search-forward "\r?\n\r?\n" nil t)
                  (let ((response (json-read)))
                    (kill-buffer response-buffer)
                    (forj-parse-api-response response))
                (progn
                  (kill-buffer response-buffer)
                  (forj-handle-api-error "Invalid response format")
                  nil)))
          (forj-handle-api-error "No response from API")
          nil))
    (error
     (forj-handle-api-error (error-message-string err))
     nil)))

(defun forj-parse-api-response (response)
  "Parse Gemini API RESPONSE and extract text content."
  (let ((candidates (cdr (assq 'candidates response))))
    (when candidates
      (let ((content (cdr (assq 'content (aref candidates 0)))))
        (when content
          (let ((parts (cdr (assq 'parts content))))
            (when parts
              (cdr (assq 'text (aref parts 0))))))))))

(defun forj-validate-response (response)
  "Validate AI RESPONSE using forj-paren-check."
  (condition-case nil
      (if (fboundp 'forj-paren-check)
          (let ((result (forj-paren-check response)))
            (eq (plist-get result :status) 'balanced))
        ;; Fallback to basic syntax check if forj-paren-check not available
        (with-temp-buffer
          (insert response)
          (condition-case nil
              (progn (check-parens) t)
            (error nil))))
    (error nil)))

(defun forj-build-context ()
  "Build context from current project and conversation."
  (let ((project-context (forj-get-project-context))
        (conversation-context (forj-get-conversation-context)))
    (format "Project: %s\n\nConversation: %s"
            project-context
            conversation-context)))

(defun forj-build-code-context (user-input)
  "Build enhanced context including file contents for code review prompts."
  (let ((project-context (forj-get-project-context))
        (conversation-context (forj-get-conversation-context))
        (code-files-context ""))
    
    ;; If prompt mentions specific files, include their content
    (when (or (string-match-p "forj-api\\.el" user-input)
              (string-match-p "review.*file" user-input)
              (string-match-p "syntax.*error" user-input)
              (string-match-p "code.*quality" user-input))
      (setq code-files-context (forj-get-relevant-file-contents user-input)))
    
    ;; Build context with size limiting
    (let ((full-context (format "Project: %s\n\nRelevant Files:\n%s\n\nConversation: %s"
                               project-context
                               code-files-context
                               conversation-context)))
      ;; Limit context size to prevent API issues (max ~8K chars)
      (if (> (length full-context) 8000)
          (concat (substring full-context 0 7900) "\n\n[Context truncated for API limits]")
        full-context))))

(defun forj-get-relevant-file-contents (user-input)
  "Get contents of files relevant to USER-INPUT prompt."
  (let ((files-to-include '())
        (content ""))
    
    ;; Determine which files to include based on prompt
    (cond
     ((string-match-p "forj-api\\.el" user-input)
      (setq files-to-include '("forj-api.el")))
     ((string-match-p "forj\\.el" user-input)
      (setq files-to-include '("forj.el")))
     ((string-match-p "review.*file\\|syntax.*error\\|code.*quality" user-input)
      (setq files-to-include '("forj-api.el" "forj.el"))))
    
    ;; Read file contents
    (dolist (filename files-to-include)
      (let ((filepath (expand-file-name filename default-directory)))
        (when (file-exists-p filepath)
          (setq content (concat content 
                               (format "\n=== %s ===\n" filename)
                               (with-temp-buffer
                                 (insert-file-contents filepath)
                                 (buffer-string))
                               "\n")))))
    
    (if (string-empty-p content)
        "No specific files identified for review."
      content)))

(defun forj-get-project-context ()
  "Get current project context for API calls."
  (let ((files (forj-scan-directory-recursive default-directory 3 20)))
    (if files
        (format "Project Structure:\n%s\n\nFile Types: %s"
                (mapconcat (lambda (file) 
                             (format "- %s (%s)" 
                                     (plist-get file :path)
                                     (plist-get file :type))) files "\n")
                (mapconcat (lambda (type) (format "%s" type))
                           (delete-dups 
                            (mapcar (lambda (f) (plist-get f :type)) files))
                           ", "))
      "No project files found")))

(defun forj-get-conversation-context ()
  "Get recent conversation context."
  (if (get-buffer "*forj-conversation*")
      (with-current-buffer "*forj-conversation*"
        (let ((content (buffer-string)))
          (if (> (length content) 1000)
              (concat "...\n" (substring content -800))
            content)))
    "No previous conversation"))

(defun forj-prompt (user-input)
  "Process USER-INPUT with AI and apply response."
  (interactive "sForj prompt: ")
  (message "Sending request to AI...")
  ;; Display user input first
  (forj-display-user-input user-input)
  ;; Use enhanced context for code review prompts
  (let* ((use-code-context (or (string-match-p "review.*file\\|syntax.*error\\|code.*quality" user-input)
                               (string-match-p "forj-.*\\.el" user-input)))
         (context (if use-code-context
                      (progn
                        (message "Including file contents for code review...")
                        (forj-build-code-context user-input))
                    (forj-build-context)))
         (response (forj-api-request user-input context)))
    (if response
        (progn
          (forj-display-response response)
          ;; Show the conversation buffer to user
          (display-buffer "*forj-conversation*")
          (when (yes-or-no-p "Apply AI suggestions? ")
            (forj-apply-response response)))
      (message "No response received from AI"))))

(defun forj-display-user-input (input)
  "Display user INPUT in conversation buffer."
  (let ((buffer (get-buffer-create "*forj-conversation*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n--- User ---\n" input)
      (goto-char (point-max)))))

(defun forj-display-response (response)
  "Display AI RESPONSE in conversation buffer and show it to user."
  (let ((buffer (get-buffer-create "*forj-conversation*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n--- AI Response ---\n" response)
      ;; Move cursor to start of new response for easy reading
      (goto-char (point-max)))
    ;; Make sure buffer is visible
    (display-buffer buffer)))

(defun forj-handle-api-error (error-data)
  "Handle API errors gracefully with user feedback."
  (let* ((error-msg (format "API Error: %s" error-data))
         (buffer (get-buffer-create "*forj-conversation*")))
    (message error-msg)
    (condition-case err
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n⚠️ " error-msg "\n"))
      (error (message "Buffer error while logging API error: %s" err)))
    ;; Show the error to user
    (display-buffer buffer)))

(defun forj-process-file-operations (response)
  "Process file operations from AI RESPONSE.
Currently displays the response; file operations are handled through forj-apply-response."
  (message "File operations detected in AI response")
  (forj-display-response response))

(defun forj-apply-response (response)
  "Apply AI RESPONSE to appropriate files with enhanced validation."
  (when (forj-validate-response response)
    (cond
     ;; Code response - apply to current buffer
     ((string-match-p "(defun\\|defvar\\|defcustom" response)
      (when (yes-or-no-p "Apply code changes to current buffer? ")
        (save-excursion
          (goto-char (point-max))
          (insert "\n\n" response))))
     ;; File operation response
     ((string-match-p "Create file:\\|Edit file:" response)
      (forj-process-file-operations response))
     ;; General response - display only
     (t (forj-display-response response)))))

(provide 'forj-api)
;;; forj-api.el ends here