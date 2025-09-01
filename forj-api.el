;;; forj-api.el --- Minimal API Integration for Forj -*- lexical-binding: t -*-

;;; Commentary:
;; Minimal API integration for immediate testing of implemented features
;; Part of Phase 1.4.5 - enables basic AI interaction

;;; Code:

(require 'json)
(require 'url)
(require 'forj-error-system)

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
      (forj-user-error "GEMINI_API_KEY environment variable not set"
                       :context "API configuration"
                       :recovery '("Set the GEMINI_API_KEY environment variable"
                                  "Restart Emacs after setting the variable"
                                  "Verify the API key is correct"))))

(defun forj-build-api-url ()
  "Build the Gemini API URL for the configured model."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
          forj-api-model))

(defun forj-build-payload (prompt &optional context)
  "Build JSON payload for API request with PROMPT and optional CONTEXT."
  (let* ((clean-prompt (forj-clean-multibyte-text prompt))
         (clean-context (when context (forj-clean-multibyte-text context)))
         (full-prompt (if clean-context
                         (format "Context: %s\n\nUser: %s" clean-context clean-prompt)
                       clean-prompt)))
    ;; Encode to UTF-8 bytes to handle multibyte characters
    (encode-coding-string 
     (json-encode
      `((contents . [((parts . [((text . ,full-prompt))]))])))
     'utf-8)))

(defun forj-api-request (prompt &optional context)
  "Send PROMPT to Gemini API with optional CONTEXT and return response."
  (condition-case err
      (forj-with-error-handling 'api-error
        (let* ((api-key (forj-get-api-key))
           (url (forj-build-api-url))
           (payload (forj-build-payload prompt context))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("x-goog-api-key" . ,api-key)))
           (url-request-data payload)
           (response-buffer (progn
                             (message "Attempting HTTP request...")
                             (url-retrieve-synchronously url nil nil forj-api-timeout))))
      (message "HTTP request completed. Response buffer: %s" response-buffer)
      (if response-buffer
          (with-current-buffer response-buffer
            (goto-char (point-min))
            (message "Raw response buffer content: %s" (buffer-string))
            (if (re-search-forward "\r?\n\r?\n" nil t)
                (let* ((json-start (point))
                       (raw-content (buffer-substring json-start (point-max)))
                       ;; Handle chunked transfer encoding - remove chunk size lines
                       (json-content (with-temp-buffer
                                      (insert raw-content)
                                      ;; Remove chunk size lines (hex numbers followed by CRLF)
                                      (goto-char (point-min))
                                      (while (re-search-forward "^[0-9a-fA-F]+\r?\n" nil t)
                                        (replace-match ""))
                                      ;; Remove trailing chunk markers
                                      (goto-char (point-min))
                                      (while (re-search-forward "\r?\n0\r?\n\r?\n" nil t)
                                        (replace-match ""))
                                      (buffer-string))))
                  (message "Processed JSON content to parse: %s" (substring json-content 0 (min 200 (length json-content))))
                  (condition-case json-err
                      (let ((response (with-temp-buffer
                                       (insert json-content)
                                       (goto-char (point-min))
                                       (json-read))))
                        (kill-buffer response-buffer)
                        (message "Successfully parsed JSON response")
                        (forj-parse-api-response response))
                    (error
                     (kill-buffer response-buffer)
                     (forj-api-error (format "JSON parsing failed: %s" (error-message-string json-err))
                                    :context "JSON parsing error"
                                    :details `(:json-content ,json-content :error ,json-err)
                                    :recovery '("Check response format"
                                               "Verify API response structure"
                                               "Check network response"))
                     nil)))
              (progn
                (kill-buffer response-buffer)
                (forj-api-error "Invalid response format - no HTTP headers found"
                               :context "HTTP response format"
                               :details `(:buffer-content ,(buffer-string))
                               :recovery '("Check API endpoint is correct"
                                          "Verify network connectivity"
                                          "Review API documentation"))
                nil)))
        (forj-api-error "No response from API"
                       :context "HTTP request"
                       :details `(:url ,url :timeout ,forj-api-timeout)
                       :recovery '("Check internet connectivity"
                                  "Verify API service status"
                                  "Increase timeout value"
                                  "Try again in a few moments"))
        nil)))
    (error
     (message "API request failed with error: %s" (error-message-string err))
     nil)))

(defun forj-parse-api-response (response)
  "Parse Gemini API RESPONSE and extract text content."
  (condition-case err
      (let ((candidates (cdr (assq 'candidates response))))
        (if candidates
            (let ((content (cdr (assq 'content (aref candidates 0)))))
              (if content
                  (let ((parts (cdr (assq 'parts content))))
                    (if parts
                        (let ((text (cdr (assq 'text (aref parts 0)))))
                          (when text
                            (message "AI response extracted successfully")
                            text))
                      (forj-api-error "No parts found in API response content"
                                     :context "API response parsing"
                                     :details `(:response-structure ,response))))
                (forj-api-error "No content found in API response"
                               :context "API response parsing"
                               :details `(:candidates ,candidates))))
          (forj-api-error "No candidates found in API response"
                         :context "API response parsing"
                         :details `(:response-keys ,(mapcar #'car response)))))
    (error
     (forj-api-error "Failed to parse API response"
                    :context "JSON parsing"
                    :details `(:error ,(error-message-string err)
                              :response-type ,(type-of response))
                    :recovery '("Check API response format"
                               "Verify JSON structure"
                               "Review API documentation"))
     nil)))

(defun forj-validate-response (response)
  "Validate AI RESPONSE using forj-paren-check."
  (condition-case err
      (if (fboundp 'forj-paren-check)
          (let ((result (forj-paren-check response)))
            (if (eq (plist-get result :status) 'balanced)
                t
              (forj-validation-error "AI response contains syntax errors"
                                   :context "Response validation"
                                   :details `(:validation-result ,result)
                                   :recovery '("Request corrected response from AI"
                                              "Check for unbalanced parentheses"
                                              "Verify code syntax"))
              nil))
        ;; Fallback to basic syntax check if forj-paren-check not available
        (with-temp-buffer
          (insert response)
          (condition-case syntax-err
              (progn (check-parens) t)
            (error 
             (forj-validation-error "Response failed basic syntax check"
                                  :context "Basic validation fallback"
                                  :details `(:syntax-error ,(error-message-string syntax-err))
                                  :recovery '("Use forj-paren-check for better validation"
                                             "Check response format manually"))
             nil))))
    (error
     (forj-validation-error "Response validation failed unexpectedly"
                          :context "Validation system error"
                          :details `(:error ,(error-message-string err))
                          :recovery '("Check validation system"
                                     "Report validation bug"))
     nil)))

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

(defun forj-clean-multibyte-text (text)
  "Remove problematic multibyte characters from TEXT for HTTP requests.
This function is specifically for cleaning content sent to APIs, not display content."
  (with-temp-buffer
    (insert text)
    ;; First, extract meaningful content and skip UI formatting
    ;; Remove header/footer UI elements that cause corruption
    (goto-char (point-min))
    (when (re-search-forward "FORJ AI CO-PILOT" nil t)
      ;; Skip the entire header section
      (when (re-search-forward "\n\n" nil t)
        (delete-region (point-min) (point))))
    
    ;; Remove corrupted control sequences
    (goto-char (point-min))
    (while (re-search-forward "_C&.*?_" nil t)
      (replace-match "" nil nil))
    
    ;; Remove corrupted separators
    (goto-char (point-min))  
    (while (re-search-forward "&-&.*?&-&.*?&_.*?&_.*?&-&.*?&-&" nil t)
      (replace-match "" nil nil))
    
    ;; Replace all box-drawing and problematic Unicode characters with ASCII
    (goto-char (point-min))
    (while (re-search-forward "[█░▀▄▌▐▀▄▌▐■□▪▫◆◇┌┐└┘│─┬┴┼├┤╭╮╰╯║═╔╗╚╝╬╠╣╦╩]+" nil t)
      (replace-match "+" nil nil))
    ;; Remove other problematic Unicode ranges
    (goto-char (point-min))
    (while (re-search-forward "[\u2500-\u257F\u2580-\u259F]+" nil t)
      (replace-match "+" nil nil))
    ;; Replace smart quotes with regular quotes
    (goto-char (point-min))
    (while (re-search-forward "\u201C\\|\u201D" nil t)
      (replace-match "\"" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "\u2018\\|\u2019" nil t)
      (replace-match "'" nil nil))
    ;; Replace em/en dashes with regular hyphens
    (goto-char (point-min))
    (while (re-search-forward "\u2013\\|\u2014" nil t)
      (replace-match "-" nil nil))
    ;; Remove any remaining non-ASCII characters that could cause issues
    (goto-char (point-min))
    (while (re-search-forward "[^\x00-\x7F]" nil t)
      (replace-match "?" nil nil))
    (buffer-string)))

(defun forj-get-conversation-context ()
  "Get recent conversation context with multibyte character filtering.
Only clean text when sending to API, not for display purposes."
  (if (get-buffer forj-conversation-buffer)
      (with-current-buffer forj-conversation-buffer
        (let* ((content (buffer-string)))
          ;; Extract only actual conversation content, not UI formatting
          ;; Skip the header and UI elements for context
          (let* ((content-start (if (string-match "\n\n.*---.*Response.*---" content)
                                   (match-end 0)
                                 0))
                (actual-content (substring content content-start))
                (clean-content (forj-clean-multibyte-text actual-content)))
            (if (> (length clean-content) 1000)
                (concat "...\n" (substring clean-content -800))
              clean-content))))
    "No previous conversation"))

(defun forj-prompt (user-input)
  "Process USER-INPUT with AI and apply response.
This function maintains backward compatibility while supporting new context management."
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
          (display-buffer (forj-conversation-buffer))
          ;; Add non-blocking interactive buttons instead of blocking prompt
          (forj-add-response-actions response))
      (message "No response received from AI"))))

;; Enhanced prompt processing function with tool integration
(defun forj-process-prompt-with-context (prompt context-data)
  "Process PROMPT with CONTEXT-DATA using new context management system.
This is the new API entry point that works with the context management system."
  (when (fboundp 'forj-add-to-history)
    (forj-add-to-history 'user prompt))
  
  (message "Processing prompt with %d context sources..." (length context-data))
  
  ;; Check if prompt should trigger tool calls before sending to AI
  (let* ((tool-result (forj-check-for-tool-trigger prompt)))
    (if tool-result
        ;; Direct tool execution triggered
        (progn
          (message "Direct tool execution triggered")
          (forj-display-response tool-result)
          (display-buffer (forj-conversation-buffer)))
      ;; Regular AI processing with tool-call detection
      (let* ((formatted-context (forj-format-context-for-api context-data))
             (enhanced-prompt (if (and context-data (not (string-empty-p formatted-context)))
                                 (format "Context:\n%s\n\nUser Request:\n%s"
                                        formatted-context prompt)
                               prompt))
             (response (forj-api-request enhanced-prompt nil))) ; Don't use old context system
        
        (if response
            (progn
              (when (fboundp 'forj-add-to-history)
                (forj-add-to-history 'assistant response))
              ;; Process response for tool calls before displaying
              (let ((processed-response (forj-process-response-with-tools response)))
                (forj-display-response processed-response))
              ;; Show the conversation buffer to user
              (display-buffer (forj-conversation-buffer))
              ;; Add non-blocking interactive buttons instead of blocking prompt
              (forj-add-response-actions response))
          (message "No response received from AI"))))))

;; Query Interpreter Integration (Specification 004)
(defun forj-process-query-with-interpretation (query)
  "Process QUERY using natural language interpretation layer.
This function integrates the query interpreter with the main conversation flow."
  (interactive "sForj query: ")
  
  ;; Add user input to conversation
  (when (fboundp 'forj-add-to-history)
    (forj-add-to-history 'user query))
  (forj-display-user-input query)
  
  ;; Try query interpretation first if available
  (if (fboundp 'forj-query-interpret)
      (progn
        (message "Interpreting natural language query...")
        (let* ((interpretation (forj-query-interpret query))
               (status (plist-get interpretation :status)))
          
          (pcase status
            ('tool_plan
             ;; Execute tools through interpretation
             (message "Query interpreted as tool plan - executing tools...")
             (let ((execution-result (forj-query-process query)))
               (if (eq (plist-get execution-result :status) 'completed)
                   (progn
                     (when (fboundp 'forj-add-to-history)
                       (forj-add-to-history 'assistant "Tools executed successfully through query interpretation"))
                     (forj-display-response "✅ Query executed through natural language interpretation. Tool results have been processed."))
                 (progn
                   (when (fboundp 'forj-add-to-history)
                     (forj-add-to-history 'assistant (format "Query interpretation failed: %s" 
                                                            (plist-get execution-result :message))))
                   (forj-display-response (format "❌ Query interpretation failed: %s\nFalling back to regular AI processing..." 
                                                 (plist-get execution-result :message)))
                   ;; Fall back to regular prompt processing
                   (forj-fallback-to-regular-processing query)))))
            
            ('question
             ;; Process as regular AI question
             (message "Query interpreted as question - using AI processing...")
             (when (fboundp 'forj-add-to-history)
               (forj-add-to-history 'assistant "Query interpreted as question, processing with AI"))
             (forj-fallback-to-regular-processing query))
            
            ('complaint
             ;; Handle complaints
             (message "Query interpreted as complaint - providing assistance...")
             (when (fboundp 'forj-add-to-history)
               (forj-add-to-history 'assistant "Query interpreted as complaint, offering assistance"))
             (forj-display-response "I understand you may have concerns. How can I help address the issue you're experiencing?"))
            
            (_
             ;; Unknown or error status - fall back
             (message "Query interpretation uncertain - using regular AI processing...")
             (forj-fallback-to-regular-processing query)))))
    
    ;; Query interpreter not available - fall back to regular processing
    (message "Query interpreter not available - using regular AI processing...")
    (forj-fallback-to-regular-processing query)))

(defun forj-add-response-actions (response)
  "Add interactive action buttons to the conversation buffer for RESPONSE."
  (let ((buffer (forj-conversation-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert-button "✓ Apply" 
                      'action (lambda (_button) 
                                (forj-apply-response response)
                                (message "AI suggestions applied"))
                      'follow-link t)
        (insert "  ")
        (insert-button "✗ Dismiss" 
                      'action (lambda (_button) 
                                (message "AI suggestions dismissed"))
                      'follow-link t)
        (insert "\n\n")))))

(defun forj-fallback-to-regular-processing (query)
  "Fall back to regular AI processing for QUERY."
  (let* ((context (forj-build-context))
         (response (forj-api-request query context)))
    (if response
        (progn
          (when (fboundp 'forj-add-to-history)
            (forj-add-to-history 'assistant response))
          (forj-display-response response)
          ;; Show the conversation buffer to user
          (display-buffer (forj-conversation-buffer))
          ;; Add non-blocking interactive buttons instead of blocking prompt
          (forj-add-response-actions response))
      (message "No response received from AI"))))

(defun forj-display-user-input (input)
  "Display user INPUT in conversation buffer."
  (let ((buffer (forj-conversation-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n\n--- User ---\n" input)
        ;; Apply syntax highlighting to the newly inserted content
        (when (featurep 'forj-syntax-highlight)
          (forj-highlight-code-blocks))
        (goto-char (point-max))))))

(defun forj-display-response (response)
  "Display AI RESPONSE in conversation buffer and show it to user."
  (when response
    (let ((buffer (forj-conversation-buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; Ensure clean display by avoiding multibyte corruption
          (let ((clean-response (if (string-match-p "[^\x00-\x7F]" response)
                                   ;; Only clean API content, preserve formatting
                                   (with-temp-buffer
                                     (insert response)
                                     ;; Only replace truly problematic chars, preserve emojis and UI
                                     (goto-char (point-min))
                                     (while (re-search-forward "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" nil t)
                                       (replace-match "" nil nil))
                                     (buffer-string))
                                 response)))
            (insert "\n\n--- AI Response ---\n" clean-response))
          ;; Apply syntax highlighting to the newly inserted content
          (when (featurep 'forj-syntax-highlight)
            (forj-highlight-code-blocks))
          ;; Move cursor to start of new response for easy reading
          (goto-char (point-max))))
      ;; Make sure buffer is visible
      (display-buffer buffer)
      (message "AI response displayed in conversation buffer"))))

(defun forj-handle-api-error (error-data)
  "Handle API errors gracefully with user feedback.
  This function is deprecated - use forj-api-error instead."
  (forj-api-error (format "Legacy API Error: %s" error-data)
                 :context "Legacy error handler"
                 :recovery '("Update code to use forj-api-error"
                            "Check error handling implementation")))

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

;; Tool Integration Functions

(defun forj-check-for-tool-trigger (prompt)
  "Check if PROMPT should directly trigger a tool call.
Returns tool result string if triggered, nil otherwise."
  (when (fboundp 'forj-tools-dispatch)
    (let ((prompt-lower (downcase (string-trim prompt))))
      (cond
       ;; Directory queries
       ((or (string-match-p "what directory" prompt-lower)
            (string-match-p "current directory" prompt-lower)
            (string-match-p "where am i" prompt-lower)
            (string-match-p "pwd" prompt-lower))
        (forj-execute-tool-call "get_current_directory" '() "directory query"))
       
       ;; List files queries  
       ((string-match-p "\\(list files\\|show files\\|what files\\|files in\\)" prompt-lower)
        (forj-execute-tool-call "list_files" '((directory . ".")) "file listing"))
       
       ;; Search queries
       ((string-match-p "\\(search for\\|find\\|look for\\)" prompt-lower)
        (cond
         ;; Pattern: "search for the term 'QUERY'"
         ((string-match "search for\\s-+the\\s-+term\\s-+[\"']\\([^\"'\n]+\\)[\"']" prompt-lower)
          (let ((query (match-string 1 prompt-lower)))
            (forj-execute-tool-call "search" `((query . ,query)) "search query")))
         ;; Pattern: "search for 'QUERY'" or "search for QUERY"
         ((string-match "\\(search for\\|find\\|look for\\)\\s-+[\"']?\\([^\"'\n]+?\\)\\(?:[\"']\\|\\s-+in\\|$\\)" prompt-lower)
          (let ((query (string-trim (match-string 2 prompt-lower))))
            (forj-execute-tool-call "search" `((query . ,query)) "search query")))
         ;; Default: extract any quoted term
         ((string-match "[\"']\\([^\"'\n]+\\)[\"']" prompt-lower)
          (let ((query (match-string 1 prompt-lower)))
            (forj-execute-tool-call "search" `((query . ,query)) "search query")))))
       
       ;; Default: no tool trigger
       (t nil)))))

(defun forj-execute-tool-call (tool-name args description)
  "Execute a tool call and format the result for display."
  (condition-case err
      (let* ((tool-call-json (json-encode `((id . "direct-1") 
                                           (name . ,tool-name) 
                                           (args . ,args) 
                                           (meta . ((description . ,description))))))
             (result-json (forj-tools-dispatch tool-call-json))
             (result (json-read-from-string result-json))
             (success (alist-get 'ok result))
             (payload (if success 
                         (alist-get 'result result)
                       (alist-get 'error result))))
        
        (if success
            ;; Format successful tool result
            (pcase tool-name
              ("get_current_directory" 
               (format "📁 Current directory: %s\n\nAbsolute path: %s" 
                      (alist-get 'directory payload)
                      (alist-get 'absolute_path payload)))
              ("list_files"
               (format "📋 Files in directory:\n\n%s" 
                      (mapconcat (lambda (file)
                                  (format "- %s (%s, %d bytes)" 
                                         (alist-get 'path file)
                                         (alist-get 'type file)
                                         (alist-get 'size file)))
                                payload "\n")))
              ("search"
               (if payload
                   (let* ((matches (if (> (length payload) 20)
                                     (append (seq-take payload 15) 
                                            '(((path . "...") (line . 0) (match_text . "(truncated - showing first 15 matches)"))))
                                   payload))
                          (result-text (mapconcat (lambda (match)
                                                   (format "- %s:%d: %s"
                                                          (alist-get 'path match)
                                                          (alist-get 'line match) 
                                                          (alist-get 'match_text match)))
                                                 matches "\n")))
                     ;; Further truncate if result is still too long for emacsclient
                     (if (> (length result-text) 3000)
                         (concat (substring result-text 0 3000) "\n\n... (results truncated for display)")
                       (format "🔍 Search results:\n\n%s" result-text)))
                 "🔍 No search results found"))
              (_ (format "✅ Tool '%s' executed successfully:\n%s" tool-name (prin1-to-string payload))))
          ;; Format error
          (format "❌ Tool '%s' failed:\n%s" tool-name 
                 (alist-get 'message payload))))
    (error 
     (format "❌ Tool execution error: %s" (error-message-string err)))))

(defun forj-process-response-with-tools (response)
  "Process AI RESPONSE looking for tool-call blocks and execute them.
Returns the response with tool results integrated."
  (if (not (fboundp 'forj-tools-dispatch))
      ;; No tools available - return original response
      response
    
    ;; Look for tool-call blocks in response
    (let ((processed-response response))
      (while (string-match "```tool-call\n\\(\\(?:.\\|\n\\)*?\\)\n```" processed-response)
        (let* ((tool-json (match-string 1 processed-response))
               (start (match-beginning 0))
               (end (match-end 0)))
          
          (condition-case err
              ;; Execute the tool call
              (let* ((result-json (forj-tools-dispatch tool-json))
                     (result (json-read-from-string result-json))
                     (success (alist-get 'ok result))
                     (tool-name (alist-get 'name result))
                     (payload (if success 
                                 (alist-get 'result result)
                               (alist-get 'error result)))
                     (formatted-result (format "```tool-result\n%s\n```\n\n%s"
                                              result-json
                                              (if success
                                                  (format "✅ Tool '%s' executed successfully" tool-name)
                                                (format "❌ Tool '%s' failed: %s" tool-name 
                                                       (alist-get 'message payload))))))
                
                ;; Replace the tool-call block with the result
                (setq processed-response 
                      (concat (substring processed-response 0 start)
                             formatted-result
                             (substring processed-response end))))
            (error 
             ;; Replace with error message
             (setq processed-response 
                   (concat (substring processed-response 0 start)
                          (format "❌ Tool execution error: %s" (error-message-string err))
                          (substring processed-response end)))))))
      
      processed-response)))

(provide 'forj-api)
;;; forj-api.el ends here