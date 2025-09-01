;;; forj-query-interpreter.el --- Natural Language Query Interpretation Layer -*- lexical-binding: t -*-

;;; Commentary:
;; This module implements the Natural Language Query Interpretation Layer as
;; specified in specs/004-query-interpretation-layer-specs.md.
;;
;; Key Features:
;; - AI-first interpretation using Gemini API via forj-api.el
;; - Intent classification (question, request, complaint)
;; - Tool plan generation with confidence scoring
;; - Fallback pattern registry for AI-unavailable scenarios
;; - Integration with forj-tools.el dispatcher
;; - Context resolution for "this directory", "current file", etc.

;;; Code:

(require 'json)
(require 'forj-api)
(require 'forj-tools)
(require 'forj-error-system)

(defgroup forj-query-interpreter nil
  "Natural language query interpretation for Forj."
  :group 'forj
  :prefix "forj-query-")

;;; Configuration

(defcustom forj-query-ai-enabled t
  "Enable AI-powered query interpretation using Gemini API."
  :type 'boolean
  :group 'forj-query-interpreter)

(defcustom forj-query-confidence-threshold 0.7
  "Minimum confidence threshold for auto-executing tool plans."
  :type 'float
  :group 'forj-query-interpreter)

(defcustom forj-query-timeout 5
  "Timeout in seconds for AI query interpretation requests."
  :type 'integer
  :group 'forj-query-interpreter)

(defcustom forj-query-max-plan-calls 5
  "Maximum number of tool calls allowed in a single AI plan."
  :type 'integer
  :group 'forj-query-interpreter)

;;; Data Structures

(defconst forj-query--intent-types '(question request complaint)
  "Valid intent types for query classification.")

(defconst forj-query--intent-mapping
  '((question . answer_only)
    (request . tool_plan)
    (complaint . escalate))
  "Mapping from AI intent classification to control actions.")

;;; Context Resolution

(defun forj-query--resolve-context-references (args)
  "Resolve context references in ARGS like 'this directory' to actual paths."
  (let ((resolved-args (copy-alist args)))
    (dolist (key-value resolved-args)
      (when (stringp (cdr key-value))
        (let ((value (cdr key-value)))
          (cond
           ;; Directory context
           ((string-match-p "\\bthis directory\\b\\|\\bhere\\b\\|\\bcurrent directory\\b" value)
            (setcdr key-value (expand-file-name default-directory)))
           ;; File context - if we're in a file buffer
           ((and (string-match-p "\\bcurrent file\\b\\|\\bthis file\\b" value)
                 (buffer-file-name))
            (setcdr key-value (buffer-file-name)))
           ;; Project root context
           ((string-match-p "\\bproject\\b\\|\\bproject root\\b" value)
            (setcdr key-value (forj--project-root)))))))
    resolved-args))

;;; Fallback Pattern Registry

(defvar forj-query--fallback-patterns
  '(;; Directory/file listing patterns
    (:pattern "\\b(?:what'?s in|list|show).{0,20}\\b(?:this )?(?:directory|folder|dir)\\b"
     :intent list_files
     :args ((:directory . ".")))
    
    (:pattern "\\b(?:list|show).{0,10}\\bfiles?\\b"
     :intent list_files
     :args ((:directory . ".")))
     
    ;; Search patterns
    (:pattern "\\b(?:find|search|look for).{1,30}\\b"
     :intent search
     :extractor forj-query--extract-search-query)
     
    ;; File reading patterns
    (:pattern "\\b(?:read|show|open|cat).{1,20}\\b(?:file|contents?)\\b"
     :intent read_file
     :extractor forj-query--extract-file-path)
     
    ;; Directory navigation
    (:pattern "\\b(?:where am i|current directory|what directory|pwd)\\b"
     :intent list_files
     :args ((:directory . ".")))
     
    ;; Project overview
    (:pattern "\\b(?:what'?s in|overview of|structure of).{0,20}\\bproject\\b"
     :intent list_files
     :args ((:directory . ".") (:max_depth . 3))))
  "Fallback patterns for when AI interpretation is unavailable.")

(defun forj-query--extract-search-query (query)
  "Extract search query parameters from natural language QUERY."
  (let ((clean-query (replace-regexp-in-string 
                      "\\b(?:find|search|look for)\\s+" "" query t t)))
    (list (cons 'query (string-trim clean-query)))))

(defun forj-query--extract-file-path (query)
  "Extract file path from natural language QUERY."
  ;; Simple extraction - look for filename patterns
  (if (string-match "\\([a-zA-Z0-9._-]+\\.[a-z]+\\)" query)
      (list (cons 'path (match-string 1 query)))
    ;; Default to current directory if no specific file
    (list (cons 'path "."))))

;;; AI Integration

(defun forj-query--build-ai-system-prompt ()
  "Build system prompt for AI query interpretation."
  (let ((tool-registry (forj-query--get-tool-registry-summary))
        (project-info (forj-query--get-project-info)))
    (format "You are Forj's query interpreter. Your job is to:

1. Classify user intent as: question, request, or complaint
2. If request: determine which tools to use and with what arguments
3. Respond ONLY with valid JSON matching the required schema

Available tools: %s

Project context: %s

Schema for intent classification:
{
  \"intent\": \"question\" | \"request\" | \"complaint\",
  \"confidence\": 0.0-1.0,
  \"reasoning\": \"brief explanation\"
}

Schema for tool planning (only if intent is \"request\"):
{
  \"needs_tools\": true,
  \"tools\": [
    {\"name\": \"tool_name\", \"args\": {\"key\": \"value\"}, \"confidence\": 0.0-1.0}
  ],
  \"rationale\": \"explanation of tool choices\",
  \"notes\": \"any assumptions or clarifications needed\"
}

Rules:
- Only use tools from the available registry
- Validate all arguments match tool requirements
- Use confidence scores to indicate uncertainty
- Keep rationale concise but informative
- Never execute destructive operations without explicit user confirmation"
            tool-registry project-info)))

(defun forj-query--get-tool-registry-summary ()
  "Get summary of available tools from forj-tools registry."
  (if (fboundp 'forj-tools-get-registry-summary)
      ;; Use enhanced registry access if available
      (let ((registry-summary (forj-tools-get-registry-summary)))
        (mapconcat (lambda (tool-desc)
                     (format "%s: %s" 
                             (symbol-name (car tool-desc))
                             (cdr tool-desc)))
                   registry-summary
                   "\n- "))
    ;; Fallback to simple tool list
    (let ((tools '()))
      (when (boundp 'forj-tools--registry)
        (maphash (lambda (name _fn)
                   (push (symbol-name name) tools))
                 forj-tools--registry))
      (mapconcat #'identity (sort tools #'string<) ", "))))

(defun forj-query--get-project-info ()
  "Get basic project information for AI context."
  (let ((root (forj--project-root))
        (files (condition-case nil
                   (forj-tools--list-files '() nil)
                 (error '()))))
    (format "Root: %s, Files: %d, Types: %s"
            root
            (length files)
            (if files
                (mapconcat (lambda (f) (alist-get 'type f))
                          (delete-dups files) ", ")
              "unknown"))))

(defun forj-query--classify-intent (query)
  "Classify intent of natural language QUERY using AI.
Returns plist with :intent, :confidence, :reasoning."
  (when (not forj-query-ai-enabled)
    (forj-user-error "AI query interpretation is disabled"
                     :context "Intent classification"
                     :recovery '("Enable forj-query-ai-enabled"
                                "Use explicit tool calls instead")))
  
  (let* ((system-prompt (forj-query--build-ai-system-prompt))
         (user-prompt (format "Classify this user query and respond with intent classification JSON only:\n\n%s" query))
         (full-prompt (format "%s\n\nUser Query: %s\n\nRespond with intent classification JSON:" system-prompt user-prompt)))
    
    (condition-case err
        (let ((response (forj-api-request full-prompt nil)))
          (if response
              (forj-query--parse-intent-response response)
            (forj-api-error "No response from AI for intent classification"
                           :context "Intent classification"
                           :recovery '("Check API connectivity"
                                      "Try again"
                                      "Use fallback patterns"))))
      (error
       (message "Intent classification failed: %s" (error-message-string err))
       ;; Return default request intent on error
       (list :intent 'request :confidence 0.5 :reasoning "AI classification failed, defaulting to request")))))

(defun forj-query--parse-intent-response (response)
  "Parse AI RESPONSE for intent classification.
Returns plist with :intent, :confidence, :reasoning."
  (condition-case err
      (let* ((json-start (string-match "{" response))
             (json-str (if json-start (substring response json-start) response))
             (json-data (json-read-from-string json-str))
             (intent-str (cdr (assq 'intent json-data)))
             (intent (intern intent-str))
             (confidence (cdr (assq 'confidence json-data)))
             (reasoning (cdr (assq 'reasoning json-data))))
        
        ;; Validate intent type
        (unless (memq intent forj-query--intent-types)
          (setq intent 'request)
          (setq confidence 0.5)
          (setq reasoning (format "Invalid intent '%s', defaulting to request" intent-str)))
        
        ;; Ensure confidence is valid
        (unless (and (numberp confidence) (<= 0.0 confidence 1.0))
          (setq confidence 0.5))
        
        (list :intent intent :confidence confidence :reasoning reasoning))
    (error
     (message "Failed to parse intent response: %s" (error-message-string err))
     (list :intent 'request :confidence 0.3 :reasoning "Failed to parse AI intent response"))))

(defun forj-query--generate-tool-plan (query)
  "Generate tool execution plan for QUERY using AI.
Returns plist with :needs_tools, :tools, :rationale, :notes."
  (when (not forj-query-ai-enabled)
    (forj-user-error "AI query interpretation is disabled"
                     :context "Tool plan generation"
                     :recovery '("Enable forj-query-ai-enabled"
                                "Use explicit tool calls instead")))
  
  (let* ((system-prompt (forj-query--build-ai-system-prompt))
         (user-prompt (format "Generate tool plan JSON for this request:\n\n%s" query))
         (full-prompt (format "%s\n\nUser Request: %s\n\nRespond with tool plan JSON:" system-prompt user-prompt)))
    
    (condition-case err
        (let ((response (forj-api-request full-prompt nil)))
          (if response
              (forj-query--parse-tool-plan-response response)
            (forj-api-error "No response from AI for tool plan generation"
                           :context "Tool plan generation"
                           :recovery '("Check API connectivity"
                                      "Try again"
                                      "Use fallback patterns"))))
      (error
       (message "Tool plan generation failed: %s" (error-message-string err))
       ;; Return empty plan on error
       (list :needs_tools nil :tools '() :rationale "AI tool plan generation failed" :notes "Using fallback")))))

(defun forj-query--parse-tool-plan-response (response)
  "Parse AI RESPONSE for tool plan.
Returns plist with :needs_tools, :tools, :rationale, :notes."
  (condition-case err
      (let* ((json-start (string-match "{" response))
             (json-str (if json-start (substring response json-start) response))
             (json-data (json-read-from-string json-str))
             (needs-tools (cdr (assq 'needs_tools json-data)))
             (tools (cdr (assq 'tools json-data)))
             (rationale (cdr (assq 'rationale json-data)))
             (notes (cdr (assq 'notes json-data))))
        
        ;; Validate and sanitize tools list
        (when (and needs-tools tools)
          (setq tools (forj-query--validate-tool-plan tools)))
        
        (list :needs_tools needs-tools
              :tools (or tools '())
              :rationale (or rationale "")
              :notes (or notes "")))
    (error
     (message "Failed to parse tool plan response: %s" (error-message-string err))
     (list :needs_tools nil :tools '() :rationale "Failed to parse AI response" :notes "Parse error"))))

(defun forj-query--validate-tool-plan (tools)
  "Validate TOOLS list from AI plan against registry.
Returns filtered list of valid tool calls."
  (let ((validated '())
        (count 0))
    (dolist (tool tools)
      (when (< count forj-query-max-plan-calls)
        (let* ((name (cdr (assq 'name tool)))
               (args (cdr (assq 'args tool)))
               (confidence (or (cdr (assq 'confidence tool)) 0.5))
               (tool-symbol (intern name)))
          (if (gethash tool-symbol forj-tools--registry)
              (progn
                (push (list :name tool-symbol
                           :args (forj-query--resolve-context-references args)
                           :confidence confidence)
                      validated)
                (setq count (1+ count)))
            (message "Warning: Unknown tool '%s' in AI plan, skipping" name)))))
    (nreverse validated)))

;;; Fallback Pattern Matching

(defun forj-query--match-fallback-patterns (query)
  "Match QUERY against fallback patterns when AI is unavailable.
Returns plist with :intent, :args or nil if no match."
  (let ((query-lower (downcase query)))
    (catch 'found
      (dolist (pattern forj-query--fallback-patterns)
        (let ((regex (plist-get pattern :pattern)))
          (when (string-match-p regex query-lower)
            (let* ((intent (plist-get pattern :intent))
                   (args (plist-get pattern :args))
                   (extractor (plist-get pattern :extractor))
                   (final-args (if extractor
                                   (funcall extractor query)
                                 args)))
              (throw 'found (list :intent intent
                                 :args (forj-query--resolve-context-references final-args))))))))))

;;; Main Interface

(defun forj-query-interpret (query &optional context)
  "Interpret natural language QUERY and return execution plan.
CONTEXT is optional additional context information.
Returns plist with interpretation results."
  (interactive "sQuery: ")
  
  (condition-case err
      (if forj-query-ai-enabled
          (forj-query--interpret-with-ai query context)
        (forj-query--interpret-with-fallback query context))
    (error
     (message "Query interpretation failed: %s" (error-message-string err))
     (list :status 'error
           :message (error-message-string err)
           :fallback (forj-query--interpret-with-fallback query context)))))

(defun forj-query--interpret-with-ai (query context)
  "Interpret QUERY using AI with optional CONTEXT."
  (message "Interpreting query with AI: %s" query)
  
  ;; Step 1: Classify intent
  (let* ((intent-result (forj-query--classify-intent query))
         (intent (plist-get intent-result :intent))
         (confidence (plist-get intent-result :confidence))
         (reasoning (plist-get intent-result :reasoning)))
    
    (message "Intent classified as: %s (confidence: %.2f)" intent confidence)
    
    ;; Step 2: Handle based on intent
    (pcase intent
      ('question
       (list :status 'question
             :intent intent
             :confidence confidence
             :reasoning reasoning
             :action 'answer_only
             :message "This appears to be a question. Providing direct answer without tool usage."))
      
      ('complaint
       (list :status 'complaint
             :intent intent
             :confidence confidence
             :reasoning reasoning
             :action 'escalate
             :message "This appears to be a complaint. Consider escalating to appropriate channels."))
      
      ('request
       ;; Generate tool plan for requests
       (let ((plan-result (forj-query--generate-tool-plan query)))
         (if (plist-get plan-result :needs_tools)
             (list :status 'tool_plan
                   :intent intent
                   :confidence confidence
                   :reasoning reasoning
                   :action 'tool_plan
                   :plan plan-result)
           (list :status 'answer_only
                 :intent intent
                 :confidence confidence
                 :reasoning reasoning
                 :action 'answer_only
                 :message "Request identified but no tools needed"))))
      
      (_
       ;; Fallback for unknown intents
       (forj-query--interpret-with-fallback query context)))))

(defun forj-query--interpret-with-fallback (query context)
  "Interpret QUERY using fallback patterns."
  (message "Using fallback pattern matching for query: %s" query)
  
  (let ((match (forj-query--match-fallback-patterns query)))
    (if match
        (list :status 'tool_plan
              :intent 'request
              :confidence 0.8
              :reasoning "Pattern-based fallback matching"
              :action 'tool_plan
              :plan (list :needs_tools t
                         :tools (list (list :name (plist-get match :intent)
                                           :args (plist-get match :args)
                                           :confidence 0.8))
                         :rationale "Fallback pattern matching"
                         :notes "AI unavailable, using pattern matching"))
      (list :status 'unknown
            :intent 'question
            :confidence 0.3
            :reasoning "No pattern match found"
            :action 'answer_only
            :message "Unable to interpret query with available patterns"))))

(defun forj-query-build-tool-calls (interpretation)
  "Build tool-call JSON objects from INTERPRETATION results.
Returns list of tool-call JSON strings."
  (let ((plan (plist-get interpretation :plan)))
    (when (and plan (plist-get plan :needs_tools))
      (let ((tools (plist-get plan :tools))
            (tool-calls '())
            (call-id 1))
        (dolist (tool tools)
          (let* ((name (plist-get tool :name))
                 (args (plist-get tool :args))
                 (tool-call (json-encode
                            `((id . ,(format "qry_%d" call-id))
                              (name . ,name)
                              (args . ,args)
                              (meta . ((source . "query-interpreter")
                                      (confidence . ,(plist-get tool :confidence))))))))
            (push tool-call tool-calls)
            (setq call-id (1+ call-id))))
        (nreverse tool-calls)))))

;;; Integration Functions

(defun forj-query-process (query)
  "Process natural language QUERY end-to-end.
This is the main entry point for query interpretation."
  (interactive "sQuery: ")
  
  (let* ((interpretation (forj-query-interpret query))
         (status (plist-get interpretation :status)))
    
    (pcase status
      ('tool_plan
       ;; Build and execute tool calls
       (let ((tool-calls (forj-query-build-tool-calls interpretation)))
         (if tool-calls
             (progn
               (message "Executing %d tool calls from query interpretation" (length tool-calls))
               (dolist (tool-call tool-calls)
                 (let ((result (forj-tools-dispatch tool-call)))
                   (message "Tool result: %s" result)))
               (list :status 'completed :tool_calls tool-calls))
           (list :status 'no_tools :message "No valid tool calls generated"))))
      
      ('question
       (list :status 'question 
             :message "Query interpreted as question - provide direct answer"))
      
      ('complaint
       (list :status 'complaint
             :message "Query interpreted as complaint - consider escalation"))
      
      ('unknown
       (list :status 'unknown
             :message (plist-get interpretation :message)))
      
      ('error
       (list :status 'error
             :message (plist-get interpretation :message)
             :fallback (plist-get interpretation :fallback))))))

(provide 'forj-query-interpreter)

;;; forj-query-interpreter.el ends here