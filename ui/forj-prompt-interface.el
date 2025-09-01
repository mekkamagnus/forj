;;; forj-prompt-interface.el --- Interactive Prompt Interface with Context Management -*- lexical-binding: t -*-

;;; Commentary:
;; Interactive prompt interface that provides context management capabilities
;; including @ file selection and / command selection functionality.
;;
;; This implements the Pop-up Buffer Interface from Specification 002.

;;; Code:

(require 'cl-lib)

;; Context modules availability - set by forj.el central loader
(defvar forj-context-available nil "Whether context management is available.")
(defvar forj-context-suggestions-available nil "Whether context suggestions are available.")

;; NOTE: Individual modules should NOT use require/load-file directly.
;; All dependencies are managed by the central forj.el loader.
;; The forj.el loader will set these availability flags appropriately.

;;; Variables

(defvar forj-prompt-buffer-name "*Forj Prompt*"
  "Name of the Forj prompt buffer.")

(defvar forj-prompt-context-sources '()
  "Currently selected context sources for the prompt.")

(defvar forj-prompt-callback-function nil
  "Callback function to process the submitted prompt.")

(defvar forj-prompt-return-buffer nil
  "Buffer to return to after prompt submission.")

;;; Prompt Mode Definition

(define-derived-mode forj-prompt-mode text-mode "Forj-Prompt"
  "Major mode for Forj AI prompt interface."
  ;; The keymap is now set automatically by `define-derived-mode`
  ;; because `forj-prompt-mode-map` follows the naming convention.
  (setq-local fill-column 80)
  (setq-local header-line-format
              "Forj: C-c C-c Submit, C-c C-k Cancel")
  
  ;; Enable word wrap
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  
  ;; Enable syntax highlighting for code blocks
  (font-lock-mode 1)
  
  ;; Evil mode integration - force insert state and prevent overrides
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-insert-state)
    ;; Make sure our keybindings take precedence
    (evil-normalize-keymaps)
    ;; Set up evil keybindings that respect our mode
    (evil-define-key 'insert forj-prompt-mode-map
      "\C-c\C-c" 'forj-prompt-submit
      "\C-c\C-k" 'forj-prompt-cancel)
    (evil-define-key 'normal forj-prompt-mode-map
      "\C-c\C-c" 'forj-prompt-submit
      "\C-c\C-k" 'forj-prompt-cancel)))

;;; Key Bindings

;; Force keymap recreation each time to ensure it's properly populated
(defvar forj-prompt-mode-map nil
  "Keymap for forj-prompt-mode.")

;; Always recreate the keymap to ensure proper binding setup
(let ((map (make-keymap)))
  ;; Inherit from text-mode-map to get standard editing keys
  (set-keymap-parent map text-mode-map)
  ;; Use explicit key sequences for better compatibility
  (define-key map "\C-c\C-c" 'forj-prompt-submit)
  (define-key map "\C-c\C-k" 'forj-prompt-cancel)
  (define-key map "@" 'forj-insert-file)
  (define-key map "/" 'forj-insert-command)
  (define-key map "\C-c\C-s" 'forj-show-context-suggestions)
  (define-key map "\C-c\C-r" 'forj-clear-context-sources)
  (define-key map "\C-c\C-l" 'forj-list-context-sources)
  (setq forj-prompt-mode-map map))

;;; Core Interface Functions

(defun forj-prompt (&optional initial-text callback return-buffer)
  "Open Forj interactive prompt buffer with context management.
INITIAL-TEXT is optional initial content for the prompt.
CALLBACK is optional function to call with the submitted prompt and context.
RETURN-BUFFER is optional buffer to return to after submission."
  (interactive)
  (let ((buffer (get-buffer-create forj-prompt-buffer-name))
        (origin-buffer (or return-buffer (current-buffer))))
    
    ;; Set up callback and return buffer
    (setq forj-prompt-callback-function callback)
    (setq forj-prompt-return-buffer origin-buffer)
    
    ;; Clear previous context sources
    (setq forj-prompt-context-sources '())
    
    (with-current-buffer buffer
      (forj-prompt-mode)
      (forj-setup-prompt-buffer initial-text)
      
      ;; Auto-suggest context based on current state (if available)
      (when (and forj-context-suggestions-available 
                 (boundp 'forj-context-auto-suggestions)
                 forj-context-auto-suggestions)
        (setq forj-prompt-context-sources 
              (forj-suggest-context-sources initial-text)))
      
      (goto-char (point-min)))
    
    (pop-to-buffer buffer)
    (message "Type your request. C-c C-c to submit, C-c C-k to cancel")))

(defun forj-setup-prompt-buffer (&optional initial-text)
  "Initialize the prompt buffer with optional INITIAL-TEXT."
  (erase-buffer)
  
  ;; Insert initial text if provided
  (when initial-text
    (insert initial-text))
  
  ;; Position cursor appropriately
  (goto-char (if initial-text (point-max) (point-min))))

(defun forj-prompt-submit ()
  "Submit the prompt buffer content with collected context."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (string-trim content))
         (context-data (forj-collect-selected-context)))
    
    (when (string-empty-p prompt)
      (user-error "Please enter a request"))
    
    ;; Close prompt buffer
    (quit-window t)
    
    ;; Process request with context
    (if forj-prompt-callback-function
        (funcall forj-prompt-callback-function prompt context-data)
      (forj-process-prompt-with-context prompt context-data))
    
    ;; Return to origin buffer if specified
    (when (and forj-prompt-return-buffer 
               (buffer-live-p forj-prompt-return-buffer))
      (switch-to-buffer forj-prompt-return-buffer))))

(defun forj-prompt-cancel ()
  "Cancel and close the prompt buffer."
  (interactive)
  (quit-window t)
  (message "Forj prompt cancelled")
  
  ;; Return to origin buffer if specified
  (when (and forj-prompt-return-buffer 
             (buffer-live-p forj-prompt-return-buffer))
    (switch-to-buffer forj-prompt-return-buffer)))

;;; File and Command Insertion

(defun forj-insert-file ()
  "Insert file path into prompt after @ symbol."
  (interactive)
  (let ((file-path (forj-select-file-for-context)))
    (when file-path
      ;; Insert the file path
      (insert file-path)
      
      ;; Add to context sources
      (let ((context-source `(:type file 
                              :path ,file-path 
                              :confidence 1.0
                              :reason "Manually selected via @ command")))
        (add-to-list 'forj-prompt-context-sources context-source))
      
      (message "Added file to context: %s" (file-name-nondirectory file-path)))))

(defun forj-insert-command ()
  "Insert command into prompt after / symbol."
  (interactive)
  (let ((command (forj-select-command)))
    (when command
      (insert command)
      (message "Inserted command: %s" command))))

(defun forj-select-file-for-context ()
  "Select file for context inclusion with enhanced browsing."
  (let* ((project-root (forj-find-project-root))
         (initial-dir (or project-root default-directory))
         (file-path (read-file-name "Select file for context: " 
                                   initial-dir nil t)))
    (when (and file-path (file-exists-p file-path))
      (expand-file-name file-path))))

(defun forj-select-command ()
  "Select command to insert into prompt."
  (let ((commands '("explain" "fix" "optimize" "refactor" "test" "document" 
                   "review" "analyze" "debug" "implement" "create" "update")))
    (completing-read "Select command: " commands nil nil)))

;;; Context Management Functions

(defun forj-show-context-suggestions ()
  "Show context suggestions in a temporary buffer."
  (interactive)
  (let ((suggestions (forj-suggest-context-sources 
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (if suggestions
        (with-output-to-temp-buffer "*Forj Context Suggestions*"
          (princ "Context Suggestions:\n")
          (princ "==================\n\n")
          (dolist (suggestion suggestions)
            (princ (format "Type: %s\n" (plist-get suggestion :type)))
            (when (plist-get suggestion :path)
              (princ (format "Path: %s\n" (plist-get suggestion :path))))
            (when (plist-get suggestion :buffer)
              (princ (format "Buffer: %s\n" (buffer-name (plist-get suggestion :buffer)))))
            (princ (format "Confidence: %.2f\n" (plist-get suggestion :confidence)))
            (princ (format "Reason: %s\n\n" (plist-get suggestion :reason)))))
      (message "No context suggestions available"))))

(defun forj-clear-context-sources ()
  "Clear all selected context sources."
  (interactive)
  (setq forj-prompt-context-sources '())
  (message "Context sources cleared"))

(defun forj-list-context-sources ()
  "List currently selected context sources."
  (interactive)
  (if forj-prompt-context-sources
      (with-output-to-temp-buffer "*Forj Context Sources*"
        (princ "Selected Context Sources:\n")
        (princ "========================\n\n")
        (dolist (source forj-prompt-context-sources)
          (princ (format "Type: %s\n" (plist-get source :type)))
          (when (plist-get source :path)
            (princ (format "Path: %s\n" (plist-get source :path))))
          (when (plist-get source :buffer)
            (princ (format "Buffer: %s\n" (buffer-name (plist-get source :buffer)))))
          (princ (format "Confidence: %.2f\n" (plist-get source :confidence)))
          (princ (format "Reason: %s\n\n" (plist-get source :reason)))))
    (message "No context sources selected")))

(defun forj-collect-selected-context ()
  "Collect context from currently selected sources."
  (let ((context-data '()))
    (dolist (source forj-prompt-context-sources)
      (let ((context-item (condition-case err
                              (if forj-context-available
                                  (pcase (plist-get source :type)
                                    ('buffer (forj-collect-buffer-context
                                             (plist-get source :buffer)
                                             (plist-get source :region)))
                                    ('file (forj-collect-file-context
                                           (plist-get source :path)))
                                    ('compilation (forj-collect-compilation-context
                                                  (plist-get source :include-warnings)))
                                    ('project (forj-collect-project-context
                                              (plist-get source :root)
                                              (plist-get source :patterns))))
                                ;; Fallback for basic file reading
                                (when (eq (plist-get source :type) 'file)
                                  (let ((path (plist-get source :path)))
                                    (when (file-exists-p path)
                                      `(:type file
                                        :path ,path
                                        :content ,(with-temp-buffer
                                                   (insert-file-contents path nil 0 50000)
                                                   (buffer-string))
                                        :relevance 0.8)))))
                            (error 
                             (message "Warning: Failed to collect context from %s: %s" 
                                    (plist-get source :type)
                                    (error-message-string err))
                             nil))))
        (when context-item
          (push context-item context-data))))
    
    ;; Optimize context size (if available, otherwise just return the data)
    (if (fboundp 'forj-optimize-context-size)
        (forj-optimize-context-size (nreverse context-data))
      (nreverse context-data))))

;;; Application Entry Points

(defun forj-start ()
  "Launch Forj application directly to conversation buffer."
  (interactive)
  (let ((buffer (forj-conversation-buffer)))
    (message "Forj AI Assistant ready. Press 'p' to open prompt interface.")
    buffer))



(defun forj-show-help ()
  "Show Forj help information."
  (interactive)
  (with-output-to-temp-buffer "*Forj Help*"
    (princ "Forj.el Help\n")
    (princ "============\n\n")
    (princ "Entry Points:\n")
    (princ "  M-x forj-start  - Launch conversation buffer\n")
    (princ "  M-x forj-prompt - Open prompt interface directly\n\n")
    (princ "Conversation Buffer:\n")
    (princ "  p               - Open prompt interface\n")
    (princ "  • View AI responses and conversation history\n\n")
    (princ "Prompt Interface (Minimal):\n")
    (princ "  C-c C-c         - Submit prompt\n")
    (princ "  C-c C-k         - Cancel prompt\n\n")))

;;; Integration Functions

(defun forj-process-prompt-with-context (prompt context-data)
  "Process PROMPT with CONTEXT-DATA using existing API integration.
This function serves as the bridge between the new context system
and the existing forj-api functionality."
  ;; Use existing forj-prompt function if available, otherwise show error
  (if (fboundp 'forj-prompt)
      (let ((formatted-context (forj-format-context-for-api context-data)))
        ;; Call existing API with enhanced context
        (forj-api-request-with-context prompt context-data))
    (message "API integration not available. Prompt: %s, Context items: %d" 
             prompt (length context-data))))

(defun forj-format-context-for-api (context-list)
  "Format CONTEXT-LIST for API consumption."
  (mapconcat
   (lambda (ctx)
     (let ((type (plist-get ctx :type))
           (content (plist-get ctx :content))
           (metadata (plist-get ctx :metadata)))
       (format "=== %s Context ===\n%s\n"
               (capitalize (symbol-name type))
               (if (> (length content) 2000)
                   (concat (substring content 0 2000) "\n[... truncated ...]")
                 content))))
   context-list
   "\n"))

;;; Enhanced API Integration Function

(defun forj-api-request-with-context (prompt context-data)
  "Send API request with structured context included.
This enhances the existing API system with context management."
  (if (fboundp 'forj-api-request)
      (let* ((context-summary (forj-format-context-for-api context-data))
             (enhanced-prompt (if (and context-data (not (string-empty-p context-summary)))
                                 (format "Context:\n%s\n\nUser Request:\n%s"
                                        context-summary prompt)
                               prompt))
             (response (forj-api-request enhanced-prompt)))
        ;; Display the user input and AI response
        (when (fboundp 'forj-display-user-input)
          (forj-display-user-input prompt))
        (when (and response (fboundp 'forj-display-response))
          (forj-display-response response)
          ;; Show the conversation buffer to user
          (display-buffer (forj-conversation-buffer)))
        response)
    (error "API integration not available. Please ensure forj-api.el is loaded.")))

;;; Mode Hook for Keybinding Setup

(defun forj-prompt-mode-setup ()
  "Set up keybindings and evil integration for forj-prompt-mode."
  ;; Ensure our keybindings are active
  (use-local-map forj-prompt-mode-map)
  
  ;; Additional evil integration if needed
  (when (and (boundp 'evil-mode) evil-mode)
    ;; Force insert state for editing
    (evil-insert-state)
    ;; Ensure evil doesn't override our C-c bindings
    (setq-local evil-intercept-maps nil)))

;; Add the setup function to the mode hook
(add-hook 'forj-prompt-mode-hook #'forj-prompt-mode-setup)

(provide 'forj-prompt-interface)
;;; forj-prompt-interface.el ends here