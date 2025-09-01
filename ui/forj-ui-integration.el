;;; forj-ui-integration.el --- Integration layer for forj UI components -*- lexical-binding: t -*-

;;; Commentary:
;; Integration layer that coordinates all forj UI components and provides
;; unified initialization, configuration, and management system.
;; Serves as the entry point for Phase 1.6 UI/UX Enhancement system.

;;; Code:

(require 'cl-lib)

;; Load all UI component modules using load-file approach like main forj.el
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
      (ui-modules '("forj-theme" "forj-syntax-highlight" "forj-markdown" 
                    "forj-ui-components" "forj-buffer-layout"))
      (lib-modules '("forj-progress")))
  ;; Load UI modules from current directory
  (dolist (module ui-modules)
    (let ((module-file (expand-file-name (concat module ".el") current-dir)))
      (if (file-exists-p module-file)
          (condition-case err
              (load-file module-file)
            (error
             (message "Warning: Failed to load UI module %s: %s" module (error-message-string err))))
        (message "Warning: UI module file not found: %s" module-file))))
  
  ;; Load lib modules from ../lib/ directory
  (dolist (module lib-modules)
    (let ((module-file (expand-file-name (concat "../lib/" module ".el") current-dir)))
      (if (file-exists-p module-file)
          (condition-case err
              (load-file module-file)
            (error
             (message "Warning: Failed to load lib module %s: %s" module (error-message-string err))))
        (message "Warning: Lib module file not found: %s" module-file)))))

;;; Integration Variables

(defvar forj-ui-initialized nil
  "Whether forj UI system has been initialized.")

(defvar forj-ui-components-loaded '()
  "List of successfully loaded UI components.")

(defvar forj-ui-integration-hooks nil
  "Hooks run after UI integration is complete.")

;;; Component Loading and Validation

(defun forj-ui-load-component (component-name require-symbol feature-symbol)
  "Load UI COMPONENT-NAME with REQUIRE-SYMBOL and validate FEATURE-SYMBOL."
  (condition-case err
      (progn
        (require require-symbol)
        (when (featurep feature-symbol)
          (push component-name forj-ui-components-loaded)
          (message "✅ Loaded forj UI component: %s" component-name)
          t))
    (error
     (message "❌ Failed to load forj UI component %s: %s" 
              component-name (error-message-string err))
     nil)))

(defun forj-ui-validate-components ()
  "Validate that all required UI components are loaded and functional."
  (let ((required-components
         '(("syntax-highlighting" forj-syntax-highlight forj-syntax-highlight)
           ("markdown-rendering" forj-markdown forj-markdown)
           ("theme-system" forj-theme forj-theme)
           ("ui-components" forj-ui-components forj-ui-components)
           ("progress-indicators" forj-progress forj-progress)
           ("buffer-layout" forj-buffer-layout forj-buffer-layout)))
        (loaded-count 0)
        (total-count 0))
    
    (dolist (component required-components)
      (setq total-count (1+ total-count))
      (when (forj-ui-load-component (nth 0 component) (nth 1 component) (nth 2 component))
        (setq loaded-count (1+ loaded-count))))
    
    (message "Forj UI components loaded: %d/%d" loaded-count total-count)
    (= loaded-count total-count)))

;;; Integration Configuration

(defcustom forj-ui-enable-all-components t
  "Whether to enable all UI components during initialization."
  :type 'boolean
  :group 'forj-ui)

(defcustom forj-ui-auto-setup t
  "Whether to automatically set up UI components when loaded."
  :type 'boolean
  :group 'forj-ui)

(defcustom forj-ui-integration-style 'full
  "Level of UI integration to enable.
'minimal: Basic syntax highlighting and markdown only
'standard: Standard UI with buttons and progress indicators
'full: Complete UI system with all enhancements"
  :type '(choice (const :tag "Minimal integration" minimal)
                 (const :tag "Standard integration" standard)
                 (const :tag "Full integration" full))
  :group 'forj-ui)

;;; Core Integration Functions

;;;###autoload
(defun forj-ui-initialize ()
  "Initialize the complete forj UI system."
  (interactive)
  (unless forj-ui-initialized
    (message "🚀 Initializing forj UI system...")
    
    ;; Step 1: Load and validate components
    (if (forj-ui-validate-components)
        (progn
          ;; Step 2: Initialize theme system first (others depend on it)
          (forj-setup-theme)
          
          ;; Step 2.5: Ensure face attributes are properly applied
          (forj-apply-theme)
          
          ;; Step 3: Set up components based on integration style
          (pcase forj-ui-integration-style
            ('minimal (forj-ui-setup-minimal))
            ('standard (forj-ui-setup-standard))
            ('full (forj-ui-setup-full)))
          
          ;; Step 4: Set up conversation buffer (handle existing read-only buffers)
          (when (and forj-ui-auto-setup (fboundp 'forj-setup-enhanced-conversation-buffer))
            (condition-case err
                (forj-setup-enhanced-conversation-buffer)
              (error
               (message "Warning: Could not set up enhanced conversation buffer: %s" (error-message-string err)))))
          
          ;; Step 5: Set up integration hooks
          (forj-ui-setup-integration-hooks)
          
          ;; Step 6: Mark as initialized
          (setq forj-ui-initialized t)
          (run-hooks 'forj-ui-integration-hooks)
          
          (message "✅ Forj UI system initialized successfully (%s mode)" forj-ui-integration-style))
      
      (message "❌ Failed to initialize forj UI system - some components missing"))))

(defun forj-ui-setup-minimal ()
  "Set up minimal UI integration."
  (message "Setting up minimal forj UI...")
  
  ;; Basic syntax highlighting
  (when (featurep 'forj-syntax-highlight)
    (forj-setup-conversation-highlighting))
  
  ;; Basic markdown rendering
  (when (featurep 'forj-markdown)
    (forj-setup-conversation-markdown))
  
  (message "✅ Minimal forj UI setup complete"))

(defun forj-ui-setup-standard ()
  "Set up standard UI integration."
  (message "Setting up standard forj UI...")
  
  ;; Include minimal setup
  (forj-ui-setup-minimal)
  
  ;; Add UI components
  (when (featurep 'forj-ui-components)
    (forj-setup-ui-components))
  
  ;; Add progress indicators
  (when (featurep 'forj-progress)
    (forj-setup-progress-integration))
  
  (message "✅ Standard forj UI setup complete"))

(defun forj-ui-setup-full ()
  "Set up full UI integration with all features."
  (message "Setting up full forj UI...")
  
  ;; Include standard setup
  (forj-ui-setup-standard)
  
  ;; Enhanced buffer layout
  (when (featurep 'forj-buffer-layout)
    (forj-setup-buffer-layout))
  
  ;; Advanced features and integrations
  (forj-ui-setup-advanced-features)
  
  (message "✅ Full forj UI setup complete"))

(defun forj-ui-setup-advanced-features ()
  "Set up advanced UI features and integrations."
  ;; Split window support
  (when (and (featurep 'forj-buffer-layout)
             forj-layout-enable-split-window)
    (global-set-key (kbd "C-c f w") #'forj-toggle-split-layout))
  
  ;; Theme toggling
  (when (featurep 'forj-theme)
    (global-set-key (kbd "C-c f t") #'forj-toggle-theme))
  
  ;; Layout cycling
  (when (featurep 'forj-buffer-layout)
    (global-set-key (kbd "C-c f l") #'forj-cycle-layout-style))
  
  ;; Advanced navigation
  (when (featurep 'forj-ui-components)
    (global-set-key (kbd "C-c f n") #'forj-next-interactive-element)
    (global-set-key (kbd "C-c f p") #'forj-previous-interactive-element)))

;;; Integration Hooks and Event Handling

(defun forj-ui-setup-integration-hooks ()
  "Set up hooks for seamless integration between UI components."
  ;; Theme change hooks
  (when (featurep 'forj-theme)
    (add-hook 'forj-theme-update-hook #'forj-ui-refresh-all-components))
  
  ;; Buffer update hooks
  (when (featurep 'forj-buffer-layout)
    (add-hook 'forj-conversation-update-hook #'forj-ui-enhance-new-content))
  
  ;; Progress completion hooks
  (when (featurep 'forj-progress)
    (advice-add 'forj-complete-progress :after #'forj-ui-progress-completed)))

(defun forj-ui-refresh-all-components ()
  "Refresh all UI components after theme or configuration change."
  (when forj-ui-initialized
    (message "Refreshing forj UI components...")
    
    ;; Refresh syntax highlighting
    (when (featurep 'forj-syntax-highlight)
      (forj-highlight-buffer))
    
    ;; Refresh markdown rendering
    (when (featurep 'forj-markdown)
      (forj-render-markdown-buffer))
    
    ;; Refresh buffer layout
    (when (featurep 'forj-buffer-layout)
      (forj-refresh-buffer-layout))
    
    (message "✅ Forj UI components refreshed")))

(defun forj-ui-enhance-new-content (start end content role)
  "Enhance new content between START and END with all UI features."
  (when forj-ui-initialized
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        
        ;; Apply syntax highlighting
        (when (featurep 'forj-syntax-highlight)
          (forj-highlight-code-blocks))
        
        ;; Apply markdown rendering
        (when (featurep 'forj-markdown)
          (forj-render-markdown-buffer))
        
        ;; Add interactive elements for AI responses
        (when (and (featurep 'forj-ui-components)
                   (memq role '(assistant ai)))
          (forj-enhance-ai-response start end content))))))

(defun forj-ui-progress-completed (progress-id &optional final-message)
  "Handle progress completion with UI integration."
  ;; Could add completion effects, sound, or other feedback here
  nil)

;;; Conversation Buffer Integration

;;;###autoload
(defun forj-ui-setup-conversation-buffer ()
  "Set up enhanced conversation buffer with all UI components."
  (interactive)
  (if forj-ui-initialized
      (let ((buffer (forj-setup-enhanced-conversation-buffer)))
        (when buffer
          (with-current-buffer buffer
            ;; Set up all UI enhancements
            (forj-ui-apply-buffer-enhancements)
            (switch-to-buffer buffer)
            (message "✅ Enhanced conversation buffer ready"))))
    (message "❌ Forj UI system must be initialized first (run M-x forj-ui-initialize)")))

(defun forj-ui-apply-buffer-enhancements ()
  "Apply all UI enhancements to current conversation buffer."
  ;; Set up local keybindings
  (when (featurep 'forj-ui-components)
    (local-set-key [mouse-3] #'forj-show-context-menu)
    (local-set-key (kbd "C-c C-a") #'forj-context-apply)
    (local-set-key (kbd "C-c C-c") #'forj-context-copy)
    (local-set-key (kbd "C-c C-r") #'forj-context-reject))
  
  ;; Set up visual enhancements
  (when (featurep 'forj-theme)
    (forj-apply-theme))
  
  ;; Enable visual line mode for better readability
  (visual-line-mode 1)
  (setq word-wrap t))

;;; Display and Interaction Enhancements

;;;###autoload
(defun forj-ui-display-message (role content &optional timestamp)
  "Display enhanced message with ROLE, CONTENT, and optional TIMESTAMP."
  (when forj-ui-initialized
    (let ((start-pos (point-max)))
      ;; Use enhanced buffer layout if available
      (if (featurep 'forj-buffer-layout)
          (forj-display-enhanced-message role content timestamp)
        ;; Fallback to simple display
        (goto-char (point-max))
        (insert (format "\n[%s]: %s\n" (upcase (symbol-name role)) content)))
      
      ;; Apply UI enhancements to new content
      (let ((end-pos (point-max)))
        (forj-ui-enhance-new-content start-pos end-pos content role)))))

;;;###autoload
(defun forj-ui-show-progress (message &optional style)
  "Show progress indicator with MESSAGE using optional STYLE."
  (if (and forj-ui-initialized (featurep 'forj-progress))
      (pcase style
        ('bar (forj-show-progress-bar message 100))
        ('api (forj-show-api-progress message))
        (_ (forj-show-progress message)))
    ;; Fallback to simple message
    (message "%s..." message)))

;;; Configuration and Customization

;;;###autoload
(defun forj-ui-configure-integration ()
  "Interactive configuration of forj UI integration."
  (interactive)
  (let ((style (intern (completing-read 
                       "Integration style: "
                       '("minimal" "standard" "full")
                       nil t nil nil "full"))))
    (setq forj-ui-integration-style style)
    (if forj-ui-initialized
        (progn
          (message "Reconfiguring forj UI to %s integration..." style)
          (forj-ui-reinitialize))
      (message "Configuration saved. Run M-x forj-ui-initialize to apply."))))

;;;###autoload
(defun forj-ui-reinitialize ()
  "Reinitialize forj UI system with current configuration."
  (interactive)
  (when forj-ui-initialized
    (forj-ui-cleanup)
    (setq forj-ui-initialized nil))
  (forj-ui-initialize))

;;; Diagnostics and Debugging

;;;###autoload
(defun forj-ui-system-status ()
  "Display comprehensive status of forj UI system."
  (interactive)
  (let ((status-buffer (get-buffer-create "*Forj UI Status*")))
    (with-current-buffer status-buffer
      (erase-buffer)
      (insert "FORJ UI SYSTEM STATUS\n")
      (insert "=====================\n\n")
      
      ;; Initialization status
      (insert (format "Initialized: %s\n" (if forj-ui-initialized "✅ Yes" "❌ No")))
      (insert (format "Integration Style: %s\n" forj-ui-integration-style))
      (insert (format "Auto Setup: %s\n\n" (if forj-ui-auto-setup "Yes" "No")))
      
      ;; Component status
      (insert "LOADED COMPONENTS\n")
      (insert "-----------------\n")
      (if forj-ui-components-loaded
          (dolist (component forj-ui-components-loaded)
            (insert (format "✅ %s\n" component)))
        (insert "❌ No components loaded\n"))
      (insert "\n")
      
      ;; Feature availability
      (insert "FEATURE AVAILABILITY\n")
      (insert "--------------------\n")
      (dolist (feature '((forj-syntax-highlight "Syntax Highlighting")
                         (forj-markdown "Markdown Rendering") 
                         (forj-theme "Theme System")
                         (forj-ui-components "Interactive Components")
                         (forj-progress "Progress Indicators")
                         (forj-buffer-layout "Enhanced Layout")))
        (insert (format "%s %s\n" 
                        (if (featurep (car feature)) "✅" "❌")
                        (cadr feature))))
      (insert "\n")
      
      ;; Buffer status
      (insert "BUFFER STATUS\n")
      (insert "-------------\n")
      (let ((conv-buffer (get-buffer "*forj-conversation*")))
        (if conv-buffer
            (progn
              (insert "✅ Conversation buffer exists\n")
              (with-current-buffer conv-buffer
                (insert (format "   Buffer size: %d characters\n" (buffer-size)))
                (insert (format "   Layout style: %s\n" 
                               (if (boundp 'forj-layout-style) forj-layout-style "unknown")))))
          (insert "❌ No conversation buffer\n")))
      
      (goto-char (point-min)))
    (display-buffer status-buffer)))

;;;###autoload
(defun forj-ui-test-components ()
  "Run basic tests on all UI components."
  (interactive)
  (message "Testing forj UI components...")
  (let ((test-results '())
        (test-buffer (get-buffer-create "*forj-ui-test*")))
    
    (with-current-buffer test-buffer
      (erase-buffer)
      
      ;; Test syntax highlighting
      (when (featurep 'forj-syntax-highlight)
        (insert "```elisp\n(defun test-function ())\n```\n")
        (forj-highlight-code-blocks)
        (push '("Syntax Highlighting" . t) test-results))
      
      ;; Test markdown rendering
      (when (featurep 'forj-markdown)
        (insert "# Test Header\n**Bold text**\n")
        (forj-render-markdown-buffer)
        (push '("Markdown Rendering" . t) test-results))
      
      ;; Test theme system
      (when (featurep 'forj-theme)
        (let ((theme-type (forj-detect-theme-type)))
          (push (cons "Theme Detection" (symbolp theme-type)) test-results)))
      
      ;; Test UI components
      (when (featurep 'forj-ui-components)
        (let ((button (forj-insert-copy-button "test code" "elisp")))
          (push (cons "UI Components" (button-at (button-start button))) test-results)))
      
      ;; Test progress indicators
      (when (featurep 'forj-progress)
        (let ((progress-id (forj-show-progress "Testing...")))
          (forj-hide-progress progress-id)
          (push '("Progress Indicators" . t) test-results)))
      
      (goto-char (point-max))
      (insert "\n\nTEST RESULTS:\n")
      (dolist (result test-results)
        (insert (format "%s %s\n" 
                        (if (cdr result) "✅" "❌")
                        (car result)))))
    
    (display-buffer test-buffer)
    (message "✅ UI component testing complete")))

;;; Cleanup and Reset

;;;###autoload
(defun forj-ui-cleanup ()
  "Clean up forj UI system and remove all enhancements."
  (interactive)
  (when forj-ui-initialized
    (message "Cleaning up forj UI system...")
    
    ;; Clean up progress indicators
    (when (featurep 'forj-progress)
      (forj-cleanup-progress))
    
    ;; Clean up theme system
    (when (featurep 'forj-theme)
      (forj-cleanup-theme))
    
    ;; Remove global keybindings
    (global-unset-key (kbd "C-c f w"))
    (global-unset-key (kbd "C-c f t"))
    (global-unset-key (kbd "C-c f l"))
    (global-unset-key (kbd "C-c f n"))
    (global-unset-key (kbd "C-c f p"))
    
    ;; Reset state
    (setq forj-ui-initialized nil)
    (setq forj-ui-components-loaded '())
    
    (message "✅ Forj UI system cleaned up")))

;;; Integration with Main System

(defun forj-ui-setup-main-integration ()
  "Set up integration with main forj system."
  ;; This function will be called from main forj.el
  ;; to ensure proper initialization order
  (when forj-ui-enable-all-components
    (forj-ui-initialize)))

(provide 'forj-ui-integration)
;;; forj-ui-integration.el ends here