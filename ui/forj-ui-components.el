;;; forj-ui-components.el --- Interactive UI elements for forj conversation buffer -*- lexical-binding: t -*-

;;; Commentary:
;; Interactive UI components for forj conversation buffer.
;; Provides clickable buttons, collapsible sections, tooltips, context menus,
;; and enhanced user interaction elements for AI responses.
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'easymenu)

;; Load error handling system if available
(when (locate-library "forj-error-system")
  (require 'forj-error-system))

;; Forward declarations
(declare-function forj-apply-response "forj-api" (response))
(declare-function forj-display-response "forj-api" (response))
(declare-function forj-format-human-error "forj-error-system" (error-context))
(declare-function forj-format-machine-error "forj-error-system" (error-context))

;;; Customization Group

(defgroup forj-ui nil
  "Interactive UI components for forj conversation buffer."
  :group 'forj
  :prefix "forj-ui-")

;;; Button Configuration

(defcustom forj-ui-button-style 'modern
  "Style for forj UI buttons.
'classic: Traditional Emacs button style
'modern: Modern flat button style with hover effects"
  :type '(choice (const :tag "Classic Emacs style" classic)
                 (const :tag "Modern flat style" modern))
  :group 'forj-ui)

(defcustom forj-ui-enable-tooltips t
  "Whether to show tooltips for interactive elements."
  :type 'boolean
  :group 'forj-ui)

(defcustom forj-ui-enable-context-menu t
  "Whether to enable context menu on right-click."
  :type 'boolean
  :group 'forj-ui)

;;; Button Definitions

;; Apply Suggestion Button
(define-button-type 'forj-apply-button
  'action #'forj-apply-button-action
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face
  'follow-link t
  'help-echo "Click to apply this AI suggestion to your code")

;; Copy Code Button  
(define-button-type 'forj-copy-button
  'action #'forj-copy-button-action
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face
  'follow-link t
  'help-echo "Click to copy this code block to clipboard")

;; Reject Suggestion Button
(define-button-type 'forj-reject-button
  'action #'forj-reject-button-action
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face
  'follow-link t
  'help-echo "Click to reject this AI suggestion")

;; Expand/Collapse Button
(define-button-type 'forj-toggle-button
  'action #'forj-toggle-button-action
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face
  'follow-link t
  'help-echo "Click to expand or collapse this section")

;; Generic Action Button
(define-button-type 'forj-action-button
  'action #'forj-action-button-action
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face
  'follow-link t)

;;; Button Actions

(defun forj-apply-button-action (button)
  "Action for apply suggestion BUTTON."
  (let ((suggestion (button-get button 'forj-suggestion))
        (suggestion-id (button-get button 'forj-suggestion-id)))
    (when suggestion
      (if (yes-or-no-p "Apply this AI suggestion? ")
          (progn
            (condition-case err
                (progn
                  (if (fboundp 'forj-apply-response)
                      (forj-apply-response suggestion)
                    (message "Apply function not available"))
                  (forj-update-button-state button 'applied)
                  (message "✅ Suggestion applied successfully"))
              (error
               (when (fboundp 'forj-user-error)
                 (forj-user-error (format "Failed to apply suggestion: %s" (error-message-string err))
                                 :context "UI button action"
                                 :function "forj-apply-button-action"
                                 :recovery '("Check suggestion format"
                                            "Try applying manually"
                                            "Report issue if persists")))
               (message "❌ Error applying suggestion: %s" (error-message-string err))
               (forj-update-button-state button 'error))))
        (message "Suggestion application cancelled")))))

(defun forj-copy-button-action (button)
  "Action for copy code BUTTON."
  (let ((code-content (button-get button 'forj-code-content))
        (code-type (button-get button 'forj-code-type)))
    (if code-content
        (progn
          (kill-new code-content)
          (forj-update-button-state button 'copied)
          (message "✅ Code copied to clipboard (%s)"
                   (if code-type
                       (format "%d chars, %s" (length code-content) code-type)
                     (format "%d chars" (length code-content)))))
      (message "❌ No code content found to copy"))))

(defun forj-reject-button-action (button)
  "Action for reject suggestion BUTTON."
  (let ((suggestion-id (button-get button 'forj-suggestion-id)))
    (when (yes-or-no-p "Reject this AI suggestion? ")
      (forj-update-button-state button 'rejected)
      (message "🚫 Suggestion rejected"))))

(defun forj-toggle-button-action (button)
  "Action for expand/collapse toggle BUTTON."
  (let ((overlay (button-get button 'forj-content-overlay))
        (expanded (button-get button 'forj-expanded)))
    (if overlay
        (if expanded
            (forj-collapse-content button overlay)
          (forj-expand-content button overlay))
      (message "❌ No collapsible content found"))))

(defun forj-action-button-action (button)
  "Generic action for custom action BUTTON."
  (let ((action-function (button-get button 'forj-action))
        (action-data (button-get button 'forj-action-data)))
    (if action-function
        (condition-case err
            (funcall action-function action-data)
          (error
           (when (fboundp 'forj-system-error)
             (forj-system-error (format "UI action execution failed: %s" (error-message-string err))
                               :context "Interactive button execution"
                               :function "forj-interactive-button-action"
                               :details `(:action ,action :button ,button)
                               :recovery '("Check system resources"
                                          "Retry the action"
                                          "Check button configuration")))
           (message "❌ Error executing action: %s" (error-message-string err))))
      (message "No action defined for this button"))))

;;; Button State Management

(defun forj-update-button-state (button new-state)
  "Update BUTTON visual state to NEW-STATE."
  (let ((button-text (button-get button 'forj-original-text)))
    (pcase new-state
      ('applied
       (button-put button 'face 'forj-success-face)
       (button-put button 'help-echo "✅ Applied successfully"))
      ('copied  
       (button-put button 'face 'forj-success-face)
       (button-put button 'help-echo "✅ Copied to clipboard")
       ;; Reset after a delay
       (run-with-timer 2.0 nil
                      (lambda () 
                        (when (button-at (button-start button))
                          (button-put button 'face 'forj-button-face)
                          (button-put button 'help-echo "Click to copy this code block to clipboard")))))
      ('rejected
       (button-put button 'face 'forj-error-face)
       (button-put button 'help-echo "🚫 Rejected"))
      ('error
       (button-put button 'face 'forj-error-face)
       (button-put button 'help-echo "❌ Error occurred"))
      ('loading
       (button-put button 'face 'forj-info-face)
       (button-put button 'help-echo "⏳ Processing...")))))

;;; Button Creation Functions

;;;###autoload
(defun forj-insert-apply-button (suggestion &optional suggestion-id)
  "Insert an apply suggestion button with SUGGESTION content and optional SUGGESTION-ID."
  (let ((button (insert-button "Apply" :type 'forj-apply-button)))
    (button-put button 'forj-suggestion suggestion)
    (button-put button 'forj-suggestion-id (or suggestion-id (format "suggestion-%d" (random 10000))))
    (button-put button 'forj-original-text "Apply")
    button))

;;;###autoload  
(defun forj-insert-copy-button (code-content &optional code-type)
  "Insert a copy code button with CODE-CONTENT and optional CODE-TYPE."
  (let ((button (insert-button "Copy" :type 'forj-copy-button)))
    (button-put button 'forj-code-content code-content)
    (button-put button 'forj-code-type code-type)
    (button-put button 'forj-original-text "Copy")
    button))

;;;###autoload
(defun forj-insert-reject-button (&optional suggestion-id)
  "Insert a reject suggestion button with optional SUGGESTION-ID."
  (let ((button (insert-button "Reject" :type 'forj-reject-button)))
    (button-put button 'forj-suggestion-id (or suggestion-id (format "suggestion-%d" (random 10000))))
    (button-put button 'forj-original-text "Reject")
    button))

;;;###autoload
(defun forj-insert-toggle-button (label content-overlay expanded)
  "Insert a toggle button with LABEL, CONTENT-OVERLAY, and EXPANDED state."
  (let ((button (insert-button label :type 'forj-toggle-button)))
    (button-put button 'forj-content-overlay content-overlay)
    (button-put button 'forj-expanded expanded)
    (button-put button 'forj-original-text label)
    button))

;;;###autoload
(defun forj-insert-action-button (label action-function &optional action-data help-text)
  "Insert a custom action button with LABEL, ACTION-FUNCTION, ACTION-DATA, and HELP-TEXT."
  (let ((button (insert-button label :type 'forj-action-button)))
    (button-put button 'forj-action action-function)
    (button-put button 'forj-action-data action-data)
    (button-put button 'forj-original-text label)
    (when help-text
      (button-put button 'help-echo help-text))
    button))

;;; Interactive Button Groups

;;;###autoload
(defun forj-add-interactive-buttons (start end &optional content-type)
  "Add interactive buttons to region between START and END.
CONTENT-TYPE can be 'code, 'suggestion, or 'general to determine button types."
  (save-excursion
    (goto-char end)
    (let ((content (buffer-substring-no-properties start end)))
      (insert "\n")
      (insert (propertize "─────────────────────────────────────" 'face 'forj-separator-face))
      (insert "\n[")
      
      ;; Add appropriate buttons based on content type
      (pcase content-type
        ('code
         (forj-insert-copy-button content "code")
         (insert "] [")
         (forj-insert-action-button "Format" #'forj-format-code-action content "Format this code block"))
        ('suggestion
         (forj-insert-apply-button content)
         (insert "] [")
         (forj-insert-copy-button content "suggestion")
         (insert "] [") 
         (forj-insert-reject-button))
        ('general
         (forj-insert-copy-button content "text")
         (insert "] [")
         (forj-insert-action-button "Quote" #'forj-quote-text-action content "Quote this text")))
      
      (insert "]\n"))))

;;;###autoload
(defun forj-add-code-block-buttons (start end code-content &optional language)
  "Add buttons specifically for code blocks between START and END.
CODE-CONTENT is the code text, LANGUAGE is optional language type."
  (save-excursion
    (goto-char end)
    (insert "\n")
    (let ((button-line-start (point)))
      (insert "┌─ ")
      (forj-insert-copy-button code-content language)
      (insert " │ ")
      (forj-insert-action-button "Execute" #'forj-execute-code-action 
                                (list :code code-content :language language)
                                "Execute this code block")
      (insert " │ ")
      (forj-insert-action-button "Save" #'forj-save-code-action 
                               (list :code code-content :language language)
                               "Save this code to file")
      (insert " ─┐")
      
      ;; Style the button line
      (add-text-properties button-line-start (point)
                         '(face forj-separator-face)))))

;;; Collapsible Sections

;;;###autoload
(defun forj-make-collapsible (start end title &optional initially-collapsed)
  "Make region between START and END collapsible with TITLE.
If INITIALLY-COLLAPSED is t, start in collapsed state."
  (let* ((content-start (save-excursion (goto-char start) (line-beginning-position)))
         (content-end (save-excursion (goto-char end) (line-end-position)))
         (overlay (make-overlay content-start content-end))
         (expanded (not initially-collapsed))
         (toggle-symbol (if expanded "▼" "▶")))
    
    ;; Set up overlay properties
    (overlay-put overlay 'forj-collapsible t)
    (overlay-put overlay 'forj-title title)
    (overlay-put overlay 'invisible (not expanded))
    (overlay-put overlay 'evaporate t)
    
    ;; Insert toggle button before the content
    (save-excursion
      (goto-char content-start)
      (beginning-of-line)
      (insert (propertize toggle-symbol 'face 'forj-button-face) " ")
      (let ((button (forj-insert-toggle-button title overlay expanded)))
        (insert "\n")
        (when initially-collapsed
          (forj-collapse-content button overlay))))
    
    overlay))

(defun forj-expand-content (button overlay)
  "Expand collapsible content for BUTTON with OVERLAY."
  (overlay-put overlay 'invisible nil)
  (button-put button 'forj-expanded t)
  (save-excursion
    (goto-char (button-start button))
    (when (looking-back "▶ " 2)
      (delete-char -2)
      (insert "▼ "))))

(defun forj-collapse-content (button overlay)
  "Collapse collapsible content for BUTTON with OVERLAY."
  (overlay-put overlay 'invisible t)
  (button-put button 'forj-expanded nil)
  (save-excursion
    (goto-char (button-start button))
    (when (looking-back "▼ " 2)
      (delete-char -2)
      (insert "▶ "))))

;;; Context Menu Support

(defvar forj-context-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [forj-apply] '(menu-item "Apply Suggestion" forj-context-apply))
    (define-key map [forj-copy] '(menu-item "Copy Text" forj-context-copy))
    (define-key map [forj-reject] '(menu-item "Reject Suggestion" forj-context-reject))
    (define-key map [separator1] '("--"))
    (define-key map [forj-format] '(menu-item "Format Code" forj-context-format))
    (define-key map [forj-save] '(menu-item "Save to File" forj-context-save))
    (define-key map [separator2] '("--"))
    (define-key map [forj-clear] '(menu-item "Clear Section" forj-context-clear))
    map)
  "Context menu for forj UI elements.")

;;;###autoload
(defun forj-show-context-menu (event)
  "Show context menu at EVENT position."
  (interactive "e")
  (when forj-ui-enable-context-menu
    (let ((click-pos (posn-point (event-start event))))
      (goto-char click-pos)
      (popup-menu forj-context-menu-map event))))

;; Context menu actions
(defun forj-context-apply ()
  "Apply suggestion from context menu."
  (interactive)
  (let ((button (button-at (point))))
    (if (and button (eq (button-type button) 'forj-apply-button))
        (forj-apply-button-action button)
      (message "No applicable suggestion at this location"))))

(defun forj-context-copy ()
  "Copy text from context menu."
  (interactive)
  (let ((button (button-at (point))))
    (if (and button (eq (button-type button) 'forj-copy-button))
        (forj-copy-button-action button)
      ;; Copy region or line if no copy button
      (if (use-region-p)
          (kill-new (buffer-substring (region-beginning) (region-end)))
        (kill-new (thing-at-point 'line)))
      (message "Text copied to clipboard"))))

(defun forj-context-reject ()
  "Reject suggestion from context menu."
  (interactive)
  (let ((button (button-at (point))))
    (if (and button (eq (button-type button) 'forj-reject-button))
        (forj-reject-button-action button)
      (message "No suggestion to reject at this location"))))

(defun forj-context-format ()
  "Format code from context menu."
  (interactive)
  (message "Code formatting not yet implemented"))

(defun forj-context-save ()
  "Save content to file from context menu."
  (interactive)
  (message "Save to file not yet implemented"))

(defun forj-context-clear ()
  "Clear section from context menu."
  (interactive)
  (when (yes-or-no-p "Clear this section? ")
    (message "Section clearing not yet implemented")))

;;; Custom Action Functions

(defun forj-format-code-action (code-content)
  "Format CODE-CONTENT action."
  (message "Code formatting: %d characters" (length code-content)))

(defun forj-quote-text-action (text-content)
  "Quote TEXT-CONTENT action."
  (kill-new (format "> %s" (string-replace "\n" "\n> " text-content)))
  (message "Text formatted as quote and copied to clipboard"))

(defun forj-execute-code-action (action-data)
  "Execute code from ACTION-DATA."
  (let ((code (plist-get action-data :code))
        (language (plist-get action-data :language)))
    (message "Execute %s code: %d characters" (or language "unknown") (length code))))

(defun forj-save-code-action (action-data)
  "Save code from ACTION-DATA to file."
  (let ((code (plist-get action-data :code))
        (language (plist-get action-data :language)))
    (let ((filename (read-file-name "Save code to file: " nil nil nil
                                   (format "code.%s" (or language "txt")))))
      (with-temp-file filename
        (insert code))
      (message "Code saved to %s" filename))))

;;; Tooltip and Help System

(defcustom forj-ui-tooltip-delay 0.5
  "Delay in seconds before showing tooltips."
  :type 'number
  :group 'forj-ui)

(defvar forj-tooltip-timer nil
  "Timer for delayed tooltip display.")

(defun forj-show-tooltip (text)
  "Show tooltip with TEXT."
  (when forj-ui-enable-tooltips
    (tooltip-show text)))

;;; Integration Functions

;;;###autoload
(defun forj-setup-ui-components ()
  "Set up UI components for forj conversation buffer."
  (when (and (boundp 'forj-conversation-buffer)
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      ;; Set up local key bindings
      (local-set-key [mouse-3] #'forj-show-context-menu)
      (local-set-key (kbd "C-c C-a") #'forj-context-apply)
      (local-set-key (kbd "C-c C-c") #'forj-context-copy)
      (local-set-key (kbd "C-c C-r") #'forj-context-reject))))

;;;###autoload
(defun forj-enhance-ai-response (response-start response-end response-content)
  "Enhance AI response between RESPONSE-START and RESPONSE-END with interactive elements.
RESPONSE-CONTENT is the text content of the response."
  (save-excursion
    ;; Detect if this is a code suggestion
    (if (string-match-p "(defun\\|defvar\\|defcustom\\|```" response-content)
        (forj-add-interactive-buttons response-start response-end 'suggestion)
      (forj-add-interactive-buttons response-start response-end 'general))
    
    ;; Add collapsible sections for long responses
    (when (> (length response-content) 1000)
      (forj-make-collapsible response-start response-end "AI Response" t))))

;;; Navigation and Utilities

;;;###autoload
(defun forj-next-interactive-element ()
  "Move to next interactive element (button) in buffer."
  (interactive)
  (let ((next-button (next-button (point))))
    (if next-button
        (goto-char (button-start next-button))
      (message "No more interactive elements"))))

;;;###autoload
(defun forj-previous-interactive-element ()
  "Move to previous interactive element (button) in buffer."
  (interactive)
  (let ((prev-button (previous-button (point))))
    (if prev-button
        (goto-char (button-start prev-button))
      (message "No previous interactive elements"))))

;;;###autoload  
(defun forj-list-interactive-elements ()
  "List all interactive elements in current buffer."
  (interactive)
  (let ((buttons '()))
    (save-excursion
      (goto-char (point-min))
      (while-let ((button (next-button (point))))
        (push (list (button-start button)
                   (button-label button)
                   (button-type button))
              buttons)
        (goto-char (1+ (button-end button)))))
    (if buttons
        (message "Interactive elements: %s" 
                (mapconcat (lambda (b) (format "%s(%s)" (nth 1 b) (nth 2 b)))
                          (reverse buttons) ", "))
      (message "No interactive elements found"))))

;;; Enhanced Error Display Integration

(defface forj-error-context-face
  '((t :foreground "#ff6b6b" :background "#2d1b1b" :weight bold))
  "Face for error context display."
  :group 'forj-ui)

(defface forj-error-recovery-face
  '((t :foreground "#ffd93d" :background "#2d2a1b" :style italic))
  "Face for error recovery suggestions."
  :group 'forj-ui)

(defun forj-display-error-with-context (error-context)
  "Display ERROR-CONTEXT with enhanced visual context and recovery options."
  (when (and error-context (fboundp 'forj-error-context-p) 
             (forj-error-context-p error-context))
    (let ((start-pos (point)))
      
      ;; Insert error header with icon
      (insert (propertize "🚨 " 'face 'forj-error-face))
      (insert (propertize (format "[%s] " 
                                 (upcase (symbol-name (forj-error-context-severity error-context))))
                         'face 'forj-error-face))
      (insert (propertize (forj-error-context-message error-context)
                         'face 'forj-error-face))
      (insert "\n")
      
      ;; Insert context information if available
      (when (forj-error-context-operation-context error-context)
        (insert (propertize "Context: " 'face 'forj-error-context-face))
        (insert (propertize (forj-error-context-operation-context error-context)
                           'face 'default))
        (insert "\n"))
      
      ;; Insert location information
      (let ((func (forj-error-context-function-name error-context))
            (file (forj-error-context-file-path error-context)))
        (when (or func file)
          (insert (propertize "Location: " 'face 'forj-error-context-face))
          (when func
            (insert (format "Function: %s" func)))
          (when (and func file)
            (insert " | "))
          (when file
            (insert (format "File: %s" (file-name-nondirectory file))))
          (insert "\n")))
      
      ;; Insert recovery suggestions with interactive buttons
      (when (forj-error-context-recovery-suggestions error-context)
        (insert (propertize "Recovery Options:\n" 'face 'forj-error-recovery-face))
        (forj-create-error-recovery-buttons error-context))
      
      ;; Add collapsible details section
      (when (forj-error-context-details error-context)
        (let ((details-start (point)))
          (insert (propertize "Details: " 'face 'forj-error-context-face))
          (insert (format "%S" (forj-error-context-details error-context)))
          (insert "\n")
          
          ;; Make details collapsible
          (forj-make-region-collapsible details-start (point) "Show Details" "Hide Details")))
      
      ;; Add separator
      (insert (propertize "─────────────────────────────\n" 'face 'forj-separator-face))
      
      ;; Return the region bounds for further processing
      (list start-pos (point)))))

(defun forj-create-error-recovery-buttons (error-context)
  "Create interactive recovery action buttons for ERROR-CONTEXT."
  (let ((suggestions (forj-error-context-recovery-suggestions error-context)))
    (when suggestions
      (let ((suggestions-list (if (listp suggestions) suggestions (list suggestions))))
        (dolist (suggestion suggestions-list)
          (let ((button-start (point)))
            (insert "  → ")
            (insert-button suggestion
                          'type 'forj-recovery-button
                          'error-context error-context
                          'recovery-action suggestion
                          'face 'forj-button-face
                          'help-echo (format "Click to attempt: %s" suggestion))
            (insert "\n")))))))

;; Define recovery button type
(define-button-type 'forj-recovery-button
  'action #'forj-recovery-button-action
  'follow-link t
  'face 'forj-button-face
  'mouse-face 'forj-button-hover-face)

(defun forj-recovery-button-action (button)
  "Handle recovery button BUTTON activation."
  (let* ((error-context (button-get button 'error-context))
         (recovery-action (button-get button 'recovery-action)))
    
    (condition-case err
        (progn
          ;; Update button state to show it's being processed
          (forj-update-button-state button 'processing)
          
          ;; Attempt the recovery action
          (if (fboundp 'forj-attempt-error-recovery)
              (let ((recovery-result (forj-attempt-error-recovery error-context)))
                (if recovery-result
                    (progn
                      (forj-update-button-state button 'success)
                      (message "✅ Recovery action completed: %s" recovery-action))
                  (forj-update-button-state button 'failed)
                  (message "⚠️ Recovery action failed: %s" recovery-action)))
            ;; If recovery system not available, just show the suggestion
            (forj-update-button-state button 'info)
            (message "💡 Recovery suggestion: %s" recovery-action)))
      
      (error
       (forj-update-button-state button 'error)
       (when (fboundp 'forj-system-error)
         (forj-system-error (format "Recovery button action failed: %s" (error-message-string err))
                           :context "Error recovery UI"
                           :details `(:recovery-action ,recovery-action
                                     :original-error ,error-context)
                           :recovery '("Try manual recovery"
                                      "Check system state"
                                      "Report recovery system issue")))
       (message "❌ Recovery action error: %s" (error-message-string err))))))

(defun forj-update-error-indicators (buffer error-context)
  "Update visual error indicators in BUFFER for ERROR-CONTEXT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (severity (when error-context (forj-error-context-severity error-context))))
        
        ;; Update mode line indicator
        (setq mode-line-format 
              (cons (format " [%s Error]" 
                           (if severity (upcase (symbol-name severity)) "Unknown"))
                    (or (cdr mode-line-format) mode-line-format)))
        
        ;; Update buffer title if it's the conversation buffer
        (when (string= (buffer-name) (bound-and-true-p forj-conversation-buffer))
          (rename-buffer (format "*Forj - Error: %s*" 
                                (if error-context 
                                    (forj-error-context-type error-context)
                                  "Unknown"))))
        
        ;; Force mode line update
        (force-mode-line-update)))))

(defun forj-clear-error-indicators (buffer)
  "Clear error indicators from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Reset buffer name if it was changed for error display
      (when (string-match-p "\\*Forj - Error:" (buffer-name))
        (rename-buffer (or (bound-and-true-p forj-conversation-buffer) "*forj*")))
      
      ;; Reset mode line
      (when (and mode-line-format (string-match-p "Error" (format "%s" mode-line-format)))
        (setq mode-line-format (default-value 'mode-line-format)))
      
      (force-mode-line-update))))

;; Integration function for conversation buffer
(defun forj-integrate-error-display ()
  "Integrate enhanced error display with conversation buffer."
  (when (fboundp 'forj-conversation-buffer)
    (let ((buffer (forj-conversation-buffer)))
      (with-current-buffer buffer
        ;; Add error display hook
        (add-hook 'after-change-functions #'forj-check-for-error-display nil t)))))

(defun forj-check-for-error-display (begin end len)
  "Check if recent buffer changes include error contexts to display."
  ;; This is a hook function that could examine recent changes
  ;; and automatically format any error contexts found
  ;; Implementation would depend on how errors are inserted into buffer
  )

(provide 'forj-ui-components)
;;; forj-ui-components.el ends here