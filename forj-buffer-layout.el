;;; forj-buffer-layout.el --- Enhanced conversation buffer layout and design -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced conversation buffer layout and visual design for forj.el.
;; Provides professional conversation formatting, split-window support,
;; enhanced message display, and visual hierarchy for AI conversations.
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function forj-apply-theme "forj-theme" (&optional theme-type))
(declare-function forj-setup-conversation-highlighting "forj-syntax-highlight" ())
(declare-function forj-setup-conversation-markdown "forj-markdown" ())

;;; Customization Group

(defgroup forj-layout nil
  "Enhanced buffer layout and design for forj."
  :group 'forj
  :prefix "forj-layout-")

;;; Configuration

(defcustom forj-layout-style 'modern
  "Visual style for conversation buffer layout.
'classic: Traditional text-based layout
'modern: Modern styled layout with visual separators and spacing
'minimal: Clean minimal layout with subtle styling"
  :type '(choice (const :tag "Classic text layout" classic)
                 (const :tag "Modern styled layout" modern)
                 (const :tag "Minimal clean layout" minimal))
  :group 'forj-layout)

(defcustom forj-layout-message-padding 2
  "Number of blank lines between conversation messages."
  :type 'integer
  :group 'forj-layout)

(defcustom forj-layout-show-timestamps t
  "Whether to show timestamps for conversation messages."
  :type 'boolean
  :group 'forj-layout)

(defcustom forj-layout-show-avatars nil
  "Whether to show role avatars/icons in conversation."
  :type 'boolean
  :group 'forj-layout)

(defcustom forj-layout-split-window-threshold 120
  "Minimum window width before enabling split-window mode."
  :type 'integer
  :group 'forj-layout)

(defcustom forj-layout-enable-split-window t
  "Whether to enable automatic split-window layout for wide screens."
  :type 'boolean
  :group 'forj-layout)

;;; Buffer Layout Constants

(defconst forj-layout-separators
  '((classic . "================================================================================")
    (modern . "────────────────────────────────────────────────────────────────────────────────")
    (minimal . "· · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · ·"))
  "Separator strings for different layout styles.")

(defconst forj-layout-role-avatars
  '((user . "👤")
    (assistant . "🤖")
    (system . "⚙️")
    (ai . "🧠")
    (error . "❌")
    (success . "✅"))
  "Avatar/icon mappings for different message roles.")

;;; Buffer Setup and Management

;;;###autoload
(defun forj-setup-enhanced-conversation-buffer ()
  "Set up enhanced conversation buffer with modern layout."
  (let ((buffer (get-buffer-create forj-conversation-buffer)))
    (with-current-buffer buffer
      ;; Set up buffer properties
      (forj-conversation-mode)
      
      ;; Temporarily disable read-only mode for setup
      (let ((inhibit-read-only t))
        ;; Clear and initialize only if buffer is empty or we're reinitializing
        (when (or (= (buffer-size) 0) 
                  (not (boundp 'forj-buffer-setup-complete)))
          (erase-buffer)
          (forj-insert-buffer-header)
          (forj-setup-buffer-layout)
          (setq-local forj-buffer-setup-complete t))
        
        ;; Set up UI enhancements
        (when (featurep 'forj-theme)
          (forj-apply-theme))
        (when (featurep 'forj-syntax-highlight)
          (forj-setup-conversation-highlighting))
        (when (featurep 'forj-markdown)
          (forj-setup-conversation-markdown))
        (when (featurep 'forj-ui-components)
          (forj-setup-ui-components)))
      
      ;; Final setup
      (setq buffer-read-only t)
      (goto-char (point-max)))
    
    buffer))

(defun forj-insert-buffer-header ()
  "Insert enhanced header for conversation buffer."
  (let ((style forj-layout-style))
    (pcase style
      ('modern
       (insert (propertize "╭──────────────────────────────────────────────────────────────────────────────────╮\n"
                          'face 'forj-separator-face))
       (insert (propertize "│" 'face 'forj-separator-face))
       (insert (propertize "                                   FORJ AI CO-PILOT                                   " 
                          'face 'forj-header-face))
       (insert (propertize "│\n" 'face 'forj-separator-face))
       (insert (propertize "│" 'face 'forj-separator-face))
       (insert (propertize "                           AI-Powered Emacs Lisp Development                           "
                          'face 'forj-subheader-face))
       (insert (propertize "│\n" 'face 'forj-separator-face))
       (insert (propertize "╰──────────────────────────────────────────────────────────────────────────────────╯\n\n"
                          'face 'forj-separator-face)))
      ('classic
       (insert (propertize "FORJ AI CO-PILOT\n" 'face 'forj-header-face))
       (insert (propertize (cdr (assq 'classic forj-layout-separators)) 'face 'forj-separator-face))
       (insert "\nAI-Powered Emacs Lisp Development\n\n"))
      ('minimal
       (insert (propertize "forj" 'face 'forj-header-face))
       (insert (propertize " › AI co-pilot ready\n\n" 'face 'forj-subheader-face)))))
  
  ;; Add usage instructions based on style
  (forj-insert-usage-instructions))

(defun forj-insert-usage-instructions ()
  "Insert usage instructions in conversation buffer."
  (let ((instructions 
         '("💡 Quick Start:"
           "  • M-x forj-prompt - Start a conversation"
           "  • C-c C-a - Apply AI suggestions" 
           "  • C-c C-c - Copy code blocks"
           "  • C-c C-r - Reject suggestions"
           "")))
    (dolist (line instructions)
      (insert (propertize line 'face 'forj-info-face) "\n"))
    (insert "\n")))

(defun forj-setup-buffer-layout ()
  "Set up buffer layout properties and local settings."
  ;; Buffer display properties
  (setq word-wrap t)
  (setq truncate-lines nil)
  (visual-line-mode 1)
  
  ;; Set up margins for better readability
  (when (eq forj-layout-style 'modern)
    (setq left-margin-width 2)
    (setq right-margin-width 2)
    (set-window-margins nil left-margin-width right-margin-width)))

;;; Message Formatting

;;;###autoload
(defun forj-format-conversation-message (role content &optional timestamp)
  "Format a conversation message with ROLE, CONTENT, and optional TIMESTAMP.
Returns formatted string ready for buffer insertion."
  (let* ((timestamp (or timestamp (current-time)))
         (time-str (format-time-string "%H:%M:%S" timestamp))
         (style forj-layout-style)
         (avatar (when forj-layout-show-avatars
                  (cdr (assq role forj-layout-role-avatars))))
         (separator (cdr (assq style forj-layout-separators)))
         (role-face (forj-get-role-face role))
         (formatted-content (forj-format-message-content content role)))
    
    (pcase style
      ('modern (forj-format-modern-message role content time-str avatar role-face separator))
      ('classic (forj-format-classic-message role content time-str role-face separator))
      ('minimal (forj-format-minimal-message role content time-str role-face))
      (_ (forj-format-classic-message role content time-str role-face separator)))))

(defun forj-format-modern-message (role content time-str avatar role-face separator)
  "Format message in modern style."
  (let ((role-display (upcase (symbol-name role)))
        (avatar-str (if avatar (concat avatar " ") ""))
        (padding (make-string forj-layout-message-padding ?\n)))
    (format "%s┌─ %s%s%s%s\n│\n%s\n│\n└%s\n%s"
            padding
            avatar-str
            (propertize role-display 'face role-face)
            (if forj-layout-show-timestamps
                (format " %s" (propertize time-str 'face 'forj-timestamp-face))
              "")
            (propertize " " 'face 'forj-separator-face)
            (forj-indent-content content "│ ")
            (make-string (- 80 1) ?─)
            padding)))

(defun forj-format-classic-message (role content time-str role-face separator)
  "Format message in classic style."
  (let ((role-display (upcase (symbol-name role)))
        (padding (make-string forj-layout-message-padding ?\n)))
    (format "%s[%s%s] %s:\n%s\n%s\n%s"
            padding
            (propertize role-display 'face role-face)
            (if forj-layout-show-timestamps
                (format " - %s" (propertize time-str 'face 'forj-timestamp-face))
              "")
            (propertize role-display 'face role-face)
            content
            (propertize separator 'face 'forj-separator-face)
            padding)))

(defun forj-format-minimal-message (role content time-str role-face)
  "Format message in minimal style."
  (let ((role-display (symbol-name role))
        (padding (make-string (max 1 (/ forj-layout-message-padding 2)) ?\n)))
    (format "%s%s%s › %s\n%s"
            padding
            (propertize role-display 'face role-face)
            (if forj-layout-show-timestamps
                (format " %s" (propertize time-str 'face 'forj-timestamp-face))
              "")
            content
            padding)))

(defun forj-indent-content (content prefix)
  "Indent CONTENT with PREFIX for each line."
  (let ((lines (split-string content "\n")))
    (mapconcat (lambda (line) (concat prefix line)) lines "\n")))

(defun forj-format-message-content (content role)
  "Format message CONTENT based on ROLE."
  ;; Apply role-specific formatting
  (pcase role
    ('system (propertize content 'face 'forj-system-message-face))
    ('error (propertize content 'face 'forj-error-face))
    ('user (propertize content 'face 'forj-user-input-face))
    ('assistant (propertize content 'face 'forj-ai-response-face))
    (_ content)))

(defun forj-get-role-face (role)
  "Get appropriate face for ROLE."
  (pcase role
    ('user 'forj-user-input-face)
    ('assistant 'forj-ai-response-face)
    ('ai 'forj-ai-response-face)
    ('system 'forj-system-message-face)
    ('error 'forj-error-face)
    ('success 'forj-success-face)
    (_ 'default)))

;;; Message Display Functions

;;;###autoload
(defun forj-display-enhanced-message (role content &optional timestamp)
  "Display enhanced message with ROLE, CONTENT, and optional TIMESTAMP."
  (let ((buffer (get-buffer "*forj-conversation*")))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (formatted-message (forj-format-conversation-message role content timestamp)))
          (save-excursion
            (goto-char (point-max))
            (insert formatted-message))
          
          ;; Apply post-processing
          (forj-post-process-message role content)
          
          ;; Scroll to bottom
          (goto-char (point-max))
          
          ;; Update display
          (when-let ((window (get-buffer-window buffer)))
            (with-selected-window window
              (recenter -1))))))))

(defun forj-post-process-message (role content)
  "Post-process message with ROLE and CONTENT for enhanced display."
  ;; Apply syntax highlighting to code blocks
  (when (featurep 'forj-syntax-highlight)
    (forj-highlight-buffer))
  
  ;; Apply markdown rendering
  (when (featurep 'forj-markdown)
    (forj-render-markdown-buffer))
  
  ;; Add interactive elements for AI responses
  (when (and (featurep 'forj-ui-components)
             (memq role '(assistant ai)))
    (save-excursion
      (goto-char (point-max))
      (forward-line -10) ; Look back a bit
      (let ((start (point))
            (end (point-max)))
        (forj-enhance-ai-response start end content)))))

;;; Split Window Support

;;;###autoload
(defun forj-setup-split-windows ()
  "Set up side-by-side conversation and code editing windows."
  (interactive)
  (when (and forj-layout-enable-split-window
             (> (window-total-width) forj-layout-split-window-threshold))
    
    (let ((current-buffer (current-buffer))
          (conversation-buffer (get-buffer-create "*forj-conversation*")))
      
      ;; Delete other windows
      (delete-other-windows)
      
      ;; Split window
      (let ((code-window (selected-window))
            (conversation-window (split-window-right)))
        
        ;; Set up conversation window
        (set-window-buffer conversation-window conversation-buffer)
        (with-selected-window conversation-window
          (goto-char (point-max)))
        
        ;; Return focus to code window
        (select-window code-window)
        (switch-to-buffer current-buffer)
        
        ;; Adjust window sizes (60% code, 40% conversation)
        (let ((total-width (frame-width)))
          (window-resize conversation-window 
                        (- (floor (* total-width 0.4)) (window-width conversation-window)) 
                        t))))))

;;;###autoload
(defun forj-toggle-split-layout ()
  "Toggle between split and single window layout."
  (interactive)
  (if (= (length (window-list)) 1)
      (forj-setup-split-windows)
    (progn
      (delete-other-windows)
      (switch-to-buffer "*forj-conversation*"))))

;;; Buffer State Management

(defvar forj-buffer-state
  '(:messages 0
    :last-update nil
    :layout-style nil
    :split-mode nil)
  "State information for conversation buffer.")

;;;###autoload
(defun forj-update-buffer-state ()
  "Update conversation buffer state information."
  (when (get-buffer "*forj-conversation*")
    (with-current-buffer "*forj-conversation*"
      (plist-put forj-buffer-state :last-update (current-time))
      (plist-put forj-buffer-state :layout-style forj-layout-style)
      (plist-put forj-buffer-state :split-mode (> (length (window-list)) 1))
      
      ;; Count messages (rough estimate)
      (let ((message-count 0))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\[\\(USER\\|ASSISTANT\\|SYSTEM\\)" nil t)
            (setq message-count (1+ message-count))))
        (plist-put forj-buffer-state :messages message-count)))))

;;;###autoload
(defun forj-show-buffer-info ()
  "Show information about current buffer state."
  (interactive)
  (forj-update-buffer-state)
  (message "Forj buffer: %d messages, %s layout, %s mode, last update: %s"
           (plist-get forj-buffer-state :messages)
           (plist-get forj-buffer-state :layout-style)
           (if (plist-get forj-buffer-state :split-mode) "split" "single")
           (if (plist-get forj-buffer-state :last-update)
               (format-time-string "%H:%M:%S" (plist-get forj-buffer-state :last-update))
             "never")))

;;; Layout Style Switching

;;;###autoload
(defun forj-set-layout-style (style)
  "Set conversation buffer layout to STYLE."
  (interactive (list (intern (completing-read "Layout style: " 
                                            '("classic" "modern" "minimal")
                                            nil t))))
  (setq forj-layout-style style)
  (when (get-buffer "*forj-conversation*")
    (forj-refresh-buffer-layout))
  (message "Forj layout style set to: %s" style))

;;;###autoload
(defun forj-cycle-layout-style ()
  "Cycle through available layout styles."
  (interactive)
  (let ((styles '(classic modern minimal))
        (current-index (cl-position forj-layout-style styles)))
    (setq forj-layout-style (nth (mod (1+ current-index) (length styles)) styles))
    (when (get-buffer "*forj-conversation*")
      (forj-refresh-buffer-layout))
    (message "Forj layout style: %s" forj-layout-style)))

(defun forj-refresh-buffer-layout ()
  "Refresh the conversation buffer layout."
  (let ((buffer (get-buffer "*forj-conversation*")))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (content (buffer-string))
              (point-pos (point)))
          ;; Save conversation history
          (let ((messages (forj-extract-messages content)))
            ;; Clear and rebuild
            (erase-buffer)
            (forj-insert-buffer-header)
            (forj-setup-buffer-layout)
            
            ;; Restore messages with new formatting
            (dolist (message messages)
              (let ((role (plist-get message :role))
                    (content (plist-get message :content))
                    (timestamp (plist-get message :timestamp)))
                (goto-char (point-max))
                (insert (forj-format-conversation-message role content timestamp))))
            
            ;; Restore point position (approximately)
            (goto-char (min point-pos (point-max)))))))))

(defun forj-extract-messages (content)
  "Extract message data from buffer CONTENT for layout refresh."
  ;; This is a simplified extraction - in practice, we'd store message data separately
  (let ((messages '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(\\w+\\).*\\].*:\n\\(\\(?:.*\n\\)*?\\)^=" nil t)
        (push (list :role (intern (downcase (match-string 1)))
                   :content (string-trim (match-string 2))
                   :timestamp (current-time))
              messages)))
    (nreverse messages)))

;;; Utility Functions

;;;###autoload
(defun forj-clear-conversation-buffer ()
  "Clear conversation buffer and reinitialize with current layout."
  (interactive)
  (when (yes-or-no-p "Clear conversation buffer? ")
    (let ((buffer (get-buffer "*forj-conversation*")))
      (when buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (forj-insert-buffer-header)
            (forj-setup-buffer-layout)
            (goto-char (point-max))))
        (message "Conversation buffer cleared")))))

;;;###autoload
(defun forj-export-conversation ()
  "Export conversation buffer to a file."
  (interactive)
  (let ((buffer (get-buffer "*forj-conversation*"))
        (filename (read-file-name "Export conversation to: " 
                                 nil nil nil 
                                 (format "forj-conversation-%s.txt"
                                        (format-time-string "%Y%m%d-%H%M%S")))))
    (when buffer
      (with-current-buffer buffer
        (write-region (point-min) (point-max) filename))
      (message "Conversation exported to: %s" filename))))

;;; Integration Hooks

(defun forj-layout-after-theme-change ()
  "Update layout after theme changes."
  (when (get-buffer "*forj-conversation*")
    (with-current-buffer "*forj-conversation*"
      (forj-setup-buffer-layout))))

;; Hook into theme system if available
(when (featurep 'forj-theme)
  (add-hook 'forj-theme-update-hook #'forj-layout-after-theme-change))

(provide 'forj-buffer-layout)
;;; forj-buffer-layout.el ends here