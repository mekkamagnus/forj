;;; forj-ui-test.el --- Comprehensive tests for forj UI components -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive test suite for all forj UI components including:
;; - Syntax highlighting system
;; - Markdown rendering engine  
;; - Theme system and faces
;; - Interactive UI components
;; - Progress indicators
;; - Buffer layout system
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load all UI components
(require 'forj-syntax-highlight)
(require 'forj-markdown)
(require 'forj-theme)
(require 'forj-ui-components)
(require 'forj-progress)
(require 'forj-buffer-layout)

;;; Syntax Highlighting Tests

(ert-deftest forj-ui-test-syntax-highlight-elisp ()
  "Test syntax highlighting for Emacs Lisp code blocks."
  (with-temp-buffer
    (insert "```elisp\n(defun test-func () \"Test function\")\n```")
    (forj-highlight-code-blocks)
    (should (get-text-property 10 'forj-code-block))
    (should (get-text-property 15 'face))))

(ert-deftest forj-ui-test-syntax-highlight-multiple-languages ()
  "Test syntax highlighting for multiple language code blocks."
  (with-temp-buffer
    (insert "```python\ndef hello():\n    print('world')\n```\n\n")
    (insert "```javascript\nfunction test() {\n  console.log('test');\n}\n```")
    (forj-highlight-code-blocks)
    
    ;; Check both code blocks are highlighted
    (goto-char (point-min))
    (should (re-search-forward "```python" nil t))
    (should (get-text-property (point) 'face))
    
    (should (re-search-forward "```javascript" nil t))
    (should (get-text-property (point) 'face))))

(ert-deftest forj-ui-test-syntax-highlight-language-detection ()
  "Test language detection and mode mapping."
  (should (eq (forj-get-language-mode "elisp") 'emacs-lisp-mode))
  (should (eq (forj-get-language-mode "python") 'python-mode))
  (should (eq (forj-get-language-mode "js") 'js-mode))
  (should (null (forj-get-language-mode "unknown-language"))))

(ert-deftest forj-ui-test-syntax-highlight-inline-code ()
  "Test inline code highlighting."
  (with-temp-buffer
    (insert "Some text with `inline code` in it.")
    (forj-highlight-inline-code)
    (goto-char (point-min))
    (should (re-search-forward "`inline code`" nil t))
    (should (get-text-property (match-beginning 0) 'face))))

;;; Markdown Rendering Tests

(ert-deftest forj-ui-test-markdown-headers ()
  "Test markdown header rendering."
  (with-temp-buffer
    (insert "# Header 1\n## Header 2\n### Header 3")
    (forj-render-headers)
    
    (goto-char (point-min))
    (should (re-search-forward "# Header 1" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-header1-face))
    
    (should (re-search-forward "## Header 2" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-header2-face))
    
    (should (re-search-forward "### Header 3" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-header3-face))))

(ert-deftest forj-ui-test-markdown-emphasis ()
  "Test markdown emphasis rendering."
  (with-temp-buffer
    (insert "**bold text** and *italic text* and ~~strikethrough~~")
    (forj-render-emphasis)
    
    (goto-char (point-min))
    (should (re-search-forward "\\*\\*bold text\\*\\*" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-bold-face))
    
    (should (re-search-forward "\\*italic text\\*" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-italic-face))
    
    (should (re-search-forward "~~strikethrough~~" nil t))
    (should (eq (get-text-property (match-beginning 0) 'face) 'forj-markdown-strike-face))))

(ert-deftest forj-ui-test-markdown-lists ()
  "Test markdown list rendering."
  (with-temp-buffer
    (insert "- Item 1\n- Item 2\n1. Numbered item\n2. Another item")
    (forj-render-lists)
    
    (goto-char (point-min))
    (should (re-search-forward "^\\s-*\\(-\\)" nil t))
    (should (eq (get-text-property (match-beginning 1) 'face) 'forj-markdown-list-face))
    
    (should (re-search-forward "^\\s-*\\(1\\.\\)" nil t))
    (should (eq (get-text-property (match-beginning 1) 'face) 'forj-markdown-list-face))))

(ert-deftest forj-ui-test-markdown-links ()
  "Test markdown link rendering and button creation."
  (with-temp-buffer
    (insert "[Test Link](https://example.com)")
    (forj-render-links)
    
    (goto-char (point-min))
    (let ((button (next-button (point))))
      (should button)
      (should (eq (button-type button) 'forj-markdown-link))
      (should (string= (button-get button 'forj-url) "https://example.com"))
      (should (string= (button-get button 'forj-link-text) "Test Link")))))

(ert-deftest forj-ui-test-markdown-table-detection ()
  "Test markdown table detection and formatting."
  (with-temp-buffer
    (insert "| Header 1 | Header 2 |\n|----------|----------|\n| Cell 1   | Cell 2   |")
    (should (progn (goto-char (point-min)) (forj-at-table-p)))
    
    (let ((table-end (forj-find-table-end)))
      (should (> table-end (point-min))))))

;;; Theme System Tests

(ert-deftest forj-ui-test-theme-detection ()
  "Test theme detection functionality."
  ;; Test basic theme detection functions
  (should (symbolp (forj-detect-theme-type)))
  (should (member (forj-detect-theme-type) '(light dark))))

(ert-deftest forj-ui-test-theme-faces-exist ()
  "Test that all forj theme faces are properly defined."
  (let ((required-faces '(forj-user-input-face
                         forj-ai-response-face
                         forj-system-message-face
                         forj-success-face
                         forj-error-face
                         forj-warning-face
                         forj-info-face
                         forj-button-face
                         forj-button-hover-face
                         forj-progress-face)))
    (dolist (face required-faces)
      (should (forj-face-exists-p face)))))

(ert-deftest forj-ui-test-theme-color-utilities ()
  "Test theme color utility functions."
  (let ((test-color "#ff0000"))
    (should (stringp (forj-lighten-color test-color 0.2)))
    (should (stringp (forj-darken-color test-color 0.2)))
    (should (stringp (forj-blend-colors test-color "#0000ff" 0.5)))))

(ert-deftest forj-ui-test-theme-application ()
  "Test theme application functions."
  (should (functionp 'forj-apply-light-theme))
  (should (functionp 'forj-apply-dark-theme))
  (should (functionp 'forj-apply-theme))
  
  ;; Test that theme application doesn't error
  (should (progn (forj-apply-light-theme) t))
  (should (progn (forj-apply-dark-theme) t)))

;;; UI Components Tests

(ert-deftest forj-ui-test-button-creation ()
  "Test interactive button creation."
  (with-temp-buffer
    (let ((apply-button (forj-insert-apply-button "test suggestion"))
          (copy-button (forj-insert-copy-button "test code"))
          (reject-button (forj-insert-reject-button)))
      
      (should (button-at (button-start apply-button)))
      (should (eq (button-type apply-button) 'forj-apply-button))
      
      (should (button-at (button-start copy-button)))
      (should (eq (button-type copy-button) 'forj-copy-button))
      
      (should (button-at (button-start reject-button)))
      (should (eq (button-type reject-button) 'forj-reject-button)))))

(ert-deftest forj-ui-test-button-properties ()
  "Test button properties and metadata."
  (with-temp-buffer
    (let ((copy-button (forj-insert-copy-button "test code content" "elisp")))
      (should (string= (button-get copy-button 'forj-code-content) "test code content"))
      (should (string= (button-get copy-button 'forj-code-type) "elisp"))
      (should (string= (button-get copy-button 'forj-original-text) "Copy")))))

(ert-deftest forj-ui-test-collapsible-sections ()
  "Test collapsible section creation and functionality."
  (with-temp-buffer
    (insert "Content to make collapsible\nMore content\nEven more content")
    (let ((start (point-min))
          (end (point-max)))
      (goto-char end)
      (let ((overlay (forj-make-collapsible start end "Test Section" t)))
        (should overlay)
        (should (overlay-get overlay 'forj-collapsible))
        (should (string= (overlay-get overlay 'forj-title) "Test Section"))))))

(ert-deftest forj-ui-test-interactive-button-groups ()
  "Test interactive button group creation."
  (with-temp-buffer
    (insert "Test content for buttons")
    (let ((start (point-min))
          (end (point-max)))
      (forj-add-interactive-buttons start end 'code)
      
      ;; Should have added buttons and separators
      (goto-char end)
      (should (re-search-forward "Copy" nil t))
      (should (button-at (match-beginning 0))))))

(ert-deftest forj-ui-test-context-menu ()
  "Test context menu functionality."
  (should (keymapp forj-context-menu-map))
  (should (lookup-key forj-context-menu-map [forj-apply]))
  (should (lookup-key forj-context-menu-map [forj-copy]))
  (should (lookup-key forj-context-menu-map [forj-reject])))

;;; Progress Indicators Tests

(ert-deftest forj-ui-test-progress-spinner ()
  "Test progress spinner creation and management."
  (let ((progress-id (forj-show-progress "Test progress")))
    (should progress-id)
    (should (forj-find-progress-indicator progress-id))
    
    ;; Test update
    (should (progn (forj-update-progress progress-id 50 "Updated message") t))
    
    ;; Test completion
    (should (progn (forj-complete-progress progress-id "Completed") t))
    
    ;; Cleanup
    (forj-hide-progress progress-id)))

(ert-deftest forj-ui-test-progress-bar ()
  "Test progress bar creation and updates."
  (let ((progress-id (forj-show-progress-bar "Test progress bar" 100)))
    (should progress-id)
    (should (forj-find-progress-indicator progress-id))
    
    ;; Test multiple updates
    (forj-update-progress progress-id 25)
    (forj-update-progress progress-id 50)
    (forj-update-progress progress-id 75)
    
    (let ((indicator (forj-find-progress-indicator progress-id)))
      (should (= (forj-progress-indicator-progress indicator) 75)))
    
    ;; Cleanup
    (forj-hide-progress progress-id)))

(ert-deftest forj-ui-test-progress-spinner-frames ()
  "Test progress spinner frame animation."
  (dolist (style '(dots bars arrows clock))
    (let ((frames (cdr (assq style forj-spinner-frames))))
      (should (vectorp frames))
      (should (> (length frames) 0))
      (should (cl-every #'stringp frames)))))

(ert-deftest forj-ui-test-progress-status-indicators ()
  "Test status indicator display."
  (with-temp-buffer
    (forj-show-status "Test success" 'success)
    (should (re-search-backward "✅ Test success" nil t))
    
    (forj-show-status "Test error" 'error)
    (should (re-search-backward "❌ Test error" nil t))
    
    (forj-show-status "Test warning" 'warning)
    (should (re-search-backward "⚠️ Test warning" nil t))
    
    (forj-show-status "Test info" 'info)
    (should (re-search-backward "ℹ️ Test info" nil t))))

(ert-deftest forj-ui-test-progress-utilities ()
  "Test progress utility functions."
  (should (functionp 'forj-with-progress))
  (should (functionp 'forj-with-progress-bar))
  
  ;; Test progress wrapper function
  (let ((result (forj-with-progress "Testing" (lambda () "test-result"))))
    (should (string= result "test-result"))))

;;; Buffer Layout Tests

(ert-deftest forj-ui-test-buffer-layout-styles ()
  "Test different buffer layout styles."
  (dolist (style '(classic modern minimal))
    (let ((forj-layout-style style))
      (with-temp-buffer
        (forj-insert-buffer-header)
        (should (> (buffer-size) 0))))))

(ert-deftest forj-ui-test-message-formatting ()
  "Test conversation message formatting."
  (dolist (style '(classic modern minimal))
    (let ((forj-layout-style style))
      (let ((formatted (forj-format-conversation-message 
                       'user "Test message content" (current-time))))
        (should (stringp formatted))
        (should (> (length formatted) 0))
        (should (string-match-p "user\\|USER" formatted))))))

(ert-deftest forj-ui-test-message-roles ()
  "Test message formatting for different roles."
  (let ((roles '(user assistant system error success)))
    (dolist (role roles)
      (let ((formatted (forj-format-conversation-message 
                       role "Test content" (current-time))))
        (should (stringp formatted))
        (should (> (length formatted) 0))))))

(ert-deftest forj-ui-test-role-face-mapping ()
  "Test role-to-face mapping."
  (should (eq (forj-get-role-face 'user) 'forj-user-input-face))
  (should (eq (forj-get-role-face 'assistant) 'forj-ai-response-face))
  (should (eq (forj-get-role-face 'system) 'forj-system-message-face))
  (should (eq (forj-get-role-face 'error) 'forj-error-face))
  (should (eq (forj-get-role-face 'success) 'forj-success-face)))

(ert-deftest forj-ui-test-buffer-state-management ()
  "Test buffer state tracking."
  (forj-update-buffer-state)
  (should (plist-get forj-buffer-state :last-update))
  (should (symbolp (plist-get forj-buffer-state :layout-style))))

;;; Integration Tests

(ert-deftest forj-ui-test-enhanced-conversation-buffer ()
  "Test enhanced conversation buffer setup."
  (let ((buffer (forj-setup-enhanced-conversation-buffer)))
    (should (bufferp buffer))
    (with-current-buffer buffer
      (should (> (buffer-size) 0))
      (should (eq major-mode 'forj-conversation-mode)))))

(ert-deftest forj-ui-test-message-display-integration ()
  "Test integrated message display with all UI enhancements."
  (with-temp-buffer
    (forj-display-enhanced-message 'user "Test user message")
    (should (> (buffer-size) 0))
    
    (forj-display-enhanced-message 'assistant "Test AI response with ```elisp\n(defun test ())\n```")
    (should (> (buffer-size) 100))
    
    ;; Should have syntax highlighting
    (goto-char (point-min))
    (when (re-search-forward "```elisp" nil t)
      (should (get-text-property (point) 'face)))))

(ert-deftest forj-ui-test-split-window-support ()
  "Test split window functionality."
  (should (functionp 'forj-setup-split-windows))
  (should (functionp 'forj-toggle-split-layout))
  
  ;; Test threshold logic
  (should (numberp forj-layout-split-window-threshold))
  (should (> forj-layout-split-window-threshold 0)))

;;; Performance and Stress Tests

(ert-deftest forj-ui-test-large-content-performance ()
  "Test UI performance with large content."
  (with-temp-buffer
    ;; Create large content
    (dotimes (i 100)
      (insert (format "# Header %d\n\n" i))
      (insert "```elisp\n(defun test-function-")
      (insert (number-to-string i))
      (insert " ()\n  \"Test function\")\n```\n\n")
      (insert "Some **bold text** and *italic text* content.\n\n"))
    
    ;; Time the rendering (should be reasonably fast)
    (let ((start-time (current-time)))
      (forj-render-markdown-buffer)
      (forj-highlight-buffer)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (should (< elapsed 2.0)))))) ; Should complete within 2 seconds

(ert-deftest forj-ui-test-multiple-progress-indicators ()
  "Test multiple concurrent progress indicators."
  (let ((progress-ids '()))
    ;; Create multiple progress indicators
    (dotimes (i 5)
      (push (forj-show-progress (format "Progress %d" i)) progress-ids))
    
    (should (= (length forj-active-progress-indicators) 5))
    
    ;; Update all of them
    (dolist (id progress-ids)
      (forj-update-progress id 50))
    
    ;; Complete them all
    (dolist (id progress-ids)
      (forj-complete-progress id))
    
    ;; Cleanup
    (forj-clear-all-progress)
    (should (= (length forj-active-progress-indicators) 0))))

;;; Error Handling Tests

(ert-deftest forj-ui-test-error-handling ()
  "Test UI component error handling."
  ;; Test with invalid progress ID
  (should-not (forj-find-progress-indicator "nonexistent-id"))
  (should-not (forj-update-progress "nonexistent-id" 50))
  
  ;; Test with invalid theme colors
  (should (stringp (forj-lighten-color "invalid-color" 0.2)))
  (should (stringp (forj-darken-color "invalid-color" 0.2)))
  
  ;; Test markdown rendering with malformed content
  (with-temp-buffer
    (insert "# Incomplete header\n**unclosed bold\n```unclosed code")
    (should (progn (forj-render-markdown-buffer) t))))

;;; Cleanup and Resource Management Tests

(ert-deftest forj-ui-test-resource-cleanup ()
  "Test proper resource cleanup."
  ;; Test progress cleanup
  (let ((progress-id (forj-show-progress "Test cleanup")))
    (forj-hide-progress progress-id)
    (should-not (forj-find-progress-indicator progress-id)))
  
  ;; Test timer cleanup
  (forj-cleanup-progress)
  (should-not forj-progress-update-timer)
  
  ;; Test overlay cleanup in collapsible sections
  (with-temp-buffer
    (insert "Test content")
    (let ((overlay (forj-make-collapsible (point-min) (point-max) "Test" t)))
      (delete-overlay overlay)
      (should-not (overlay-buffer overlay)))))

;;; Configuration Tests

(ert-deftest forj-ui-test-configuration-options ()
  "Test UI configuration options."
  ;; Test syntax highlighting configuration
  (should (memq forj-highlight-enabled-languages '(all)))
  (should (vectorp (cdr (assq forj-progress-spinner-style forj-spinner-frames))))
  
  ;; Test theme configuration
  (should (memq forj-theme-style '(auto light dark)))
  
  ;; Test layout configuration  
  (should (memq forj-layout-style '(classic modern minimal)))
  (should (numberp forj-layout-message-padding))
  (should (numberp forj-layout-split-window-threshold)))

;;; Acceptance Criteria Tests

(ert-deftest forj-ac1-syntax-highlighting ()
  "AC1: Validate syntax highlighting for multiple languages."
  (dolist (lang '("elisp" "python" "javascript"))
    (with-temp-buffer
      (insert (format "```%s\ntest code\n```" lang))
      (forj-highlight-code-blocks)
      (goto-char (point-min))
      (should (re-search-forward (format "```%s" lang) nil t))
      (should (get-text-property (point) 'face)))))

(ert-deftest forj-ac2-markdown-rendering ()
  "AC2: Validate markdown rendering features."
  (with-temp-buffer
    (let ((markdown-text "# Header\n\n- Item 1\n- Item 2\n\n**Bold** text\n\n[Link](http://example.com)"))
      (insert markdown-text)
      (forj-render-markdown-buffer)
      
      ;; Check headers
      (goto-char (point-min))
      (should (re-search-forward "# Header" nil t))
      (should (get-text-property (match-beginning 0) 'face))
      
      ;; Check lists
      (should (re-search-forward "- Item 1" nil t))
      (should (get-text-property (match-beginning 0) 'face))
      
      ;; Check emphasis
      (should (re-search-forward "\\*\\*Bold\\*\\*" nil t))
      (should (get-text-property (match-beginning 0) 'face))
      
      ;; Check links
      (should (next-button (point-min))))))

(ert-deftest forj-ac3-theme-integration ()
  "AC3: Validate theme integration and customization."
  (forj-apply-light-theme)
  (should (eq (forj-detect-theme-type) 'light))
  
  (forj-apply-dark-theme)
  ;; Theme should be applied without errors
  (should t))

(ert-deftest forj-ac4-interactive-elements ()
  "AC4: Validate interactive UI components."
  (should (functionp 'forj-apply-button-action))
  (should (functionp 'forj-copy-button-action))  
  (should (functionp 'forj-reject-button-action))
  (should (keymapp forj-context-menu-map))
  
  (with-temp-buffer
    (let ((button (forj-insert-apply-button "test")))
      (should (button-at (button-start button)))
      (should (eq (button-type button) 'forj-apply-button)))))

;;; Test Suite Summary

(defun forj-ui-run-all-tests ()
  "Run all forj UI tests and display summary."
  (interactive)
  (let ((start-time (current-time)))
    (ert-run-tests-batch "^forj-ui-test\\|^forj-ac[0-9]")
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Forj UI test suite completed in %.2f seconds" elapsed))))

(provide 'forj-ui-test)
;;; forj-ui-test.el ends here