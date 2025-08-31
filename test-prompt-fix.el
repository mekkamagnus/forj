;;; test-prompt-fix.el --- Test script to verify forj-prompt fix

;; Load the forj system
(load-file (expand-file-name "forj.el"))

(defun test-forj-prompt-interactive ()
  "Test the forj-prompt interactive workflow."
  (interactive)
  (message "Testing forj-prompt workflow...")
  
  ;; Clear the conversation buffer for testing
  (with-current-buffer (forj-conversation-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== Test Session ===\n")))
  
  ;; Test the interactive prompt interface (what user would call)
  (call-interactively 'forj-prompt)
  
  (message "Test initiated. Use the prompt interface and press C-c C-c to submit."))

(defun test-verify-response ()
  "Check if response appeared in conversation buffer."
  (interactive)
  (with-current-buffer (get-buffer "*forj*")
    (if (string-match-p "AI Response" (buffer-string))
        (message "SUCCESS: AI response found in conversation buffer!")
      (message "FAILED: No AI response found in conversation buffer"))))

;; Make functions available for testing
(message "Test functions loaded:")
(message "- M-x test-forj-prompt-interactive: Test the full workflow") 
(message "- M-x test-verify-response: Check if response appeared")