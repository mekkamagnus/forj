;;; test/forj-conversation-test.el --- Tests for forj conversation buffer -*- lexical-binding: t -*-

(require 'ert)
(require 'forj)

(ert-deftest forj-test-conversation-buffer-creation ()
  "Test that forj-conversation-buffer is created correctly."
  (let ((buffer-name forj-conversation-buffer))
    ;; Ensure buffer doesn't exist initially
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    ;; Create the conversation buffer
    (forj-conversation-buffer)
    
    ;; Verify buffer exists
    (should (get-buffer buffer-name))
    
    ;; Verify buffer is in the correct mode
    (with-current-buffer buffer-name
      (should (eq major-mode 'forj-conversation-mode)))
    
    ;; Verify buffer has the correct name
    (should (string= (buffer-name (get-buffer buffer-name)) buffer-name))))

(ert-deftest forj-test-conversation-buffer-content ()
  "Test that conversation buffer displays initial content correctly."
  (let ((buffer-name forj-conversation-buffer))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (forj-conversation-buffer)
    
    (with-current-buffer buffer-name
      (should (string-match-p "Forj AI Assistant" (buffer-string)))
      (should (string-match-p "Type your prompt below" (buffer-string))))))

(provide 'forj-conversation-test)