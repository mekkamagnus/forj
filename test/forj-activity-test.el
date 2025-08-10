;;; test/forj-activity-test.el --- Tests for forj activity tracking -*- lexical-binding: t -*-

(require 'ert)
(require 'forj)

(ert-deftest forj-test-activity-tracking ()
  "Test that activity tracking updates correctly."
  (let ((buffer-name forj-conversation-buffer))
    ;; Clean state
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    ;; Create conversation buffer
    (forj-conversation-buffer)
    
    ;; Test setting activity
    (forj-set-activity "Reading file...")
    (should (string= forj-current-activity "Reading file..."))
    
    ;; Test activity display in buffer
    (with-current-buffer buffer-name
      (should (string-match-p "🔄 Reading file..." (buffer-string))))
    
    ;; Test clearing activity
    (forj-set-activity nil)
    (should (null forj-current-activity))))

(ert-deftest forj-test-conversation-history ()
  "Test conversation history tracking."
  ;; Clear history
  (setq forj-conversation-history nil)
  
  ;; Add a user message
  (forj-add-to-history 'user "Test prompt")
  (should (= (length forj-conversation-history) 1))
  
  ;; Check structure
  (let ((entry (car forj-conversation-history)))
    (should (eq (plist-get entry :role) 'user))
    (should (string= (plist-get entry :content) "Test prompt"))
    (should (plist-get entry :timestamp)))
  
  ;; Add an assistant response
  (forj-add-to-history 'assistant "Test response")
  (should (= (length forj-conversation-history) 2))
  
  ;; Test clear history
  (forj-clear-conversation)
  (should (null forj-conversation-history)))

(ert-deftest forj-test-conversation-display ()
  "Test that conversation entries display properly in buffer."
  (let ((buffer-name forj-conversation-buffer))
    ;; Clean state
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (setq forj-conversation-history nil)
    
    ;; Create buffer and add history entry
    (forj-conversation-buffer)
    (forj-add-to-history 'user "Hello AI")
    
    ;; Check that entry appears in buffer
    (with-current-buffer buffer-name
      (should (string-match-p "USER:" (buffer-string)))
      (should (string-match-p "Hello AI" (buffer-string))))))

(provide 'forj-activity-test)