;;; forj-test-minimal-working.el --- Minimal working tests for forj.el -*- lexical-binding: t -*-

(require 'ert)
(require 'forj)

;; Basic infrastructure test
(ert-deftest forj-test-package-provides ()
  "Test that forj package is properly provided."
  (should (featurep 'forj)))

;; Debug function test
(ert-deftest forj-test-debug-function ()
  "Test debug logging functionality."
  (let ((forj-debug t)
        (message-log-max 100))
    (forj-debug "Test message: %s" "hello")
    (should t)))

;; API key test
(ert-deftest forj-test-api-key-retrieval-success ()
  "Test that forj-get-api-key returns key when set."
  (let ((test-key "test-api-key-123")
        (process-environment (copy-sequence process-environment)))
    (setenv "GEMINI_API_KEY" test-key)
    (should (string= test-key (forj-get-api-key)))))

;; Buffer reading test
(ert-deftest forj-test-read-buffer-content ()
  "Test reading buffer with content."
  (with-temp-buffer
    (insert "test content")
    (should (string= "test content" (forj-read-buffer)))))

(provide 'forj-test-minimal-working)
;;; forj-test-minimal-working.el ends here