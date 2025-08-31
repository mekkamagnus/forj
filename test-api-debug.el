;;; test-api-debug.el --- Test script for debugging API issues -*- lexical-binding: t -*-

;; Load required modules
(setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
(load-file "forj-error-system.el")
(load-file "forj.el")

;; Test function
(defun test-api-debug ()
  "Test the API functionality with debug output."
  (interactive)
  (let ((api-key (getenv "GEMINI_API_KEY")))
    (if (not api-key)
        (message "ERROR: GEMINI_API_KEY not set")
      (progn
        (message "Testing API with key: %s..." (substring api-key 0 10))
        (let ((response (forj-api-request "Hello, test message")))
          (if response
              (message "SUCCESS: Received response: %s" (substring response 0 100))
            (message "FAILED: No response received")))))))

;; Enable debug
(setq debug-on-error t)

(message "Test script loaded. Run (test-api-debug) to test API.")

;;; test-api-debug.el ends here