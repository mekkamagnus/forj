;;; test-forj-tools-simple.el --- Basic tests for fixed forj-tools -*- lexical-binding: t -*-

;;; Commentary:
;; Simple ERT tests for the fixed forj-tools implementation.

;;; Code:

(require 'ert)
(require 'forj-tools-fixed)

(ert-deftest test-forj-tools-dispatch-basic ()
  "Test basic dispatch functionality."
  (let* ((temp-dir (make-temp-file "forj-test" t))
         (forj-tools-project-root temp-dir)
         (test-file (expand-file-name "test.txt" temp-dir))
         (call-json "{\"id\":\"test1\",\"name\":\"write_file\",\"args\":{\"path\":\"test.txt\",\"content\":\"Hello World\",\"dry_run\":true}}")
         (result-json (forj-tools-dispatch call-json))
         (result (json-read-from-string result-json)))
    (should (assoc 'id result))
    (should (equal (cdr (assoc 'id result)) "test1"))
    (should (assoc 'ok result))
    (should (cdr (assoc 'ok result)))
    (delete-directory temp-dir t)))

(ert-deftest test-forj-paren-check-integration ()
  "Test that paren checker integrates with write operations."
  (let* ((temp-dir (make-temp-file "forj-test" t))
         (forj-tools-project-root temp-dir)
         (invalid-elisp "(defun broken () (message \"hello\"")
         (call-json (json-encode `((id . "test2")
                                 (name . "write_file")
                                 (args . ((path . "test.el")
                                        (content . ,invalid-elisp)
                                        (dry_run . t))))))
         (result-json (forj-tools-dispatch call-json))
         (result (json-read-from-string result-json)))
    (should (not (cdr (assoc 'ok result))))
    (should (assoc 'error result))
    (delete-directory temp-dir t)))

(provide 'test-forj-tools-simple)

;;; test-forj-tools-simple.el ends here