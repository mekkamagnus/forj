;;; forj-test.el --- Tests for forj.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'forj)

(ert-deftest forj-paren-check-valid ()
  "Test forj-paren-check with valid code."
  (should (equal (forj-paren-check "(defun hello () (message \"world\"))")
                 '(:status valid))))

(ert-deftest forj-paren-check-unclosed-paren ()
  "Test forj-paren-check with unclosed parenthesis."
  (let ((result (forj-paren-check "(defun hello () (message \\"world\\"")))
    (should (eq (plist-get result :status) 'invalid))
    (should (eq (plist-get (car (plist-get result :errors)) :type) 'unclosed-paren)))))

(ert-deftest forj-paren-check-extra-paren ()
  "Test forj-paren-check with extra closing parenthesis."
  (let ((result (forj-paren-check "(defun hello () (message \"world\")))"))
    (should (eq (plist-get result :status) 'invalid))
    (should (eq (plist-get (car (plist-get result :errors)) :type) 'extra-paren))))

(ert-deftest forj-paren-check-mismatched-paren ()
  "Test forj-paren-check with mismatched parentheses."
  (let ((result (forj-paren-check "(defun hello [) (message \"world\")]")))
    (should (eq (plist-get result :status) 'invalid))
    (should (eq (plist-get (car (plist-get result :errors)) :type) 'mismatched-paren))))

(provide 'forj-test)
;;; forj-test.el ends here