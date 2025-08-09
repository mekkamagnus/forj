;;; test-cases.el --- Edge cases for parenthesis checker

;;; Correct cases
(defun my-correct-function ()
  (interactive)
  (message "This is a correct function."))

(let ((x 1)
      (y 2))
  (+ x y))

;;; Incorrect cases

;; Unmatched closing paren
(defun my-incorrect-function-1 ())
)

;; Unclosed opening paren
(defun my-incorrect-function-2 ()
  (interactive)
  (message "This is an incorrect function."

;; Mismatched closing paren
(defun my-incorrect-function-3 ()
  (interactive)
  (message "This is also incorrect."])

;; Parens inside strings (should be ignored)
(defun my-function-with-strings ()
  (interactive)
  (message "This string has (parens) in it."))

;; Parens inside comments (should be ignored)
;; This is a comment with (parens).
(defun my-function-with-comments ()
  (interactive)
  (message "Hello"))