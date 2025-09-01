;;; forj-paren-checker.el --- Parentheses balance checker for AI agents -*- lexical-binding: t -*-

;;; Commentary:
;; Custom parentheses balance checker for AI agents to validate generated
;; Emacs Lisp code with detailed error reporting and correction suggestions.

;;; Code:

(require 'cl-lib)

(defun forj-paren-check (code-string)
  "Analyze CODE-STRING for parentheses balance and syntax errors.
Returns a plist with :status, :balanced, :errors, and :message."
  (let ((stack '())
        (errors '())
        (line 1)
        (column 1)
        (in-string nil)
        (in-comment nil)
        (string-start-line nil)
        (string-start-column nil)
        (i 0)
        (len (length code-string)))
    
    (while (< i len)
      (let ((char (aref code-string i))
            (next-char (when (< (1+ i) len) (aref code-string (1+ i)))))
        
        (cond
         ;; Handle newlines
         ((eq char ?\n)
          (setq line (1+ line)
                column 1
                in-comment nil))
         
         ;; Handle comments (outside strings)
         ((and (eq char ?\;) (not in-string))
          (setq in-comment t))
         
         ;; Handle string literals
         ((and (eq char ?\") (not in-comment))
          (cond
           ;; Starting a string
           ((not in-string)
            (setq in-string t
                  string-start-line line
                  string-start-column column))
           ;; Ending a string (not escaped)
           ((and in-string
                 (not (and (> i 0) (eq (aref code-string (1- i)) ?\\))))
            (setq in-string nil
                  string-start-line nil
                  string-start-column nil))))
         
         ;; Handle parentheses (outside strings and comments)
         ((and (not in-string) (not in-comment))
          (cond
           ;; Opening parens
           ((memq char '(?\( ?\[ ?\{))
            (push (list char line column) stack))
           
           ;; Closing parens
           ((memq char '(?\) ?\] ?\}))
            (let ((expected-open (cond ((eq char ?\)) ?\()
                                       ((eq char ?\]) ?\[)
                                       ((eq char ?\}) ?\{))))
              (cond
               ;; Stack is empty - unexpected closing paren
               ((null stack)
                (push (list :line line
                           :column column
                           :type 'extra-paren
                           :paren-type 'close-paren
                           :expected nil
                           :found (char-to-string char)
                           :message (format "Unexpected closing %s"
                                          (cond ((eq char ?\)) "parenthesis")
                                                ((eq char ?\]) "bracket")
                                                ((eq char ?\}) "brace")))
                           :suggestion (format "Remove '%c' at line %d, column %d"
                                             char line column))
                      errors))
               
               ;; Wrong closing paren type
               ((not (eq (caar stack) expected-open))
                (let* ((open-info (car stack))
                       (open-char (car open-info))
                       (open-line (cadr open-info))
                       (open-column (caddr open-info))
                       (expected-close (cond ((eq open-char ?\() ")")
                                            ((eq open-char ?\[) "]")
                                            ((eq open-char ?\{) "}"))))
                  (push (list :line line
                             :column column
                             :type 'unmatched-paren
                             :paren-type 'close-paren
                             :expected expected-close
                             :found (char-to-string char)
                             :message (format "Unmatched closing %s, expected %s for opening at line %d, column %d"
                                            (cond ((eq char ?\)) "parenthesis")
                                                  ((eq char ?\]) "bracket")
                                                  ((eq char ?\}) "brace"))
                                            expected-close
                                            open-line open-column)
                             :suggestion (format "Replace '%c' with '%s' at line %d, column %d"
                                               char expected-close line column))
                        errors)))
               
               ;; Correct closing paren
               (t (pop stack))))))))
        
        ;; Update position
        (unless (eq char ?\n)
          (setq column (1+ column)))
        (setq i (1+ i))))
    
    ;; Check for unclosed string
    (when in-string
      (push (list :line string-start-line
                 :column string-start-column
                 :type 'string-unclosed
                 :paren-type 'string
                 :expected "\""
                 :found nil
                 :message (format "Unclosed string literal starting at line %d, column %d"
                                string-start-line string-start-column)
                 :suggestion (format "Add closing quote for string at line %d, column %d"
                                   string-start-line string-start-column))
            errors))
    
    ;; Check for unclosed parens
    (dolist (open-info stack)
      (let* ((open-char (car open-info))
             (open-line (cadr open-info))
             (open-column (caddr open-info))
             (expected-close (cond ((eq open-char ?\() ")")
                                  ((eq open-char ?\[) "]")
                                  ((eq open-char ?\{) "}")))
             (paren-name (cond ((eq open-char ?\() "parenthesis")
                              ((eq open-char ?\[) "bracket")
                              ((eq open-char ?\{) "brace"))))
        (push (list :line open-line
                   :column open-column
                   :type 'unclosed-paren
                   :paren-type 'open-paren
                   :expected expected-close
                   :found nil
                   :message (format "Unclosed %s at line %d, column %d"
                                  paren-name open-line open-column)
                   :suggestion (format "Add closing '%s' for %s at line %d, column %d"
                                     expected-close paren-name open-line open-column))
              errors)))
    
    ;; Build result
    (if errors
        (list :status 'invalid
              :balanced nil
              :errors (nreverse errors)
              :message (format "Found %d syntax error%s"
                             (length errors)
                             (if (= (length errors) 1) "" "s")))
      (list :status 'valid
            :balanced t
            :message "Code is syntactically valid"))))

(provide 'forj-paren-checker)

;;; forj-paren-checker.el ends here