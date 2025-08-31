;;; forj-syntax-highlight.el --- Simple working syntax highlighting -*- lexical-binding: t -*-

;;; Commentary:
;; Simple but working syntax highlighting for forj code blocks

;;; Code:

(require 'font-lock)
(require 'forj-theme)

;;; Faces

(defface forj-language-label-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for language labels in code blocks."
  :group 'forj-faces)

(defface forj-code-block-face
  '((((background light)) :background "#f8f9fa" :extend t)
    (((background dark))  :background "#1a1a1a" :extend t)
    (t :background "gray90" :extend t))
  "Face for code blocks background."
  :group 'forj-faces)

;;; Main highlighting function

(defun forj-highlight-code-blocks ()
  "Apply syntax highlighting to code blocks in current buffer."
  (interactive)
  (save-excursion
    ;; First highlight inline code (backticks)
    (forj-highlight-inline-code)
    
    ;; Then highlight code blocks
    (goto-char (point-min))
    (let ((case-fold-search nil))
      ;; Find code blocks with triple backticks (including indented ones)
      (while (re-search-forward "^\\s-*```\\([a-zA-Z0-9+-]*\\)\\s-*\\(\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(\n\\)\\s-*```" nil t)
        (let* ((lang-start (match-beginning 1))
               (lang-end (match-end 1))
               (lang (when (< lang-start lang-end)
                      (match-string 1)))
               (code-start (match-beginning 3))
               (code-end (match-end 3))
               (block-start (match-beginning 0))
               (block-end (match-end 0)))
          
          ;; Apply highlighting to the entire code block
          (add-text-properties block-start block-end
                             '(face forj-code-block-face
                               forj-code-block t))
          
          ;; Apply language label highlighting if present
          (when (and lang (> (length lang) 0))
            (add-text-properties lang-start lang-end
                               '(face forj-language-label-face)))
          
          ;; Apply elisp syntax highlighting
          (when (and lang (string= lang "elisp"))
            (forj-highlight-elisp-region code-start code-end)))))))

(defun forj-highlight-inline-code ()
  "Apply highlighting to inline code marked with backticks."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (add-text-properties start end
                           '(face forj-inline-code-face))))))

(defun forj-highlight-elisp-region (start end)
  "Apply elisp syntax highlighting between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (skip-chars-forward " \t\n" end)
      (when (< (point) end)
        (cond
         ;; Function definitions
         ((looking-at "(defun\\s-+\\([[:word:]-]+\\)")
          (add-text-properties (match-beginning 1) (match-end 1)
                             '(face font-lock-function-name-face))
          (goto-char (match-end 0)))
         ;; Variable definitions  
         ((looking-at "(defvar\\s-+\\([[:word:]-]+\\)")
          (add-text-properties (match-beginning 1) (match-end 1)
                             '(face font-lock-variable-name-face))
          (goto-char (match-end 0)))
         ;; Strings
         ((looking-at "\"\\([^\"\\\\]\\|\\\\.\\)*\"")
          (add-text-properties (match-beginning 0) (match-end 0)
                             '(face font-lock-string-face))
          (goto-char (match-end 0)))
         ;; Comments
         ((looking-at ";.*$")
          (add-text-properties (match-beginning 0) (match-end 0)
                             '(face font-lock-comment-face))
          (goto-char (match-end 0)))
         ;; Keywords
         ((looking-at "(\\(let\\*?\\|when\\|unless\\|if\\|cond\\|setq\\|message\\|interactive\\|progn\\)\\b")
          (add-text-properties (match-beginning 1) (match-end 1)
                             '(face font-lock-keyword-face))
          (goto-char (match-end 1)))
         ;; Built-in functions
         ((looking-at "(\\(concat\\|format\\|length\\|car\\|cdr\\|cons\\)\\b")
          (add-text-properties (match-beginning 1) (match-end 1)
                             '(face font-lock-builtin-face))
          (goto-char (match-end 1)))
         (t
          (forward-char 1)))))))

;;; Setup function

;;;###autoload
(defun forj-setup-conversation-highlighting ()
  "Set up syntax highlighting for forj conversation buffer."
  (when (and (boundp 'forj-conversation-buffer)
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      (add-hook 'after-change-functions #'forj-highlight-after-change nil t)
      (let ((inhibit-read-only t))
        (forj-highlight-code-blocks)))))

;;;###autoload
(defun forj-fix-highlighting ()
  "Fix syntax highlighting in current buffer - troubleshooting command."
  (interactive)
  (let ((inhibit-read-only t))
    (message "Applying forj theme...")
    (when (featurep 'forj-theme)
      (forj-apply-theme))
    (message "Re-highlighting buffer...")
    (forj-highlight-code-blocks)
    (message "Highlighting fix complete. Check if `code` appears blue now.")))

;;;###autoload
(defun forj-fix-conversation-highlighting ()
  "Fix highlighting in the conversation buffer specifically."
  (interactive)
  (if (and (boundp 'forj-conversation-buffer)
           (get-buffer forj-conversation-buffer))
      (with-current-buffer forj-conversation-buffer
        (let ((inhibit-read-only t))
          (message "Fixing conversation buffer highlighting...")
          
          ;; Apply theme first
          (when (featurep 'forj-theme)
            (forj-apply-theme))
          
          ;; Remove all existing text properties first
          (remove-text-properties (point-min) (point-max) '(face nil))
          
          ;; Apply fresh highlighting
          (forj-highlight-code-blocks)
          
          (message "Conversation buffer highlighting fixed!")))
    (message "No conversation buffer found. Run M-x forj-prompt first.")))

(defun forj-highlight-after-change (beg end len)
  "Re-highlight code blocks after buffer changes."
  (when (> (- end beg) 0)
    (let ((inhibit-read-only t))
      (save-excursion
        ;; Look for code blocks in the changed region
        (goto-char (max (point-min) (- beg 100)))
        (let ((region-end (min (point-max) (+ end 100))))
          (while (re-search-forward "```" region-end t)
            (beginning-of-line)
            (when (looking-at "\\s-*```")
              (forj-highlight-code-blocks))
            (forward-line 1)))))))

(provide 'forj-syntax-highlight)
;;; forj-syntax-highlight.el ends here