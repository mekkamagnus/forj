;;; forj-markdown.el --- Rich markdown rendering for forj conversation buffer -*- lexical-binding: t -*-

;;; Commentary:
;; Rich markdown rendering system for forj conversation buffer.
;; Provides comprehensive markdown formatting including headers, lists, emphasis,
;; links, tables, and code blocks for AI responses.
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'browse-url)

;; Optional dependency
(require 'markdown-mode nil t)

;;; Customization Group

(defgroup forj-markdown nil
  "Markdown rendering for forj conversation buffer."
  :group 'forj
  :prefix "forj-markdown-")

;;; Faces for Markdown Elements

(defface forj-markdown-header1-face
  '((t :inherit font-lock-function-name-face :height 1.4 :weight bold))
  "Face for level 1 markdown headers (# Header)."
  :group 'forj-markdown)

(defface forj-markdown-header2-face
  '((t :inherit font-lock-function-name-face :height 1.3 :weight bold))
  "Face for level 2 markdown headers (## Header)."
  :group 'forj-markdown)

(defface forj-markdown-header3-face
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold))
  "Face for level 3 markdown headers (### Header)."
  :group 'forj-markdown)

(defface forj-markdown-header4-face
  '((t :inherit font-lock-function-name-face :height 1.1 :weight bold))
  "Face for level 4 markdown headers (#### Header)."
  :group 'forj-markdown)

(defface forj-markdown-bold-face
  '((t :inherit default :weight bold))
  "Face for bold markdown text (**bold**)."
  :group 'forj-markdown)

(defface forj-markdown-italic-face
  '((t :inherit default :slant italic))
  "Face for italic markdown text (*italic*)."
  :group 'forj-markdown)

(defface forj-markdown-strike-face
  '((t :inherit default :strike-through t))
  "Face for strikethrough markdown text (~~strike~~)."
  :group 'forj-markdown)

(defface forj-markdown-list-face
  '((t :inherit font-lock-builtin-face))
  "Face for markdown list markers."
  :group 'forj-markdown)

(defface forj-markdown-quote-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for markdown blockquotes (> quote)."
  :group 'forj-markdown)

(defface forj-markdown-link-face
  '((t :inherit link))
  "Face for markdown links."
  :group 'forj-markdown)

(defface forj-markdown-url-face
  '((t :inherit font-lock-string-face :underline t))
  "Face for markdown URLs."
  :group 'forj-markdown)

(defface forj-markdown-table-face
  '((t :inherit default :background "#f9f9f9"))
  "Face for markdown tables."
  :group 'forj-markdown)

(defface forj-markdown-hr-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for markdown horizontal rules (---)."
  :group 'forj-markdown)

;;; Configuration

(defcustom forj-markdown-enable-links t
  "Whether to make markdown links clickable."
  :type 'boolean
  :group 'forj-markdown)

(defcustom forj-markdown-enable-tables t
  "Whether to format markdown tables."
  :type 'boolean
  :group 'forj-markdown)

(defcustom forj-markdown-link-browser-function nil
  "Function to use for opening markdown links.
If nil, uses `browse-url-browser-function'."
  :type '(choice (const :tag "Default browser" nil)
                 function)
  :group 'forj-markdown)

;;; Core Rendering Functions

;;;###autoload
(defun forj-render-markdown-buffer ()
  "Apply markdown formatting to the entire current buffer."
  (interactive)
  (when (buffer-live-p (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (forj-render-markdown-region (point-min) (point-max))))))

(defun forj-render-markdown-region (start end)
  "Apply markdown formatting to region between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      
      ;; Apply markdown formatting in order
      (forj-render-headers)
      (forj-render-emphasis)
      (forj-render-lists)
      (forj-render-quotes)
      (forj-render-horizontal-rules)
      (when forj-markdown-enable-links
        (forj-render-links))
      (when forj-markdown-enable-tables
        (forj-render-tables)))))

;;; Header Rendering

(defun forj-render-headers ()
  "Render markdown headers (# Header, ## Header, etc.)."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" nil t)
    (let ((level (length (match-string 1)))
          (text (match-string 2))
          (start (match-beginning 0))
          (end (match-end 0))
          (face (forj-get-header-face level)))
      
      (when face
        (add-text-properties start end
                           `(face ,face
                            forj-markdown-element header
                            forj-header-level ,level
                            rear-nonsticky (face forj-markdown-element forj-header-level)))))))

(defun forj-get-header-face (level)
  "Get the appropriate face for header LEVEL."
  (pcase level
    (1 'forj-markdown-header1-face)
    (2 'forj-markdown-header2-face)
    (3 'forj-markdown-header3-face)
    (4 'forj-markdown-header4-face)
    (_ 'forj-markdown-header4-face))) ; Default for h5, h6, etc.

;;; Emphasis Rendering

(defun forj-render-emphasis ()
  "Render markdown emphasis (**bold**, *italic*, ~~strike~~)."
  (goto-char (point-min))
  
  ;; Bold (**text** or __text__)
  (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*\\|__\\([^_\n]+\\)__" nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (text (or (match-string 1) (match-string 2))))
      (add-text-properties start end
                         '(face forj-markdown-bold-face
                           forj-markdown-element emphasis
                           rear-nonsticky (face forj-markdown-element)))))
  
  ;; Italic (*text* or _text_) - but not inside bold
  (goto-char (point-min))
  (while (re-search-forward "\\(?:^\\|[^*]\\)\\(\\*\\([^*\n]+\\)\\*\\|_\\([^_\n]+\\)_\\)" nil t)
    (unless (get-text-property (match-beginning 1) 'forj-markdown-element)
      (let ((start (match-beginning 1))
            (end (match-end 1))
            (text (or (match-string 2) (match-string 3))))
        (add-text-properties start end
                           '(face forj-markdown-italic-face
                             forj-markdown-element emphasis
                             rear-nonsticky (face forj-markdown-element))))))
  
  ;; Strikethrough (~~text~~)
  (goto-char (point-min))
  (while (re-search-forward "~~\\([^~\n]+\\)~~" nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (add-text-properties start end
                         '(face forj-markdown-strike-face
                           forj-markdown-element emphasis
                           rear-nonsticky (face forj-markdown-element))))))

;;; List Rendering

(defun forj-render-lists ()
  "Render markdown lists (- item, 1. item)."
  (goto-char (point-min))
  
  ;; Unordered lists (-, *, +)
  (while (re-search-forward "^\\s-*\\([-*+]\\)\\s-+" nil t)
    (let ((start (match-beginning 1))
          (end (match-end 1)))
      (add-text-properties start end
                         '(face forj-markdown-list-face
                           forj-markdown-element list
                           rear-nonsticky (face forj-markdown-element)))))
  
  ;; Ordered lists (1., 2., etc.)
  (goto-char (point-min))
  (while (re-search-forward "^\\s-*\\([0-9]+\\.\\)\\s-+" nil t)
    (let ((start (match-beginning 1))
          (end (match-end 1)))
      (add-text-properties start end
                         '(face forj-markdown-list-face
                           forj-markdown-element list
                           rear-nonsticky (face forj-markdown-element))))))

;;; Quote Rendering

(defun forj-render-quotes ()
  "Render markdown blockquotes (> quote)."
  (goto-char (point-min))
  (while (re-search-forward "^\\s-*>\\s-*\\(.+\\)$" nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (add-text-properties start end
                         '(face forj-markdown-quote-face
                           forj-markdown-element quote
                           rear-nonsticky (face forj-markdown-element))))))

;;; Horizontal Rule Rendering

(defun forj-render-horizontal-rules ()
  "Render markdown horizontal rules (--- or ***)."
  (goto-char (point-min))
  (while (re-search-forward "^\\s-*\\(---+\\|\\*\\*\\*+\\|___+\\)\\s-*$" nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (add-text-properties start end
                         '(face forj-markdown-hr-face
                           forj-markdown-element hr
                           rear-nonsticky (face forj-markdown-element))))))

;;; Link Rendering

(defun forj-render-links ()
  "Render markdown links [text](url) and make them clickable."
  (goto-char (point-min))
  (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
    (let ((link-text (match-string 1))
          (url (match-string 2))
          (start (match-beginning 0))
          (end (match-end 0)))
      
      ;; Create clickable link
      (make-button start end
                   'type 'forj-markdown-link
                   'forj-url url
                   'forj-link-text link-text
                   'face 'forj-markdown-link-face
                   'help-echo (format "Visit: %s" url)
                   'follow-link t))))

;; Define button type for markdown links
(define-button-type 'forj-markdown-link
  'action #'forj-markdown-follow-link
  'face 'forj-markdown-link-face
  'follow-link t)

(defun forj-markdown-follow-link (button)
  "Follow the URL associated with markdown link BUTTON."
  (let ((url (button-get button 'forj-url)))
    (when url
      (if forj-markdown-link-browser-function
          (funcall forj-markdown-link-browser-function url)
        (browse-url url)))))

;;; Table Rendering

(defun forj-render-tables ()
  "Render markdown tables with basic formatting."
  (goto-char (point-min))
  (while (re-search-forward "^\\s-*|" nil t)
    (beginning-of-line)
    (when (forj-at-table-p)
      (let ((table-start (point))
            (table-end (forj-find-table-end)))
        (forj-format-table-region table-start table-end)
        (goto-char table-end)))))

(defun forj-at-table-p ()
  "Check if point is at the start of a markdown table."
  (and (looking-at "^\\s-*|.*|\\s-*$")
       (save-excursion
         (forward-line 1)
         (or (looking-at "^\\s-*|.*|\\s-*$")    ; Another table row
             (looking-at "^\\s-*|[-:| ]+|\\s-*$"))))) ; Header separator

(defun forj-find-table-end ()
  "Find the end of the markdown table starting at point."
  (save-excursion
    (while (and (not (eobp))
                (looking-at "^\\s-*|.*|\\s-*$\\|^\\s-*|[-:| ]+|\\s-*$\\|^\\s-*$"))
      (forward-line 1))
    (point)))

(defun forj-format-table-region (start end)
  "Apply table formatting to region between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at "^\\s-*|.*|\\s-*$")
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (add-text-properties line-start line-end
                             '(face forj-markdown-table-face
                               forj-markdown-element table
                               rear-nonsticky (face forj-markdown-element)))))
      (forward-line 1))))

;;; Auto-rendering Setup

(defcustom forj-markdown-auto-render t
  "Whether to automatically render markdown in conversation buffer."
  :type 'boolean
  :group 'forj-markdown)

;;;###autoload
(defun forj-setup-conversation-markdown ()
  "Set up markdown rendering for forj conversation buffer."
  (when (and (boundp 'forj-conversation-buffer)
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      (when forj-markdown-auto-render
        (add-hook 'after-change-functions #'forj-markdown-after-change nil t)
        (let ((inhibit-read-only t))
          (forj-render-markdown-buffer))))))

(defun forj-markdown-after-change (beg end len)
  "Re-render markdown after buffer changes between BEG and END.
LEN is the length of the deleted text."
  (when (> (- end beg) 0)
    (let ((inhibit-read-only t))
      (save-excursion
        ;; Expand region to include potential markdown elements
        (goto-char beg)
        (beginning-of-line)
        (let ((region-start (point)))
          (goto-char end)
          (forward-line 2) ; Include a bit extra for multiline elements
          (let ((region-end (min (point-max) (point))))
            (forj-clear-markdown-formatting region-start region-end)
            (forj-render-markdown-region region-start region-end)))))))

(defun forj-clear-markdown-formatting (&optional start end)
  "Clear markdown formatting in region or entire buffer."
  (interactive)
  (let ((beg (or start (point-min)))
        (finish (or end (point-max))))
    (remove-text-properties beg finish
                          '(face nil
                            forj-markdown-element nil
                            forj-header-level nil
                            forj-url nil
                            forj-link-text nil
                            button nil
                            category nil
                            rear-nonsticky nil))))

;;; Interactive Commands

;;;###autoload
(defun forj-markdown-toggle-rendering ()
  "Toggle markdown rendering in current buffer."
  (interactive)
  (if (local-variable-p 'forj-markdown-auto-render)
      (if forj-markdown-auto-render
          (progn
            (setq-local forj-markdown-auto-render nil)
            (forj-clear-markdown-formatting)
            (remove-hook 'after-change-functions #'forj-markdown-after-change t)
            (message "Forj markdown rendering disabled"))
        (progn
          (setq-local forj-markdown-auto-render t)
          (forj-render-markdown-buffer)
          (add-hook 'after-change-functions #'forj-markdown-after-change nil t)
          (message "Forj markdown rendering enabled")))
    (progn
      (setq-local forj-markdown-auto-render t)
      (forj-render-markdown-buffer)
      (add-hook 'after-change-functions #'forj-markdown-after-change nil t)
      (message "Forj markdown rendering enabled"))))

;;;###autoload
(defun forj-markdown-preview-at-point ()
  "Show information about markdown element at point."
  (interactive)
  (let ((element (get-text-property (point) 'forj-markdown-element)))
    (if element
        (let ((level (get-text-property (point) 'forj-header-level))
              (url (get-text-property (point) 'forj-url)))
          (message "Forj markdown: %s%s%s"
                  element
                  (if level (format " (level %s)" level) "")
                  (if url (format " -> %s" url) "")))
      (message "No forj markdown element at point"))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode forj-markdown-mode
  "Minor mode for markdown rendering in forj buffers."
  :init-value nil
  :lighter " FM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m r") #'forj-render-markdown-buffer)
            (define-key map (kbd "C-c m t") #'forj-markdown-toggle-rendering)
            (define-key map (kbd "C-c m p") #'forj-markdown-preview-at-point)
            map)
  :group 'forj-markdown
  (if forj-markdown-mode
      (progn
        (when forj-markdown-auto-render
          (forj-render-markdown-buffer)
          (add-hook 'after-change-functions #'forj-markdown-after-change nil t)))
    (progn
      (forj-clear-markdown-formatting)
      (remove-hook 'after-change-functions #'forj-markdown-after-change t))))

;;; Utility Functions

(defun forj-markdown-get-element-at-point ()
  "Get the markdown element type at point."
  (get-text-property (point) 'forj-markdown-element))

(defun forj-markdown-goto-next-header (&optional level)
  "Go to the next header, optionally of specific LEVEL."
  (interactive "P")
  (let ((found nil)
        (target-level (when level (prefix-numeric-value level))))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (let ((element (get-text-property (point) 'forj-markdown-element))
              (header-level (get-text-property (point) 'forj-header-level)))
          (when (and (eq element 'header)
                     (or (not target-level)
                         (= header-level target-level)))
            (setq found (point))))
        (forward-line 1)))
    (when found
      (goto-char found)
      (beginning-of-line))))

(defun forj-markdown-goto-previous-header (&optional level)
  "Go to the previous header, optionally of specific LEVEL."
  (interactive "P")
  (let ((found nil)
        (target-level (when level (prefix-numeric-value level))))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (let ((element (get-text-property (point) 'forj-markdown-element))
              (header-level (get-text-property (point) 'forj-header-level)))
          (when (and (eq element 'header)
                     (or (not target-level)
                         (= header-level target-level)))
            (setq found (point))))
        (forward-line -1)))
    (when found
      (goto-char found)
      (beginning-of-line))))

(provide 'forj-markdown)
;;; forj-markdown.el ends here