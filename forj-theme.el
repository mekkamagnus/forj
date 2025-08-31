;;; forj-theme.el --- Color scheme and visual design for forj.el -*- lexical-binding: t -*-

;;; Commentary:
;; Color scheme and visual design system for forj.el interface.
;; Provides custom faces, theme detection, and visual consistency across
;; all forj UI components. Supports light/dark themes and popular Emacs themes.
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'cl-lib)
(require 'color)

;;; Customization Group

(defgroup forj-faces nil
  "Faces for forj.el interface."
  :group 'forj
  :prefix "forj-")

(defgroup forj-theme nil
  "Theme configuration for forj.el interface."
  :group 'forj
  :prefix "forj-theme-")

;;; Core Interface Faces

(defface forj-header-face
  '((t :inherit font-lock-function-name-face :height 1.3 :weight bold))
  "Face for forj main headers and titles."
  :group 'forj-faces)

(defface forj-subheader-face
  '((t :inherit font-lock-variable-name-face :height 1.1 :weight bold))
  "Face for forj subheaders and section titles."
  :group 'forj-faces)

(defface forj-separator-face
  '((((background light)) :foreground "#6b7280" :weight bold)
    (((background dark))  :foreground "#9ca3af" :weight bold)
    (t :inherit font-lock-comment-face :weight bold))
  "Face for forj separators and dividers."
  :group 'forj-faces)

;;; Conversation Interface Faces

(defface forj-user-input-face
  '((((background light)) :foreground "#2563eb" :weight bold)
    (((background dark))  :foreground "#60a5fa" :weight bold)
    (t :foreground "blue" :weight bold))
  "Face for user input in conversation."
  :group 'forj-faces)

(defface forj-ai-response-face
  '((((background light)) :foreground "#16a34a")
    (((background dark))  :foreground "#4ade80")
    (t :foreground "green"))
  "Face for AI responses."
  :group 'forj-faces)

(defface forj-system-message-face
  '((((background light)) :foreground "#7c3aed" :slant italic)
    (((background dark))  :foreground "#a78bfa" :slant italic)
    (t :foreground "purple" :slant italic))
  "Face for system messages."
  :group 'forj-faces)

(defface forj-timestamp-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for timestamps in conversation."
  :group 'forj-faces)

;;; Status and Activity Faces

(defface forj-success-face
  '((((background light)) :foreground "#16a34a" :weight bold)
    (((background dark))  :foreground "#22c55e" :weight bold)
    (t :foreground "green" :weight bold))
  "Face for success indicators and messages."
  :group 'forj-faces)

(defface forj-error-face
  '((((background light)) :foreground "#dc2626" :weight bold)
    (((background dark))  :foreground "#ef4444" :weight bold)
    (t :foreground "red" :weight bold))
  "Face for error messages and indicators."
  :group 'forj-faces)

(defface forj-warning-face
  '((((background light)) :foreground "#d97706" :weight bold)
    (((background dark))  :foreground "#f59e0b" :weight bold)
    (t :foreground "orange" :weight bold))
  "Face for warning messages and indicators."
  :group 'forj-faces)

(defface forj-info-face
  '((((background light)) :foreground "#0284c7")
    (((background dark))  :foreground "#38bdf8")
    (t :foreground "cyan"))
  "Face for info messages and indicators."
  :group 'forj-faces)

(defface forj-activity-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for activity status indicators."
  :group 'forj-faces)

;;; Interactive Element Faces

(defface forj-button-face
  '((((background light)) :background "#e5e7eb" :foreground "#1f2937" :box (:line-width 1 :color "#d1d5db"))
    (((background dark))  :background "#374151" :foreground "#f9fafb" :box (:line-width 1 :color "#6b7280"))
    (t :inherit custom-button))
  "Face for forj buttons."
  :group 'forj-faces)

(defface forj-button-hover-face
  '((((background light)) :background "#d1d5db" :foreground "#111827")
    (((background dark))  :background "#4b5563" :foreground "#ffffff")
    (t :inherit custom-button-pressed))
  "Face for forj buttons when hovered."
  :group 'forj-faces)

(defface forj-link-face
  '((t :inherit link))
  "Face for forj links."
  :group 'forj-faces)

(defface forj-link-visited-face
  '((t :inherit link-visited))
  "Face for visited forj links."
  :group 'forj-faces)

;;; Code and Technical Faces

(defface forj-inline-code-face
  '((((background light)) :foreground "#2563eb" :inherit fixed-pitch)
    (((background dark))  :foreground "#60a5fa" :inherit fixed-pitch)
    (t :foreground "blue" :inherit fixed-pitch))
  "Face for inline code snippets."
  :group 'forj-faces)

(defface forj-code-block-face
  '((((background light)) :background "#f8f9fa" :extend t)
    (((background dark))  :background "#1a1a1a" :extend t)
    (t :background "gray90" :extend t))
  "Face for code blocks background."
  :group 'forj-faces)

(defface forj-file-name-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for file names and paths."
  :group 'forj-faces)

(defface forj-line-number-face
  '((t :inherit line-number))
  "Face for line numbers in code displays."
  :group 'forj-faces)

;;; Progress and Animation Faces

(defface forj-progress-face
  '((((background light)) :foreground "#2563eb")
    (((background dark))  :foreground "#60a5fa")
    (t :foreground "blue"))
  "Face for progress indicators."
  :group 'forj-faces)

(defface forj-spinner-face
  '((t :inherit forj-progress-face :weight bold))
  "Face for animated spinners."
  :group 'forj-faces)

(defface forj-progress-bar-face
  '((((background light)) :background "#dbeafe" :foreground "#1d4ed8")
    (((background dark))  :background "#1e3a8a" :foreground "#93c5fd")
    (t :background "lightblue" :foreground "blue"))
  "Face for progress bars."
  :group 'forj-faces)

;;; Theme Configuration

(defcustom forj-theme-style 'auto
  "Theme style for forj interface.
'auto: Automatically detect based on current Emacs theme
'light: Always use light theme colors
'dark: Always use dark theme colors"
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Light theme" light)
                 (const :tag "Dark theme" dark))
  :group 'forj-theme
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'forj-theme)
           (forj-apply-theme))))

(defcustom forj-auto-update-theme t
  "Whether to automatically update theme when Emacs theme changes."
  :type 'boolean
  :group 'forj-theme)

;;; Theme Detection

(defun forj-detect-dark-theme ()
  "Detect if current Emacs theme is dark.
Returns t if using a dark theme, nil otherwise."
  (let ((bg-color (face-background 'default nil 'default)))
    (when bg-color
      (condition-case nil
          (let ((rgb (color-name-to-rgb bg-color)))
            (when rgb
              ;; Calculate brightness using standard luminance formula
              (let ((brightness (+ (* 0.299 (nth 0 rgb))
                                  (* 0.587 (nth 1 rgb))
                                  (* 0.114 (nth 2 rgb)))))
                (< brightness 0.5))))
        (error nil)))))

(defun forj-detect-theme-type ()
  "Detect current theme type based on configuration and environment."
  (pcase forj-theme-style
    ('light 'light)
    ('dark 'dark)
    ('auto (if (forj-detect-dark-theme) 'dark 'light))
    (_ 'light))) ; Default to light if unknown

(defun forj-get-theme-color (light-color dark-color &optional fallback)
  "Get appropriate color for current theme.
Returns LIGHT-COLOR for light themes, DARK-COLOR for dark themes.
FALLBACK is used if theme detection fails."
  (let ((theme-type (forj-detect-theme-type)))
    (pcase theme-type
      ('light light-color)
      ('dark dark-color)
      (_ (or fallback light-color)))))

;;; Color Utilities

(defun forj-lighten-color (color percent)
  "Lighten COLOR by PERCENT (0.0 to 1.0)."
  (condition-case nil
      (let ((rgb (color-name-to-rgb color)))
        (when rgb
          (color-rgb-to-hex
           (+ (nth 0 rgb) (* percent (- 1.0 (nth 0 rgb))))
           (+ (nth 1 rgb) (* percent (- 1.0 (nth 1 rgb))))
           (+ (nth 2 rgb) (* percent (- 1.0 (nth 2 rgb)))))))
    (error color)))

(defun forj-darken-color (color percent)
  "Darken COLOR by PERCENT (0.0 to 1.0)."
  (condition-case nil
      (let ((rgb (color-name-to-rgb color)))
        (when rgb
          (color-rgb-to-hex
           (* (nth 0 rgb) (- 1.0 percent))
           (* (nth 1 rgb) (- 1.0 percent))
           (* (nth 2 rgb) (- 1.0 percent)))))
    (error color)))

(defun forj-blend-colors (color1 color2 percent)
  "Blend COLOR1 and COLOR2 by PERCENT (0.0 = color1, 1.0 = color2)."
  (condition-case nil
      (let ((rgb1 (color-name-to-rgb color1))
            (rgb2 (color-name-to-rgb color2)))
        (when (and rgb1 rgb2)
          (color-rgb-to-hex
           (+ (* (- 1.0 percent) (nth 0 rgb1)) (* percent (nth 0 rgb2)))
           (+ (* (- 1.0 percent) (nth 1 rgb1)) (* percent (nth 1 rgb2)))
           (+ (* (- 1.0 percent) (nth 2 rgb1)) (* percent (nth 2 rgb2))))))
    (error color1)))

;;; Theme Application

;;;###autoload
(defun forj-apply-theme (&optional theme-type)
  "Apply forj theme colors based on THEME-TYPE or current detection."
  (interactive)
  (let ((theme (or theme-type (forj-detect-theme-type))))
    (pcase theme
      ('light (forj-apply-light-theme))
      ('dark (forj-apply-dark-theme))
      (_ (forj-apply-light-theme)))))

(defun forj-apply-light-theme ()
  "Apply light theme colors to forj faces."
  (interactive)
  (set-face-attribute 'forj-user-input-face nil
                      :foreground "#2563eb" :weight 'bold)
  (set-face-attribute 'forj-ai-response-face nil
                      :foreground "#16a34a")
  (set-face-attribute 'forj-system-message-face nil
                      :foreground "#7c3aed" :slant 'italic)
  (set-face-attribute 'forj-success-face nil
                      :foreground "#16a34a" :weight 'bold)
  (set-face-attribute 'forj-error-face nil
                      :foreground "#dc2626" :weight 'bold)
  (set-face-attribute 'forj-warning-face nil
                      :foreground "#d97706" :weight 'bold)
  (set-face-attribute 'forj-info-face nil
                      :foreground "#0284c7")
  (set-face-attribute 'forj-button-face nil
                      :background "#e5e7eb" :foreground "#1f2937"
                      :box '(:line-width 1 :color "#d1d5db"))
  (set-face-attribute 'forj-button-hover-face nil
                      :background "#d1d5db" :foreground "#111827")
  (set-face-attribute 'forj-inline-code-face nil
                      :background 'unspecified :foreground "#2563eb")
  (set-face-attribute 'forj-code-block-face nil
                      :background "#f8f9fa" :extend t)
  (set-face-attribute 'forj-progress-face nil
                      :foreground "#2563eb")
  (set-face-attribute 'forj-progress-bar-face nil
                      :background "#dbeafe" :foreground "#1d4ed8")
  (set-face-attribute 'forj-separator-face nil
                      :foreground "#6b7280" :weight 'bold)
  
  (message "Forj light theme applied"))

(defun forj-apply-dark-theme ()
  "Apply dark theme colors to forj faces."
  (interactive)
  (set-face-attribute 'forj-user-input-face nil
                      :foreground "#60a5fa" :weight 'bold)
  (set-face-attribute 'forj-ai-response-face nil
                      :foreground "#4ade80")
  (set-face-attribute 'forj-system-message-face nil
                      :foreground "#a78bfa" :slant 'italic)
  (set-face-attribute 'forj-success-face nil
                      :foreground "#22c55e" :weight 'bold)
  (set-face-attribute 'forj-error-face nil
                      :foreground "#ef4444" :weight 'bold)
  (set-face-attribute 'forj-warning-face nil
                      :foreground "#f59e0b" :weight 'bold)
  (set-face-attribute 'forj-info-face nil
                      :foreground "#38bdf8")
  (set-face-attribute 'forj-button-face nil
                      :background "#374151" :foreground "#f9fafb"
                      :box '(:line-width 1 :color "#6b7280"))
  (set-face-attribute 'forj-button-hover-face nil
                      :background "#4b5563" :foreground "#ffffff")
  (set-face-attribute 'forj-inline-code-face nil
                      :background 'unspecified :foreground "#60a5fa")
  (set-face-attribute 'forj-code-block-face nil
                      :background "#1a1a1a" :extend t)
  (set-face-attribute 'forj-progress-face nil
                      :foreground "#60a5fa")
  (set-face-attribute 'forj-progress-bar-face nil
                      :background "#1e3a8a" :foreground "#93c5fd")
  (set-face-attribute 'forj-separator-face nil
                      :foreground "#9ca3af" :weight 'bold)
  
  (message "Forj dark theme applied"))

;;; Popular Theme Integration

(defvar forj-known-dark-themes
  '("doom-one" "doom-molokai" "doom-nova" "doom-palenight" "doom-vibrant"
    "doom-city-lights" "doom-dracula" "doom-gruvbox" "doom-material"
    "spacemacs-dark" "monokai" "zenburn" "material" "dracula" "gruvbox-dark"
    "solarized-dark" "tomorrow-night" "base16" "cyberpunk")
  "List of known dark theme names for better detection.")

(defvar forj-known-light-themes
  '("doom-one-light" "doom-opera-light" "spacemacs-light" "leuven"
    "adwaita" "tango" "dichromacy" "light-blue" "tsdh-light"
    "solarized-light" "gruvbox-light" "tomorrow" "github")
  "List of known light theme names for better detection.")

(defun forj-detect-theme-by-name ()
  "Detect theme type by checking known theme names."
  (let ((current-themes (mapcar #'symbol-name custom-enabled-themes)))
    (cond
     ((cl-some (lambda (theme)
                 (cl-some (lambda (dark) (string-match-p dark theme))
                          forj-known-dark-themes))
               current-themes)
      'dark)
     ((cl-some (lambda (theme)
                 (cl-some (lambda (light) (string-match-p light theme))
                          forj-known-light-themes))
               current-themes)
      'light)
     (t nil))))

;;; Theme Update Hooks

(defvar forj-theme-update-hook nil
  "Hook run after theme is updated.")

(defun forj-theme-change-hook (&rest _args)
  "Hook function called when theme changes."
  (when forj-auto-update-theme
    (run-with-timer 0.1 nil #'forj-update-theme-after-change)))

(defun forj-update-theme-after-change ()
  "Update forj theme after Emacs theme change."
  (forj-apply-theme)
  (run-hooks 'forj-theme-update-hook))

;;; Interactive Commands

;;;###autoload
(defun forj-toggle-theme ()
  "Toggle between light and dark forj theme."
  (interactive)
  (let ((current-theme (forj-detect-theme-type)))
    (pcase current-theme
      ('light (progn
                (setq forj-theme-style 'dark)
                (forj-apply-dark-theme)))
      ('dark (progn
               (setq forj-theme-style 'light)
               (forj-apply-light-theme)))
      (_ (progn
           (setq forj-theme-style 'light)
           (forj-apply-light-theme))))))

;;;###autoload
(defun forj-reset-theme ()
  "Reset forj theme to auto-detection mode."
  (interactive)
  (setq forj-theme-style 'auto)
  (forj-apply-theme)
  (message "Forj theme reset to auto-detection"))

;;;###autoload
(defun forj-customize-faces ()
  "Open customization interface for forj faces."
  (interactive)
  (customize-group 'forj-faces))

;;;###autoload
(defun forj-show-theme-info ()
  "Show information about current forj theme configuration."
  (interactive)
  (let ((theme-type (forj-detect-theme-type))
        (is-dark (forj-detect-dark-theme))
        (theme-by-name (forj-detect-theme-by-name)))
    (message "Forj theme: %s (detected: %s, by-name: %s, setting: %s)"
             theme-type
             (if is-dark "dark" "light")
             (or theme-by-name "unknown")
             forj-theme-style)))

;;; Setup and Initialization

;;;###autoload
(defun forj-setup-theme ()
  "Set up forj theme system."
  (interactive)
  ;; Apply initial theme
  (forj-apply-theme)
  
  ;; Set up theme change monitoring
  (when forj-auto-update-theme
    (advice-add 'load-theme :after #'forj-theme-change-hook)
    (advice-add 'disable-theme :after #'forj-theme-change-hook)
    (advice-add 'enable-theme :after #'forj-theme-change-hook))
  
  (message "Forj theme system initialized"))

;;;###autoload
(defun forj-cleanup-theme ()
  "Clean up forj theme system."
  (interactive)
  (advice-remove 'load-theme #'forj-theme-change-hook)
  (advice-remove 'disable-theme #'forj-theme-change-hook)
  (advice-remove 'enable-theme #'forj-theme-change-hook)
  (message "Forj theme system cleaned up"))

;;; Face Utilities

(defun forj-copy-face-attributes (from-face to-face &optional frame)
  "Copy all attributes from FROM-FACE to TO-FACE for FRAME."
  (let ((attributes (face-all-attributes from-face frame)))
    (dolist (attr attributes)
      (let ((attr-name (car attr))
            (attr-value (cdr attr)))
        (unless (eq attr-value 'unspecified)
          (set-face-attribute to-face frame attr-name attr-value))))))

(defun forj-face-exists-p (face)
  "Check if FACE exists as a defined face."
  (facep face))

(defun forj-get-face-color (face attribute &optional fallback)
  "Get color ATTRIBUTE from FACE with optional FALLBACK."
  (let ((color (face-attribute face attribute nil 'default)))
    (if (eq color 'unspecified)
        fallback
      color)))

;;; Integration with Existing Systems

(defun forj-integrate-with-conversation-buffer ()
  "Integrate theme system with conversation buffer."
  (when (and (boundp 'forj-conversation-buffer)
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      ;; Apply theme to existing content if needed
      (when (fboundp 'forj-render-markdown-buffer)
        (forj-render-markdown-buffer))
      (when (fboundp 'forj-highlight-buffer)
        (forj-highlight-buffer)))))

;; Hook into theme update
(add-hook 'forj-theme-update-hook #'forj-integrate-with-conversation-buffer)

(provide 'forj-theme)
;;; forj-theme.el ends here