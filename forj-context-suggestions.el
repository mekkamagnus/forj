;;; forj-context-suggestions.el --- Smart Context Suggestion Engine -*- lexical-binding: t -*-

;;; Commentary:
;; Smart context suggestion engine that analyzes user prompts and current Emacs state
;; to automatically suggest relevant context sources for AI interactions.
;;
;; This implements the Smart Context Suggestions from Specification 002.

;;; Code:

(require 'cl-lib)

;; NOTE: Dependencies are managed by the central forj.el loader.
;; Do not use require/load-file directly in individual modules.

;;; Prompt Analysis Engine

(defun forj-analyze-prompt-for-context (prompt)
  "Analyze prompt and suggest relevant context sources.
PROMPT is the user's input string.
Returns a list of context suggestion plists with :type, :confidence, and :reason."
  (let ((context-suggestions '())
        (prompt-lower (downcase prompt)))
    
    ;; File mention analysis
    (when (string-match-p "\\b\\w+\\.\\w+\\b" prompt)
      (let ((files (forj-extract-file-references prompt)))
        (dolist (file files)
          (when (file-exists-p file)
            (push `(:type file 
                    :path ,file
                    :confidence 0.95
                    :reason ,(format "File mentioned in prompt: %s" file))
                  context-suggestions)))))
    
    ;; Buffer mention analysis
    (when (or (string-match-p "\\bcurrent buffer\\b" prompt-lower)
              (string-match-p "\\bthis buffer\\b" prompt-lower)
              (string-match-p "\\bactive buffer\\b" prompt-lower))
      (push `(:type buffer 
              :buffer ,(current-buffer)
              :confidence 0.9
              :reason "Current buffer mentioned in prompt")
            context-suggestions))
    
    ;; Error/debugging keywords
    (when (string-match-p "\\b\\(error\\|debug\\|fix\\|broken\\|bug\\|issue\\|problem\\)\\b" prompt-lower)
      (push `(:type compilation 
              :confidence 0.8
              :reason "Error/debugging keywords detected")
            context-suggestions))
    
    ;; Code review keywords
    (when (string-match-p "\\b\\(review\\|check\\|analyze\\|examine\\|look at\\)\\b" prompt-lower)
      (push `(:type buffer 
              :buffer ,(current-buffer)
              :confidence 0.7
              :reason "Code review keywords detected")
            context-suggestions))
    
    ;; Project-wide keywords
    (when (string-match-p "\\b\\(project\\|codebase\\|all files\\|entire\\)\\b" prompt-lower)
      (push `(:type project 
              :root ,(or (forj-find-project-root) default-directory)
              :confidence 0.8
              :reason "Project-wide scope detected")
            context-suggestions))
    
    ;; Mode-specific analysis
    (let ((mode-suggestions (forj-analyze-prompt-for-mode-context prompt prompt-lower)))
      (setq context-suggestions (append context-suggestions mode-suggestions)))
    
    ;; Function/variable references
    (let ((symbol-suggestions (forj-analyze-prompt-for-symbols prompt)))
      (setq context-suggestions (append context-suggestions symbol-suggestions)))
    
    ;; Sort by confidence (highest first)
    (sort context-suggestions 
          (lambda (a b) (> (plist-get a :confidence) 
                          (plist-get b :confidence))))))

(defun forj-extract-file-references (prompt)
  "Extract file references from PROMPT.
Returns a list of file paths found in the prompt."
  (let ((files '())
        (words (split-string prompt))
        (current-dir default-directory))
    
    (dolist (word words)
      ;; Remove common punctuation
      (setq word (replace-regexp-in-string "[\"',.:;!?()]" "" word))
      
      ;; Check if it looks like a filename
      (when (and (string-match-p "\\." word)
                 (> (length word) 3))
        (let ((potential-file (expand-file-name word current-dir)))
          (when (file-exists-p potential-file)
            (push potential-file files)))
        
        ;; Also check in common directories
        (dolist (dir '("." "./src" "./lib" "./test"))
          (let ((potential-file (expand-file-name word (expand-file-name dir current-dir))))
            (when (and (file-exists-p potential-file)
                       (not (member potential-file files)))
              (push potential-file files))))))
    
    (nreverse files)))

(defun forj-analyze-prompt-for-mode-context (prompt prompt-lower)
  "Analyze PROMPT for mode-specific context hints.
PROMPT is the original prompt, PROMPT-LOWER is the lowercase version.
Returns a list of mode-specific context suggestions."
  (let ((suggestions '()))
    
    ;; Emacs Lisp specific
    (when (or (string-match-p "\\b\\(defun\\|defvar\\|defcustom\\|elisp\\|emacs lisp\\)\\b" prompt-lower)
              (string-match-p "\\.el\\b" prompt))
      (push `(:type buffer 
              :buffer ,(current-buffer)
              :confidence 0.8
              :reason "Emacs Lisp development context")
            suggestions)
      
      ;; Look for .el files in project
      (let ((elisp-files (forj-find-files-by-extension "el")))
        (when elisp-files
          (push `(:type project 
                  :root ,(or (forj-find-project-root) default-directory)
                  :patterns ("\\.el$")
                  :confidence 0.7
                  :reason "Emacs Lisp project files")
                suggestions))))
    
    ;; Configuration/customization
    (when (string-match-p "\\b\\(config\\|configuration\\|customize\\|setting\\)\\b" prompt-lower)
      (let ((config-files (forj-find-config-files)))
        (dolist (config-file config-files)
          (push `(:type file 
                  :path ,config-file
                  :confidence 0.6
                  :reason "Configuration file")
                suggestions))))
    
    ;; Documentation
    (when (string-match-p "\\b\\(readme\\|documentation\\|docs\\|manual\\|guide\\)\\b" prompt-lower)
      (let ((doc-files (forj-find-documentation-files)))
        (dolist (doc-file doc-files)
          (push `(:type file 
                  :path ,doc-file
                  :confidence 0.6
                  :reason "Documentation file")
                suggestions))))
    
    suggestions))

(defun forj-analyze-prompt-for-symbols (prompt)
  "Analyze PROMPT for function/variable symbol references.
Returns context suggestions based on symbol analysis."
  (let ((suggestions '())
        (symbols (forj-extract-symbol-references prompt)))
    
    (dolist (symbol symbols)
      (when (fboundp symbol)
        ;; Function is defined, suggest buffer containing definition
        (let ((definition-buffer (forj-find-symbol-definition-buffer symbol)))
          (when definition-buffer
            (push `(:type buffer 
                    :buffer ,definition-buffer
                    :confidence 0.7
                    :reason ,(format "Contains definition of function %s" symbol))
                  suggestions))))
      
      (when (boundp symbol)
        ;; Variable is defined
        (let ((definition-buffer (forj-find-symbol-definition-buffer symbol)))
          (when definition-buffer
            (push `(:type buffer 
                    :buffer ,definition-buffer
                    :confidence 0.6
                    :reason ,(format "Contains definition of variable %s" symbol))
                  suggestions)))))
    
    suggestions))

(defun forj-extract-symbol-references (prompt)
  "Extract potential Emacs Lisp symbols from PROMPT."
  (let ((symbols '()))
    ;; Look for words that could be function/variable names
    ;; Match words with hyphens (common in Emacs Lisp)
    (let ((case-fold-search nil))
      (dolist (match (string-match-all "\\b[a-zA-Z][a-zA-Z0-9-]*[a-zA-Z0-9]\\b" prompt))
        (let ((symbol-name (match-string 0 prompt)))
          ;; Only consider symbols that look like Emacs Lisp conventions
          (when (and (string-match-p "-" symbol-name)
                     (> (length symbol-name) 3))
            (push (intern symbol-name) symbols)))))
    
    (delete-dups symbols)))

;;; Emacs State Analysis

(defun forj-analyze-emacs-state ()
  "Analyze current Emacs state and suggest relevant context.
Returns a plist with current state information."
  (list
   :active-buffer (current-buffer)
   :buffer-mode major-mode
   :buffer-file (buffer-file-name)
   :region-active (use-region-p)
   :region-content (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
   :compilation-buffer (get-buffer "*compilation*")
   :compilation-errors (when (get-buffer "*compilation*")
                        (forj-has-compilation-errors-p))
   :project-files (when (forj-find-project-root)
                    (length (forj-get-project-files)))
   :modified-buffers (length (forj-get-modified-buffers))
   :recent-files (when (boundp 'recentf-list)
                   (cl-subseq recentf-list 0 (min 5 (length recentf-list))))))

(defun forj-suggest-context-sources (&optional prompt)
  "Suggest relevant context sources based on prompt and Emacs state.
PROMPT is optional user input. If not provided, uses current Emacs state only.
Returns list of context source suggestions."
  (let ((suggestions '())
        (emacs-state (forj-analyze-emacs-state)))
    
    ;; Analyze prompt if provided
    (when prompt
      (setq suggestions (forj-analyze-prompt-for-context prompt)))
    
    ;; Analyze current buffer (always relevant)
    (when (plist-get emacs-state :buffer-file)
      (let ((existing-buffer-suggestion 
             (cl-find-if (lambda (s) (and (eq (plist-get s :type) 'buffer)
                                          (eq (plist-get s :buffer) (current-buffer))))
                        suggestions)))
        (unless existing-buffer-suggestion
          (push `(:type buffer
                  :buffer ,(current-buffer)
                  :confidence 0.9
                  :reason "Current active buffer")
                suggestions))))
    
    ;; Check for region selection
    (when (plist-get emacs-state :region-active)
      (push `(:type buffer
              :buffer ,(current-buffer)
              :region (,(region-beginning) . ,(region-end))
              :confidence 0.95
              :reason "Active region selection")
            suggestions))
    
    ;; Check for compilation buffer with errors
    (when (plist-get emacs-state :compilation-errors)
      (let ((existing-compilation-suggestion
             (cl-find-if (lambda (s) (eq (plist-get s :type) 'compilation))
                        suggestions)))
        (unless existing-compilation-suggestion
          (push `(:type compilation
                  :confidence 0.8
                  :reason "Recent compilation output with errors")
                suggestions))))
    
    ;; Suggest modified buffers
    (let ((modified-buffers (forj-get-modified-buffers)))
      (when (and modified-buffers (< (length modified-buffers) 5))
        (dolist (buffer modified-buffers)
          (unless (cl-find-if (lambda (s) (and (eq (plist-get s :type) 'buffer)
                                              (eq (plist-get s :buffer) buffer)))
                             suggestions)
            (push `(:type buffer
                    :buffer ,buffer
                    :confidence 0.6
                    :reason "Modified buffer")
                  suggestions)))))
    
    ;; Suggest recent files
    (when (plist-get emacs-state :recent-files)
      (dolist (file (cl-subseq (plist-get emacs-state :recent-files) 0 
                              (min 3 (length (plist-get emacs-state :recent-files)))))
        (when (file-exists-p file)
          (unless (cl-find-if (lambda (s) (and (eq (plist-get s :type) 'file)
                                              (string= (plist-get s :path) file)))
                             suggestions)
            (push `(:type file
                    :path ,file
                    :confidence 0.4
                    :reason "Recently accessed file")
                  suggestions)))))
    
    ;; Sort by confidence and limit results
    (let ((sorted-suggestions (sort suggestions 
                                   (lambda (a b) (> (plist-get a :confidence) 
                                                   (plist-get b :confidence))))))
      (cl-subseq sorted-suggestions 0 (min 10 (length sorted-suggestions))))))

;;; Utility Functions

(defun forj-find-project-root (&optional directory)
  "Find project root starting from DIRECTORY (or current directory)."
  (let ((dir (or directory default-directory)))
    (or (locate-dominating-file dir ".git")
        (locate-dominating-file dir "package.json")
        (locate-dominating-file dir "Makefile")
        (locate-dominating-file dir "README.md")
        (locate-dominating-file dir "README.org")
        dir)))

(defun forj-find-files-by-extension (extension &optional directory)
  "Find all files with EXTENSION in DIRECTORY (or current directory)."
  (let* ((dir (or directory default-directory))
         (pattern (concat "\\." extension "$"))
         (files '()))
    (dolist (file (directory-files-recursively dir pattern))
      (push file files))
    (nreverse files)))

(defun forj-find-config-files (&optional directory)
  "Find configuration files in DIRECTORY (or current directory)."
  (let* ((dir (or directory default-directory))
         (config-patterns '("config" "configuration" "\\.cfg" "\\.conf" "\\.ini"))
         (files '()))
    (dolist (pattern config-patterns)
      (dolist (file (directory-files dir t pattern))
        (when (file-regular-p file)
          (push file files))))
    (delete-dups files)))

(defun forj-find-documentation-files (&optional directory)
  "Find documentation files in DIRECTORY (or current directory)."
  (let* ((dir (or directory default-directory))
         (doc-patterns '("README" "readme" "MANUAL" "manual" "GUIDE" "guide" 
                        "INSTALL" "install" "CHANGELOG" "changelog"))
         (files '()))
    (dolist (pattern doc-patterns)
      (dolist (file (directory-files dir t pattern))
        (when (file-regular-p file)
          (push file files))))
    ;; Also look for .md, .org, .txt files in docs/ directory
    (let ((docs-dir (expand-file-name "docs" dir)))
      (when (file-directory-p docs-dir)
        (dolist (file (directory-files-recursively docs-dir "\\.(md|org|txt)$"))
          (push file files))))
    (delete-dups files)))

(defun forj-find-symbol-definition-buffer (symbol)
  "Find buffer containing definition of SYMBOL."
  (let ((location (find-function-noselect symbol t)))
    (when (and location (bufferp (car location)))
      (car location))))

(defun forj-get-modified-buffers ()
  "Get list of modified buffers."
  (cl-remove-if-not #'buffer-modified-p (buffer-list)))

(defun forj-has-compilation-errors-p ()
  "Check if compilation buffer has errors."
  (let ((compilation-buffer (get-buffer "*compilation*")))
    (when compilation-buffer
      (with-current-buffer compilation-buffer
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "error:" nil t))))))

(defun forj-get-project-files (&optional directory)
  "Get list of project files in DIRECTORY."
  (let ((project-root (forj-find-project-root directory)))
    (when project-root
      (if (fboundp 'forj-scan-directory-recursive)
          (mapcar (lambda (file-meta) (plist-get file-meta :path))
                  (forj-scan-directory-recursive project-root 3 50))
        (directory-files-recursively project-root "\\.(el|md|txt|org)$")))))

(defun string-match-all (regexp string)
  "Return all matches of REGEXP in STRING."
  (let ((matches '())
        (start 0))
    (while (string-match regexp string start)
      (push (match-string 0 string) matches)
      (setq start (match-end 0)))
    (nreverse matches)))

;;; Context Collection Interface

(defun forj-collect-context (&optional sources)
  "Collect context from specified SOURCES or auto-detect.
SOURCES is an optional list of source specifications.
If not provided, automatically suggests and uses relevant sources."
  (interactive)
  (let ((context-data '())
        (selected-sources (or sources (forj-suggest-context-sources))))
    
    (dolist (source selected-sources)
      (let ((context-item (condition-case err
                              (pcase (plist-get source :type)
                                ('buffer (forj-collect-buffer-context
                                         (plist-get source :buffer)
                                         (plist-get source :region)))
                                ('file (forj-collect-file-context
                                       (plist-get source :path)))
                                ('compilation (forj-collect-compilation-context
                                              (plist-get source :include-warnings)))
                                ('project (forj-collect-project-context
                                          (plist-get source :root)
                                          (plist-get source :patterns))))
                            (error 
                             (message "Warning: Failed to collect context from %s: %s" 
                                    (plist-get source :type)
                                    (error-message-string err))
                             nil))))
        (when context-item
          (push context-item context-data))))
    
    ;; Optimize context size
    (forj-optimize-context-size (nreverse context-data))))

(defun forj-optimize-context-size (context-list)
  "Optimize CONTEXT-LIST to fit within size constraints."
  (let ((total-size 0)
        (optimized '())
        (max-size forj-context-max-size))
    
    ;; Sort by relevance (keep highest relevance items)
    (setq context-list (sort context-list
                            (lambda (a b)
                              (> (plist-get a :relevance 0)
                                 (plist-get b :relevance 0)))))
    
    (dolist (ctx context-list)
      (let* ((content (plist-get ctx :content))
             (size (length content)))
        (when (< (+ total-size size) max-size)
          (push ctx optimized)
          (setq total-size (+ total-size size)))))
    
    (nreverse optimized)))

(provide 'forj-context-suggestions)
;;; forj-context-suggestions.el ends here