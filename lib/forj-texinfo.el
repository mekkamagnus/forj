;;; forj-texinfo.el --- Texinfo documentation generation for Forj -*- lexical-binding: t -*-

;;; Commentary:
;; Functions to automatically generate Texinfo documentation from
;; Emacs Lisp source code, particularly for API reference generation.

;;; Code:

(require 'cl-lib)
(require 'forj)

;; Forward declarations
(declare-function forj-scan-directory-recursive "forj" (&optional directory max-depth max-files))

;;; Configuration

(defcustom forj-texinfo-output-dir "docs/texinfo"
  "Directory for generated Texinfo files."
  :type 'string
  :group 'forj)

(defcustom forj-texinfo-api-file "api-reference.texi"
  "Filename for generated API reference documentation."
  :type 'string
  :group 'forj)

;;; Utility Functions

(defun forj-texinfo--escape-string (string)
  "Escape STRING for safe inclusion in Texinfo documents."
  (when string
    (replace-regexp-in-string
     "@" "@@"
     (replace-regexp-in-string
      "{" "@{"
      (replace-regexp-in-string
       "}" "@}"
       string)))))

(defun forj-texinfo--format-docstring (docstring)
  "Format DOCSTRING for Texinfo output with proper escaping."
  (if docstring
      (let ((escaped (forj-texinfo--escape-string docstring)))
        ;; Convert parameter references to @var{param} format
        (replace-regexp-in-string
         "\\b\\([A-Z][A-Z0-9-]*\\)\\b"
         "@var{\\1}"
         ;; Convert `code` to @code{code} format
         (replace-regexp-in-string
          "`\\([^'`]+\\)'"
          "@code{\\1}"
          escaped)))
    "No documentation available."))

(defun forj-texinfo--extract-function-info (symbol)
  "Extract documentation information for function SYMBOL."
  (when (fboundp symbol)
    (let* ((func (symbol-function symbol))
           (docstring (documentation symbol))
           (args (help-function-arglist symbol)))
      (list :name symbol
            :type 'function
            :args args
            :docstring docstring
            :interactive (commandp symbol)))))

(defun forj-texinfo--extract-variable-info (symbol)
  "Extract documentation information for variable SYMBOL."
  (when (boundp symbol)
    (let ((docstring (documentation-property symbol 'variable-documentation)))
      (list :name symbol
            :type 'variable
            :docstring docstring
            :customizable (get symbol 'standard-value)))))

(defun forj-texinfo--scan-symbols (&optional prefix)
  "Scan for symbols with optional PREFIX filter.
Returns list of symbol information plists."
  (let ((symbols '())
        (prefix-string (if prefix (symbol-name prefix) "forj-")))
    
    ;; Collect functions
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (string-prefix-p prefix-string (symbol-name symbol)))
         (let ((info (forj-texinfo--extract-function-info symbol)))
           (when info
             (push info symbols))))))
    
    ;; Collect variables
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (string-prefix-p prefix-string (symbol-name symbol)))
         (let ((info (forj-texinfo--extract-variable-info symbol)))
           (when info
             (push info symbols))))))
    
    ;; Sort by name
    (sort symbols (lambda (a b) 
                    (string< (symbol-name (plist-get a :name))
                             (symbol-name (plist-get b :name)))))))

;;; Texinfo Generation

(defun forj-texinfo--generate-function-entry (info)
  "Generate Texinfo entry for function INFO."
  (let* ((name (plist-get info :name))
         (args (plist-get info :args))
         (docstring (plist-get info :docstring))
         (interactive-p (plist-get info :interactive))
         (type-desc (if interactive-p "Interactive Function" "Function")))
    
    (format "@deffn {%s} %s %s\n%s\n@end deffn\n\n"
            type-desc
            name
            (if args (format "%s" args) "")
            (forj-texinfo--format-docstring docstring))))

(defun forj-texinfo--generate-variable-entry (info)
  "Generate Texinfo entry for variable INFO."
  (let* ((name (plist-get info :name))
         (docstring (plist-get info :docstring))
         (customizable-p (plist-get info :customizable))
         (type-desc (if customizable-p "Variable" "Variable")))
    
    (format "@defvr {%s} %s\n%s\n@end defvr\n\n"
            type-desc
            name
            (forj-texinfo--format-docstring docstring))))

(defun forj-texinfo--generate-section (title symbols)
  "Generate Texinfo section with TITLE containing SYMBOLS."
  (let ((content (format "@subsection %s\n\n" title)))
    (dolist (symbol-info symbols)
      (setq content
            (concat content
                    (pcase (plist-get symbol-info :type)
                      ('function (forj-texinfo--generate-function-entry symbol-info))
                      ('variable (forj-texinfo--generate-variable-entry symbol-info))))))
    content))

;;; Main Generation Functions

(defun forj-texinfo-generate-api-reference (&optional symbols)
  "Generate complete API reference Texinfo content.
If SYMBOLS is provided, use those; otherwise scan for forj- prefixed symbols."
  (let* ((symbol-list (or symbols (forj-texinfo--scan-symbols)))
         (functions (cl-remove-if-not 
                     (lambda (s) (eq (plist-get s :type) 'function)) 
                     symbol-list))
         (variables (cl-remove-if-not 
                     (lambda (s) (eq (plist-get s :type) 'variable)) 
                     symbol-list))
         (interactive-functions (cl-remove-if-not 
                                 (lambda (s) (plist-get s :interactive))
                                 functions))
         (internal-functions (cl-remove-if 
                              (lambda (s) (plist-get s :interactive))
                              functions))
         (custom-variables (cl-remove-if-not 
                            (lambda (s) (plist-get s :customizable))
                            variables))
         (internal-variables (cl-remove-if 
                              (lambda (s) (plist-get s :customizable))
                              variables)))
    
    (concat
     ";; This file is auto-generated from forj.el source code\n"
     ";; Do not edit manually - run forj-texinfo-generate-api-reference instead\n\n"
     "@node API Reference\n"
     "@chapter API Reference\n\n"
     "This chapter provides complete documentation for all Forj.el functions and variables.\n\n"
     "@menu\n"
     "* Interactive Functions::   User commands and interactive functions\n"
     "* Core Functions::         Internal functions and utilities\n"
     "* Customization Variables:: User configuration options\n"
     "* Internal Variables::     Internal state and configuration\n"
     "@end menu\n\n"
     
     ;; Interactive Functions Section
     "@node Interactive Functions\n"
     "@section Interactive Functions\n\n"
     "Functions that can be called interactively with @kbd{M-x}.\n\n"
     (forj-texinfo--generate-section "User Commands" interactive-functions)
     
     ;; Core Functions Section  
     "@node Core Functions\n"
     "@section Core Functions\n\n"
     "Internal functions and utilities.\n\n"
     (forj-texinfo--generate-section "Internal Functions" internal-functions)
     
     ;; Customization Variables Section
     "@node Customization Variables\n"
     "@section Customization Variables\n\n"
     "Variables that can be customized through the Emacs customize interface.\n\n"
     (forj-texinfo--generate-section "Custom Variables" custom-variables)
     
     ;; Internal Variables Section
     "@node Internal Variables\n"
     "@section Internal Variables\n\n"
     "Internal state and configuration variables.\n\n"
     (forj-texinfo--generate-section "Internal Variables" internal-variables))))

(defun forj-texinfo-write-api-reference (&optional output-file)
  "Write API reference to OUTPUT-FILE.
If OUTPUT-FILE is nil, uses default location."
  (interactive)
  (let* ((output-path (or output-file 
                          (expand-file-name 
                           forj-texinfo-api-file 
                           forj-texinfo-output-dir)))
         (content (forj-texinfo-generate-api-reference)))
    
    ;; Ensure output directory exists
    (make-directory (file-name-directory output-path) t)
    
    ;; Write the file
    (with-temp-file output-path
      (insert content))
    
    (message "API reference written to: %s" output-path)
    output-path))

;;; Integration Functions

(defun forj-texinfo-update-api-reference ()
  "Update the API reference file with current symbol information.
This is typically called during the build process."
  (interactive)
  (let ((start-time (current-time)))
    (message "Updating API reference documentation...")
    
    ;; Generate new API reference
    (forj-texinfo-write-api-reference)
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "API reference updated in %.3f seconds" elapsed))))

(defun forj-texinfo-validate-references ()
  "Validate that all documented functions actually exist."
  (interactive)
  (let ((symbols (forj-texinfo--scan-symbols))
        (missing '())
        (found 0))
    
    (dolist (symbol-info symbols)
      (let ((name (plist-get symbol-info :name)))
        (cond
         ((eq (plist-get symbol-info :type) 'function)
          (if (fboundp name)
              (setq found (1+ found))
            (push name missing)))
         ((eq (plist-get symbol-info :type) 'variable)
          (if (boundp name)
              (setq found (1+ found))
            (push name missing))))))
    
    (if missing
        (message "⚠️  Missing symbols: %s" missing)
      (message "✅ All %d symbols validated successfully" found))
    
    (null missing)))

;; Development helper
(defun forj-texinfo-preview-symbol (symbol)
  "Preview Texinfo output for SYMBOL in a temporary buffer."
  (interactive "SSymbol: ")
  (let* ((info (cond
                ((fboundp symbol) (forj-texinfo--extract-function-info symbol))
                ((boundp symbol) (forj-texinfo--extract-variable-info symbol))
                (t (error "Symbol %s is not bound" symbol))))
         (output (pcase (plist-get info :type)
                   ('function (forj-texinfo--generate-function-entry info))
                   ('variable (forj-texinfo--generate-variable-entry info))))
         (buffer (get-buffer-create "*Forj Texinfo Preview*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert output)
      (texinfo-mode)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(provide 'forj-texinfo)
;;; forj-texinfo.el ends here