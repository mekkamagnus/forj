;;; forj-progress.el --- Progress indicators and animations for forj.el -*- lexical-binding: t -*-

;;; Commentary:
;; Progress indicators and animation system for forj conversation buffer.
;; Provides animated spinners, progress bars, status indicators, and visual feedback
;; for AI operations, file processing, and user interactions.
;; Part of Phase 1.6 UI/UX Enhancement workflow.

;;; Code:

(require 'cl-lib)

;;; Customization Group

(defgroup forj-progress nil
  "Progress indicators and animations for forj."
  :group 'forj
  :prefix "forj-progress-")

;;; Configuration

(defcustom forj-progress-spinner-style 'dots
  "Style of progress spinner animation.
'dots: Rotating dots (⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏)
'bars: Rotating bars (│╱─╲)  
'arrows: Rotating arrows (←↖↑↗→↘↓↙)
'clock: Clock-like rotation (🕐🕑🕒🕓🕔🕕🕖🕗🕘🕙🕚🕛)"
  :type '(choice (const :tag "Rotating dots" dots)
                 (const :tag "Rotating bars" bars)
                 (const :tag "Rotating arrows" arrows)
                 (const :tag "Clock faces" clock))
  :group 'forj-progress)

(defcustom forj-progress-update-interval 0.1
  "Interval in seconds between progress animation updates."
  :type 'number
  :group 'forj-progress)

(defcustom forj-progress-show-percentage t
  "Whether to show percentage in progress indicators."
  :type 'boolean
  :group 'forj-progress)

(defcustom forj-progress-bar-width 30
  "Width of progress bars in characters."
  :type 'integer
  :group 'forj-progress)

;;; Spinner Definitions

(defvar forj-spinner-frames
  '((dots . ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
    (bars . ["│" "╱" "─" "╲"])
    (arrows . ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"])
    (clock . ["🕐" "🕑" "🕒" "🕓" "🕔" "🕕" "🕖" "🕗" "🕘" "🕙" "🕚" "🕛"]))
  "Animation frames for different spinner styles.")

;;; Progress State Management

(defvar forj-active-progress-indicators '()
  "List of currently active progress indicators.")

(defvar forj-progress-update-timer nil
  "Timer for updating progress animations.")

(cl-defstruct forj-progress-indicator
  id
  type
  message
  position
  overlay
  start-time
  current-frame
  total-frames
  progress
  max-progress
  completed)

;;; Core Progress Functions

;;;###autoload
(defun forj-show-progress (message &optional id)
  "Show animated progress indicator with MESSAGE and optional ID.
Returns progress indicator ID for later updates or removal."
  (let* ((indicator-id (or id (format "progress-%d" (random 100000))))
         (spinner-frames (cdr (assq forj-progress-spinner-style forj-spinner-frames)))
         (indicator (make-forj-progress-indicator
                    :id indicator-id
                    :type 'spinner
                    :message message
                    :position (point)
                    :start-time (current-time)
                    :current-frame 0
                    :total-frames (length spinner-frames)
                    :progress 0
                    :max-progress 100
                    :completed nil)))
    
    ;; Insert progress indicator in buffer
    (forj-insert-progress-indicator indicator)
    
    ;; Add to active indicators list
    (push indicator forj-active-progress-indicators)
    
    ;; Start animation timer if needed
    (forj-ensure-progress-timer)
    
    indicator-id))

;;;###autoload
(defun forj-show-progress-bar (message max-value &optional id initial-value)
  "Show progress bar with MESSAGE, MAX-VALUE, optional ID and INITIAL-VALUE.
Returns progress indicator ID for updates."
  (let* ((indicator-id (or id (format "progress-bar-%d" (random 100000))))
         (initial (or initial-value 0))
         (indicator (make-forj-progress-indicator
                    :id indicator-id
                    :type 'progress-bar
                    :message message
                    :position (point)
                    :start-time (current-time)
                    :current-frame 0
                    :total-frames forj-progress-bar-width
                    :progress initial
                    :max-progress max-value
                    :completed nil)))
    
    ;; Insert progress bar in buffer
    (forj-insert-progress-indicator indicator)
    
    ;; Add to active indicators list
    (push indicator forj-active-progress-indicators)
    
    ;; Start update timer if needed
    (forj-ensure-progress-timer)
    
    indicator-id))

;;;###autoload
(defun forj-update-progress (id progress &optional message)
  "Update progress indicator ID with new PROGRESS value and optional MESSAGE."
  (when-let ((indicator (forj-find-progress-indicator id)))
    (setf (forj-progress-indicator-progress indicator) progress)
    (when message
      (setf (forj-progress-indicator-message indicator) message))
    (forj-refresh-progress-indicator indicator)))

;;;###autoload
(defun forj-complete-progress (id &optional final-message)
  "Complete progress indicator ID with optional FINAL-MESSAGE."
  (when-let ((indicator (forj-find-progress-indicator id)))
    (setf (forj-progress-indicator-completed indicator) t)
    (setf (forj-progress-indicator-progress indicator) 
          (forj-progress-indicator-max-progress indicator))
    (when final-message
      (setf (forj-progress-indicator-message indicator) final-message))
    
    ;; Update display one final time
    (forj-refresh-progress-indicator indicator)
    
    ;; Remove after delay
    (run-with-timer 2.0 nil #'forj-hide-progress id)))

;;;###autoload
(defun forj-hide-progress (id)
  "Hide and remove progress indicator with ID."
  (when-let ((indicator (forj-find-progress-indicator id)))
    ;; Remove overlay
    (when (forj-progress-indicator-overlay indicator)
      (delete-overlay (forj-progress-indicator-overlay indicator)))
    
    ;; Remove from active list
    (setq forj-active-progress-indicators
          (cl-remove indicator forj-active-progress-indicators))
    
    ;; Stop timer if no more active indicators
    (when (and (null forj-active-progress-indicators) forj-progress-update-timer)
      (cancel-timer forj-progress-update-timer)
      (setq forj-progress-update-timer nil))))

;;; Progress Display Functions

(defun forj-insert-progress-indicator (indicator)
  "Insert visual representation of INDICATOR in buffer."
  (let* ((start-pos (point))
         (content (forj-format-progress-indicator indicator))
         (overlay (make-overlay start-pos start-pos)))
    
    ;; Insert content
    (insert content)
    (let ((end-pos (point)))
      ;; Update overlay
      (move-overlay overlay start-pos end-pos)
      (overlay-put overlay 'forj-progress-indicator t)
      (overlay-put overlay 'forj-progress-id (forj-progress-indicator-id indicator))
      (overlay-put overlay 'face 'forj-progress-face)
      (overlay-put overlay 'evaporate t)
      
      ;; Store overlay in indicator
      (setf (forj-progress-indicator-overlay indicator) overlay)
      (setf (forj-progress-indicator-position indicator) start-pos))))

(defun forj-format-progress-indicator (indicator)
  "Format INDICATOR for display."
  (let ((type (forj-progress-indicator-type indicator))
        (message (forj-progress-indicator-message indicator)))
    (pcase type
      ('spinner
       (format "%s %s" 
               (forj-get-spinner-frame indicator)
               message))
      ('progress-bar
       (format "%s %s%s" 
               message
               (forj-get-progress-bar indicator)
               (if forj-progress-show-percentage
                   (format " %d%%" (forj-get-progress-percentage indicator))
                 "")))
      (_ message))))

(defun forj-get-spinner-frame (indicator)
  "Get current spinner frame for INDICATOR."
  (let* ((frames (cdr (assq forj-progress-spinner-style forj-spinner-frames)))
         (frame-index (forj-progress-indicator-current-frame indicator)))
    (aref frames frame-index)))

(defun forj-get-progress-bar (indicator)
  "Get progress bar representation for INDICATOR."
  (let* ((progress (forj-progress-indicator-progress indicator))
         (max-progress (forj-progress-indicator-max-progress indicator))
         (completed (forj-progress-indicator-completed indicator))
         (bar-width forj-progress-bar-width)
         (filled-width (if completed
                          bar-width
                        (floor (* bar-width (/ (float progress) max-progress)))))
         (empty-width (- bar-width filled-width)))
    
    (format "[%s%s]"
            (make-string filled-width (if completed ?✓ ?█))
            (make-string empty-width ?░))))

(defun forj-get-progress-percentage (indicator)
  "Get progress percentage for INDICATOR."
  (let ((progress (forj-progress-indicator-progress indicator))
        (max-progress (forj-progress-indicator-max-progress indicator)))
    (floor (* 100 (/ (float progress) max-progress)))))

;;; Update and Animation System

(defun forj-ensure-progress-timer ()
  "Ensure progress update timer is running."
  (unless forj-progress-update-timer
    (setq forj-progress-update-timer
          (run-with-timer 0 forj-progress-update-interval
                         #'forj-update-all-progress))))

(defun forj-update-all-progress ()
  "Update all active progress indicators."
  (dolist (indicator forj-active-progress-indicators)
    (unless (forj-progress-indicator-completed indicator)
      ;; Update spinner frame
      (when (eq (forj-progress-indicator-type indicator) 'spinner)
        (setf (forj-progress-indicator-current-frame indicator)
              (mod (1+ (forj-progress-indicator-current-frame indicator))
                   (forj-progress-indicator-total-frames indicator))))
      
      ;; Refresh display
      (forj-refresh-progress-indicator indicator)))
  
  ;; Stop timer if no active indicators
  (when (null forj-active-progress-indicators)
    (cancel-timer forj-progress-update-timer)
    (setq forj-progress-update-timer nil)))

(defun forj-refresh-progress-indicator (indicator)
  "Refresh visual display of INDICATOR."
  (when-let ((overlay (forj-progress-indicator-overlay indicator)))
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (new-content (forj-format-progress-indicator indicator)))
      
      ;; Update buffer content
      (save-excursion
        (goto-char start)
        (let ((inhibit-read-only t))
          (delete-region start end)
          (insert new-content)
          ;; Update overlay position
          (move-overlay overlay start (point)))))))

;;; Utility Functions

(defun forj-find-progress-indicator (id)
  "Find progress indicator with ID."
  (cl-find id forj-active-progress-indicators
           :key #'forj-progress-indicator-id
           :test #'string=))

(defun forj-clear-all-progress ()
  "Clear all active progress indicators."
  (interactive)
  (dolist (indicator forj-active-progress-indicators)
    (forj-hide-progress (forj-progress-indicator-id indicator))))

;;; High-Level Progress Functions

;;;###autoload
(defun forj-with-progress (message function &rest args)
  "Execute FUNCTION with ARGS while showing progress MESSAGE.
Returns the result of calling FUNCTION."
  (let ((progress-id (forj-show-progress message)))
    (unwind-protect
        (apply function args)
      (forj-complete-progress progress-id "✅ Done"))))

;;;###autoload
(defun forj-with-progress-bar (message max-value function &rest args)
  "Execute FUNCTION with ARGS while showing progress bar with MESSAGE and MAX-VALUE.
FUNCTION should call (forj-update-progress progress-id current-value) to update progress.
Returns the result of calling FUNCTION."
  (let ((progress-id (forj-show-progress-bar message max-value)))
    (unwind-protect
        (apply function progress-id args)
      (forj-complete-progress progress-id "✅ Completed"))))

;;; Specialized Progress Indicators

;;;###autoload
(defun forj-show-api-progress (&optional custom-message)
  "Show progress indicator for API requests with optional CUSTOM-MESSAGE."
  (forj-show-progress (or custom-message "🤖 Processing AI request...") 'api-request))

;;;###autoload
(defun forj-show-file-progress (operation filename)
  "Show progress indicator for file OPERATION on FILENAME."
  (forj-show-progress (format "📁 %s: %s" operation (file-name-nondirectory filename))
                     (format "file-%s" operation)))

;;;###autoload
(defun forj-show-validation-progress (&optional filename)
  "Show progress indicator for code validation with optional FILENAME."
  (forj-show-progress (format "🔍 Validating%s..." 
                             (if filename (format " %s" (file-name-nondirectory filename)) " code"))
                     'validation))

;;;###autoload
(defun forj-show-git-progress (operation)
  "Show progress indicator for git OPERATION."
  (forj-show-progress (format "🔧 Git %s..." operation) (format "git-%s" operation)))

;;; Integration with Existing Systems

;;;###autoload
(defun forj-setup-progress-integration ()
  "Set up progress indicators for forj operations."
  (when (and (boundp 'forj-conversation-buffer)
             (get-buffer forj-conversation-buffer))
    (with-current-buffer forj-conversation-buffer
      ;; Set up progress display area if needed
      (save-excursion
        (goto-char (point-max))
        (unless (looking-back "\n\n" 2)
          (insert "\n"))))))

;; Integration hooks for existing functions
(defun forj-progress-advice-before (orig-fun &rest args)
  "Advice to show progress before function execution."
  (let ((progress-id (forj-show-progress "Processing...")))
    (unwind-protect
        (apply orig-fun args)
      (forj-complete-progress progress-id "✅ Done"))))

;;; Status Indicators

;;;###autoload
(defun forj-show-status (message &optional type duration)
  "Show status message with optional TYPE and DURATION.
TYPE can be 'success, 'error, 'warning, or 'info.
DURATION is how long to show the status (default 3 seconds)."
  (let* ((duration (or duration 3.0))
         (face (pcase type
                ('success 'forj-success-face)
                ('error 'forj-error-face)  
                ('warning 'forj-warning-face)
                ('info 'forj-info-face)
                (_ 'forj-progress-face)))
         (icon (pcase type
                ('success "✅")
                ('error "❌")
                ('warning "⚠️") 
                ('info "ℹ️")
                (_ "📍")))
         (formatted-message (format "%s %s" icon message)))
    
    (save-excursion
      (goto-char (point-max))
      (let* ((start (point))
             (overlay (make-overlay start start)))
        (insert formatted-message "\n")
        (let ((end (point)))
          (move-overlay overlay start end)
          (overlay-put overlay 'face face)
          (overlay-put overlay 'forj-status-indicator t)
          (overlay-put overlay 'evaporate t)
          
          ;; Remove after duration
          (run-with-timer duration nil
                         (lambda ()
                           (when (overlay-buffer overlay)
                             (delete-overlay overlay)))))))))

;;; Animation Utilities

(defun forj-animate-text (text frames interval &optional repeat)
  "Animate TEXT through FRAMES with INTERVAL, optionally REPEAT."
  (let ((frame-index 0)
        (timer nil))
    (cl-labels ((animate-frame ()
                  (when (< frame-index (length frames))
                    (message "%s %s" (aref frames frame-index) text)
                    (setq frame-index (1+ frame-index))
                    (setq timer (run-with-timer interval nil #'animate-frame)))
                  (when (and repeat (>= frame-index (length frames)))
                    (setq frame-index 0)
                    (setq timer (run-with-timer interval nil #'animate-frame)))))
      (animate-frame)
      ;; Return timer for potential cancellation
      timer)))

;;; Cleanup Functions

;;;###autoload
(defun forj-cleanup-progress ()
  "Clean up all progress indicators and timers."
  (interactive)
  (forj-clear-all-progress)
  (when forj-progress-update-timer
    (cancel-timer forj-progress-update-timer)
    (setq forj-progress-update-timer nil)))

(provide 'forj-progress)
;;; forj-progress.el ends here