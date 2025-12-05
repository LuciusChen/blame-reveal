;;; blame-reveal-ui.el --- UI rendering and event handling -*- lexical-binding: t; -*-

;;; Commentary:
;; UI layer: rendering, scrolling, animations, and event handlers.
;; Coordinates visual updates and user interactions.

;;; Code:

(require 'blame-reveal-core)
(require 'blame-reveal-state)
(require 'blame-reveal-git)
(require 'blame-reveal-overlay)
(require 'blame-reveal-color)
(require 'blame-reveal-header)

;;; Loading Animation

(defun blame-reveal--get-loading-gradient-color (intensity)
  "Get loading animation color with INTENSITY (0.0 to 1.0).
Dark theme: intensity controls brightness (0=dark, 1=bright).
Light theme: intensity controls darkness (0=light/invisible, 1=dark/visible)."
  (let* ((is-dark (blame-reveal-color--is-dark-theme-p))
         (scheme blame-reveal-color-scheme)
         (hue (plist-get scheme :hue))
         (saturation (plist-get scheme :saturation-max))
         ;; Calculate lightness based on intensity and theme
         (lightness (if is-dark
                        ;; Dark theme: intensity 1.0 = L 0.70 (bright)
                        ;;             intensity 0.0 = L 0.0 (dark/invisible)
                        (* 0.70 intensity)
                      ;; Light theme: intensity 1.0 = L 0.45 (medium dark/visible)
                      ;;              intensity 0.0 = L 0.95 (very light/invisible)
                      (- 0.97 (* 0.45 intensity))))  ; 0.95 - 0.50*1.0 = 0.45
         (color (blame-reveal-color--hsl-to-hex hue saturation lightness)))
    color))

(defun blame-reveal--get-fringe-background ()
  "Get the appropriate fringe background color."
  (or (face-background 'fringe nil t)
      (face-background 'default nil t)
      "unspecified"))

(defun blame-reveal--ensure-fringe-face-with-bg (color)
  "Ensure a face exists for COLOR with proper fringe background."
  (let* ((face-name (intern (format "blame-reveal-fringe-%s" color)))
         (bg-color (blame-reveal--get-fringe-background)))
    (unless (facep face-name)
      (make-face face-name)
      (set-face-attribute face-name nil
                          :foreground color
                          :background bg-color))
    ;; Dynamically update background (on theme change)
    (when (facep face-name)
      (set-face-attribute face-name nil :background bg-color))
    face-name))

(defun blame-reveal--create-loading-animation ()
  "Create loading animation overlays with smooth interpolated trail effect."
  (let ((win (get-buffer-window (current-buffer))))
    (if (not win)
        nil  ; Don't create animation if buffer not visible
      (let* ((win-height (window-body-height win))
             (center-line (/ win-height 2))
             (num-indicators 6)
             (half-num (/ num-indicators 2))
             (start-line (max 1 (- center-line half-num)))
             (overlays nil)
             ;; Current precise position (including sub-step)
             (current-pos (+ blame-reveal--loading-animation-step
                             blame-reveal--loading-animation-sub-step)))
        (save-excursion
          (goto-char (window-start win))
          (forward-line (1- start-line))
          (dotimes (i num-indicators)
            (unless (eobp)
              (let* ((pos (line-beginning-position))
                     (ov (make-overlay pos pos))
                     ;; Calculate precise floating-point distance
                     (raw-offset (- i current-pos))
                     ;; Normalize offset for seamless cycling
                     (offset (cond
                              ((> raw-offset (/ num-indicators 2.0))
                               (- raw-offset num-indicators))
                              ((< raw-offset (- (/ num-indicators 2.0)))
                               (+ raw-offset num-indicators))
                              (t raw-offset)))
                     (distance (abs offset))
                     ;; Exponential decay + smooth interpolation
                     (intensity (cond
                                 ((< distance 0.5) 1.0)  ; Core bright region
                                 ((<= distance 3.5)      ; Smooth decay region
                                  (max 0.1 (* (- 1.0 (* distance 0.25))
                                              (- 1.0 (* distance 0.15)))))
                                 (t 0.05)))              ; Background dim
                     (bitmap (cond
                              ((> intensity 0.5) 'blame-reveal-loading-bright)
                              ((> intensity 0.2) 'blame-reveal-loading-bright)
                              (t 'blame-reveal-loading-dim)))
                     (color (blame-reveal--get-loading-gradient-color intensity))
                     (face (blame-reveal--ensure-fringe-face-with-bg color)))
                (overlay-put ov 'blame-reveal-loading t)
                (overlay-put ov 'before-string
                             (propertize "!" 'display
                                         (list blame-reveal-fringe-side
                                               bitmap
                                               face)))
                (push ov overlays)
                (forward-line 1)))))
        (nreverse overlays)))))

(defun blame-reveal--update-loading-animation ()
  "Update loading animation with smooth sub-step interpolation."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and blame-reveal-mode
                   (eq blame-reveal--state-status 'loading)
                   (get-buffer-window buffer))
          ;; Clean up old overlays
          (dolist (ov blame-reveal--loading-animation-overlays)
            (when (overlay-buffer ov)
              (delete-overlay ov)))
          ;; Update sub-step (smooth change between 0.0 and 1.0)
          (setq blame-reveal--loading-animation-frame-counter
                (1+ blame-reveal--loading-animation-frame-counter))
          (setq blame-reveal--loading-animation-sub-step
                (/ (float blame-reveal--loading-animation-frame-counter)
                   blame-reveal-loading-animation-frames-per-step))
          ;; Advance to next integer step when sub-step reaches 1.0
          (when (>= blame-reveal--loading-animation-frame-counter
                    blame-reveal-loading-animation-frames-per-step)
            (setq blame-reveal--loading-animation-frame-counter 0)
            (setq blame-reveal--loading-animation-sub-step 0.0)
            (setq blame-reveal--loading-animation-step
                  (mod (1+ blame-reveal--loading-animation-step) 6)))
          ;; Recreate overlays (update every frame for smooth transition)
          (setq blame-reveal--loading-animation-overlays
                (blame-reveal--create-loading-animation)))))))

(defun blame-reveal--start-loading-animation ()
  "Start loading animation with smooth interpolation."
  (setq blame-reveal--loading-animation-step 0)
  (setq blame-reveal--loading-animation-frame-counter 0)
  (setq blame-reveal--loading-animation-sub-step 0.0)
  (setq blame-reveal--loading-animation-overlays
        (blame-reveal--create-loading-animation))
  ;; Start global timer if not already running
  (unless blame-reveal--global-loading-animation-timer
    (setq blame-reveal--global-loading-animation-timer
          (run-with-timer blame-reveal-loading-animation-speed
                          blame-reveal-loading-animation-speed
                          #'blame-reveal--update-loading-animation))))

(defun blame-reveal--stop-loading-animation ()
  "Stop and clear loading animation."
  ;; Clean up current buffer's animation overlays
  (dolist (ov blame-reveal--loading-animation-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq blame-reveal--loading-animation-overlays nil)
  (setq blame-reveal--loading-animation-step 0)
  (setq blame-reveal--loading-animation-frame-counter 0)
  (setq blame-reveal--loading-animation-sub-step 0.0)
  ;; Stop global timer if all buffers stopped loading
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       (and blame-reveal-mode
                            (eq blame-reveal--state-status 'loading))))
                   (buffer-list))
    (when blame-reveal--global-loading-animation-timer
      (cancel-timer blame-reveal--global-loading-animation-timer)
      (setq blame-reveal--global-loading-animation-timer nil))))

;;; Rendering Functions

(defun blame-reveal--render-visible-region ()
  "Render git blame fringe for visible region.
Reuses existing overlays when possible to minimize visual disruption."
  (when (and blame-reveal--blame-data
             (buffer-file-name)
             (not (= (point-max) 1)))
    (when-let ((range (blame-reveal--get-visible-line-range)))
      (condition-case err
          (let* ((start-line (car range))
                 (end-line (cdr range))
                 (blocks (blame-reveal--find-block-boundaries
                          blame-reveal--blame-data start-line end-line))
                 (rendered-lines (make-hash-table :test 'eql)))
            (run-hook-with-args 'blame-reveal-before-render-hook start-line end-line)
            ;; Ensure commit info is loaded for visible blocks
            (dolist (block blocks)
              (let ((commit-hash (nth 1 block)))
                (blame-reveal--ensure-commit-info commit-hash)))
            ;; Update recent commits based on what's loaded so far
            (blame-reveal--update-recent-commits)
            ;; Render blocks
            (dolist (block blocks)
              (let* ((block-start (nth 0 block))
                     (commit-hash (nth 1 block))
                     (block-length (nth 2 block)))
                ;; Skip uncommitted changes unless explicitly enabled
                (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                             (not blame-reveal-show-uncommitted-fringe))
                  ;; Render permanent fringe for recent commits
                  (when (blame-reveal--should-render-commit commit-hash)
                    (let ((color (blame-reveal--get-commit-color commit-hash))
                          (block-end (+ block-start block-length -1)))
                      ;; Render each line in visible range
                      (let ((render-start (max block-start start-line))
                            (render-end (min block-end end-line)))
                        (dotimes (i (- render-end render-start -1))
                          (let ((line-num (+ render-start i)))
                            (blame-reveal--create-fringe-overlay
                             line-num color commit-hash)
                            (puthash line-num t rendered-lines)))))))))
            ;; Clean up overlays outside visible range
            (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
              (when (overlay-buffer overlay)
                (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
                  (when (and line
                             (not (gethash line rendered-lines))
                             (or (< line start-line) (> line end-line)))
                    (blame-reveal--unregister-overlay overlay)))))
            (run-hook-with-args 'blame-reveal-after-render-hook start-line end-line)
            ;; Re-trigger header update
            (blame-reveal--update-header))
        (error
         (message "Error rendering visible region: %s" (error-message-string err)))))))

(defun blame-reveal--render-expanded-region (start-line end-line)
  "Render blame fringe for newly expanded region.
Reuses existing overlays when possible to avoid flicker."
  (when blame-reveal--blame-data
    (let* ((blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data start-line end-line))
           (rendered-lines (make-hash-table :test 'eql)))
      ;; Render blocks
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))
          ;; Skip uncommitted changes unless explicitly enabled
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            ;; Render permanent fringe for recent commits
            (when (blame-reveal--should-render-commit commit-hash)
              (let ((color (blame-reveal--get-commit-color commit-hash))
                    (block-end (+ block-start block-length -1)))
                ;; Render each line in block
                (dotimes (i (- block-end block-start -1))
                  (let ((line-num (+ block-start i)))
                    (blame-reveal--create-fringe-overlay
                     line-num color commit-hash)
                    (puthash line-num t rendered-lines))))))))
      ;; Keep existing overlays outside expanded region
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line
                       (not (gethash line rendered-lines))
                       (or (< line start-line) (> line end-line)))
              nil)))))))  ; Keep this overlay (outside expanded region)

(defun blame-reveal--should-render-commit (commit-hash)
  "Check if commit should be rendered in permanent layer.
Only recent commits are permanently visible."
  (blame-reveal--is-recent-commit-p commit-hash))

(defun blame-reveal--recolor-and-render ()
  "Recalculate colors and re-render (for theme changes)."
  (when blame-reveal--blame-data
    (setq blame-reveal--color-map (make-hash-table :test 'equal))
    (blame-reveal--init-color-strategy)
    (blame-reveal--render-visible-region)))

;;; Event Handlers

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line.
Uses caching to avoid repeated searches within the same block."
  (let ((line-num (line-number-at-pos)))
    ;; 检查缓存是否有效
    (if (and blame-reveal--current-line-cache
             (let* ((cached-line (nth 0 blame-reveal--current-line-cache))
                    (cached-commit (nth 1 blame-reveal--current-line-cache))
                    (cached-start (nth 2 blame-reveal--current-line-cache)))
               ;; 检查是否在缓存的 block 范围内
               (when-let ((entry (assoc line-num blame-reveal--blame-data)))
                 (equal (cdr entry) cached-commit))))
        ;; 缓存命中，直接返回
        (cons (nth 1 blame-reveal--current-line-cache)
              (nth 2 blame-reveal--current-line-cache))

      ;; 缓存未命中，重新查找
      (let ((result
             (or (cl-loop for ov in (overlays-at (point))
                          for commit = (overlay-get ov 'blame-reveal-commit)
                          when commit
                          return (cl-loop for block in (blame-reveal--find-block-boundaries
                                                        blame-reveal--blame-data)
                                          when (and (equal commit (nth 1 block))
                                                    (>= line-num (nth 0 block))
                                                    (< line-num (+ (nth 0 block) (nth 2 block))))
                                          return (cons commit (nth 0 block))))
                 ;; Fallback
                 (cl-loop for block in (blame-reveal--find-block-boundaries
                                        blame-reveal--blame-data)
                          when (and (>= line-num (nth 0 block))
                                    (< line-num (+ (nth 0 block) (nth 2 block))))
                          return (cons (nth 1 block) (nth 0 block))))))
        ;; 更新缓存
        (when result
          (setq blame-reveal--current-line-cache
                (list line-num (car result) (cdr result))))
        result))))

(defun blame-reveal--update-header ()
  "Update header display based on cursor position.
Throttles updates to avoid excessive calls during rapid cursor movement."
  (let* ((current-line (line-number-at-pos))
         (current-block (blame-reveal--get-current-block)))
    (when current-block
      (let ((commit-hash (car current-block)))
        ;; 只在切换 block 时清理旧 overlay
        (when (and blame-reveal--last-rendered-commit
                   (not (equal blame-reveal--last-rendered-commit commit-hash)))
          (blame-reveal--clear-temp-overlays-for-commit
           blame-reveal--last-rendered-commit)
          (setq blame-reveal--last-rendered-commit nil))

        ;; 更新当前 block 标记
        (setq blame-reveal--current-block-commit commit-hash)

        ;; 节流：如果在同一行或同一 commit block，跳过更新
        (when (or (not blame-reveal--last-update-line)
                  (not (equal blame-reveal--last-update-line current-line))
                  (not (equal blame-reveal--last-rendered-commit commit-hash)))

          ;; 取消之前的 timer
          (when blame-reveal--header-update-timer
            (cancel-timer blame-reveal--header-update-timer)
            (setq blame-reveal--header-update-timer nil))

          ;; 设置新 timer
          (setq blame-reveal--header-update-timer
                (run-with-idle-timer
                 blame-reveal--scroll-render-delay nil
                 (lambda ()
                   (setq blame-reveal--last-update-line current-line)
                   (blame-reveal--update-header-impl)))))))))

(defun blame-reveal--update-header-impl ()
  "Implementation of header update (using unified header system)."
  (when blame-reveal--blame-data
    (blame-reveal--ensure-visible-commits-loaded)
    (if-let ((current-block (blame-reveal--get-current-block)))
        (let* ((commit-hash (car current-block))
               (block-start (cdr current-block)))
          (unless (equal commit-hash blame-reveal--current-block-commit)
            (setq blame-reveal--current-block-commit commit-hash))
          ;; FIXED: Use blame-reveal--get-commit-color for consistent colors
          (let* ((color (blame-reveal--get-commit-color commit-hash))
                 (is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
                 (is-old-commit (and (not is-uncommitted)
                                     (not (blame-reveal--is-recent-commit-p commit-hash))))
                 (hide-header-fringe (and is-uncommitted
                                          (not blame-reveal-show-uncommitted-fringe))))
            (when blame-reveal--temp-overlay-timer
              (cancel-timer blame-reveal--temp-overlay-timer)
              (setq blame-reveal--temp-overlay-timer nil))
            (when blame-reveal--header-overlay
              (delete-overlay blame-reveal--header-overlay)
              (setq blame-reveal--header-overlay nil))
            ;; Use same color for header as code lines
            (setq blame-reveal--header-overlay
                  (blame-reveal--create-header-overlay
                   block-start commit-hash color hide-header-fringe))
            (when (or is-old-commit
                      (and is-uncommitted blame-reveal-show-uncommitted-fringe))
              (setq blame-reveal--temp-overlay-timer
                    (run-with-idle-timer
                     blame-reveal-temp-overlay-delay nil
                     #'blame-reveal--temp-overlay-renderer
                     (current-buffer) commit-hash color)))
            (setq blame-reveal--last-rendered-commit commit-hash)))
      (when blame-reveal--temp-overlay-timer
        (cancel-timer blame-reveal--temp-overlay-timer)
        (setq blame-reveal--temp-overlay-timer nil))
      (when blame-reveal--header-overlay
        (delete-overlay blame-reveal--header-overlay)
        (setq blame-reveal--header-overlay nil))
      (blame-reveal--clear-temp-overlays)
      (setq blame-reveal--current-block-commit nil)
      (setq blame-reveal--last-rendered-commit nil))
    (blame-reveal--update-sticky-header)))

(defun blame-reveal--scroll-handler-impl (buf)
  "Implementation of scroll handler for buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and blame-reveal-mode
                 (buffer-file-name))
        (condition-case err
            (progn
              (when blame-reveal--blame-data-range
                (when-let ((range (blame-reveal--get-visible-line-range)))
                  (let ((start-line (car range))
                        (end-line (cdr range))
                        (current-start (car blame-reveal--blame-data-range))
                        (current-end (cdr blame-reveal--blame-data-range)))
                    (when (or (< start-line current-start)
                              (> end-line current-end))
                      (blame-reveal--ensure-range-loaded start-line end-line)))))
              (unless (and (eq blame-reveal--state-status 'loading)
                           (eq blame-reveal--state-operation 'expansion))
                (blame-reveal--render-visible-region)
                (blame-reveal--update-sticky-header)))
          (error
           (message "Error in scroll handler: %s" (error-message-string err))))))))

(defun blame-reveal--on-scroll ()
  "Handle scroll event with debouncing.
Delays rendering until scrolling stops."
  (let ((current-start (window-start)))
    (unless (equal current-start blame-reveal--last-window-start)
      (setq blame-reveal--last-window-start current-start)
      ;; Cancel previous timer
      (when blame-reveal--scroll-timer
        (cancel-timer blame-reveal--scroll-timer))
      ;; Clear permanent fringe overlays
      (blame-reveal--clear-overlays-by-type 'fringe)
      (when blame-reveal--header-overlay
        (delete-overlay blame-reveal--header-overlay)
        (setq blame-reveal--header-overlay nil))
      (blame-reveal--clear-sticky-header)
      ;; Set new timer with configured delay
      (setq blame-reveal--scroll-timer
            (run-with-idle-timer
             blame-reveal--scroll-render-delay nil
             #'blame-reveal--scroll-handler-impl
             (current-buffer))))))

(defun blame-reveal--scroll-handler (_win _start)
  "Handle window scroll events."
  (blame-reveal--on-scroll))

(defun blame-reveal--theme-change-handler ()
  "Handle theme change for all blame-reveal buffers.
Recalculates colors and refreshes all displays."
  (let ((current-buf (current-buffer))
        (current-point (point)))
    ;; Re-render fringe overlays with new theme colors in all buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when blame-reveal-mode
          (blame-reveal--recolor-and-render))))
    ;; Refresh header in current buffer if it exists
    (when (buffer-live-p current-buf)
      (with-current-buffer current-buf
        (when (and blame-reveal-mode
                   blame-reveal--current-block-commit)
          (save-excursion
            (goto-char current-point)
            ;; Force header refresh by clearing and re-triggering
            (let ((old-commit blame-reveal--current-block-commit))
              (setq blame-reveal--current-block-commit nil)
              ;; Clear existing header
              (when blame-reveal--header-overlay
                (delete-overlay blame-reveal--header-overlay)
                (setq blame-reveal--header-overlay nil))
              ;; Clear temp overlays
              (blame-reveal--clear-temp-overlays)
              ;; Re-trigger header update
              (blame-reveal--update-header))))))))

(defun blame-reveal--on-theme-change (&rest _)
  "Handle theme change event and refresh all blame displays."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer))
  (setq blame-reveal--theme-change-timer
        (run-with-timer 0.3 nil #'blame-reveal--theme-change-handler)))

(defun blame-reveal--setup-theme-advice ()
  "Setup advice to monitor theme changes."
  (if (boundp 'after-enable-theme-hook)
      ;; Use official hook for Emacs 29+
      (add-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    ;; Use advice for older Emacs versions
    (advice-add 'load-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'enable-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'disable-theme :after #'blame-reveal--on-theme-change)))

(defun blame-reveal--remove-theme-advice ()
  "Remove theme change advice."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer)
    (setq blame-reveal--theme-change-timer nil))
  (if (boundp 'after-enable-theme-hook)
      (remove-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    (advice-remove 'load-theme #'blame-reveal--on-theme-change)
    (advice-remove 'enable-theme #'blame-reveal--on-theme-change)
    (advice-remove 'disable-theme #'blame-reveal--on-theme-change)))

;;; Loading Orchestration

(defun blame-reveal--load-blame-data ()
  "Load git blame data using sync or async based on configuration."
  (when (not (blame-reveal--state-is-busy-p))
    (condition-case err
        (if (blame-reveal--should-use-async-p)
            (blame-reveal--load-blame-data-async)
          (blame-reveal--load-blame-data-sync))
      (error
       (blame-reveal--state-error (error-message-string err))
       (message "Failed to load git blame: %s" (error-message-string err))))))

(defun blame-reveal--safe-cancel-timer (timer-symbol)
  "Safely cancel timer bound to TIMER-SYMBOL and set it to nil."
  (let ((timer (and (boundp timer-symbol) (symbol-value timer-symbol))))
    (when (timerp timer)
      (cancel-timer timer))
    (set timer-symbol nil)))


(defun blame-reveal--cleanup-operation-ui-artifacts ()
  "Cleanup UI elements and timers directly related to an active operation.
Used for immediate aborts (error or cancel)."

  (blame-reveal--stop-loading-animation)

  ;; 1. 清理 Timers
  (blame-reveal--safe-cancel-timer 'blame-reveal--temp-overlay-timer)
  (blame-reveal--safe-cancel-timer 'blame-reveal--header-update-timer)

  ;; 2. 清理 Header Overlays
  (ignore-errors
    (when (bound-and-true-p blame-reveal--header-overlay)
      (delete-overlay blame-reveal--header-overlay)
      (setq blame-reveal--header-overlay nil))
    (when (fboundp 'blame-reveal--clear-sticky-header)
      (blame-reveal--clear-sticky-header))))

(defun blame-reveal--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  ;; 强制取消当前操作并清理
  (when (blame-reveal--state-is-busy-p)
    (message "[Update] Cancelling current operation...")

    ;; **UI 清理 (留在 UI 层)**
    (blame-reveal--stop-loading-animation)

    ;; **状态清理 (委托给 State 层)**
    (blame-reveal--state-cancel "full update requested"))

  ;; 短暂延迟，确保清理完成
  (run-with-timer
   0.1 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ;; Reset buffer-specific data (这些数据是独立于 state 进程的)
         (setq blame-reveal--blame-data nil
               blame-reveal--blame-data-range nil
               blame-reveal--commit-info nil
               blame-reveal--color-map nil
               blame-reveal--timestamps nil
               blame-reveal--recent-commits nil
               blame-reveal--all-commits-loaded nil)
         ;; Reload
         (blame-reveal--load-blame-data))))
   (current-buffer)))

(provide 'blame-reveal-ui)
;;; blame-reveal-ui.el ends here
