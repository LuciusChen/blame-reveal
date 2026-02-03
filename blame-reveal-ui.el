;;; blame-reveal-ui.el --- UI rendering and event handling -*- lexical-binding: t; -*-

;;; Commentary:
;; UI layer: rendering, scrolling, animations, and event handlers.
;; Coordinates visual updates and user interactions.

;;; Code:

(require 'blame-reveal-core)
(require 'blame-reveal-git)
(require 'blame-reveal-color)

;;; Transient Protection
;; Prevents header interference during transient menu operations.
;; This ensures a smooth UI experience when using transient commands.

(defvar-local blame-reveal--in-transient-setup nil
  "Non-nil during transient-setup to prevent header interference.
Set to t when transient-setup begins, cleared after it completes.")

(defvar-local blame-reveal--pending-delete-overlays nil
  "List of overlays pending deletion.
These overlays will be deleted after the next redisplay cycle to prevent flicker.")

;;; Registry Initialization

(defun blame-reveal--init-overlay-registry ()
  "Initialize overlay registry and indices."
  (setq blame-reveal--overlay-registry (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-type (make-hash-table :test 'eq))
  (setq blame-reveal--overlays-by-commit (make-hash-table :test 'equal))
  (setq blame-reveal--overlays-by-line (make-hash-table :test 'eql)))

;;; Core Overlay Operations

(defun blame-reveal--register-overlay (overlay type &optional metadata)
  "Register OVERLAY of TYPE with optional METADATA.
METADATA is a plist that may contain:
  :commit - commit hash
  :line - line number
  :data - type-specific data
Returns the overlay."
  (unless blame-reveal--overlay-registry
    (blame-reveal--init-overlay-registry))
  (let ((full-metadata (plist-put (copy-sequence metadata) :type type)))
    (puthash overlay full-metadata blame-reveal--overlay-registry)
    (let ((type-list (gethash type blame-reveal--overlays-by-type)))
      (puthash type (cons overlay type-list) blame-reveal--overlays-by-type))
    (when-let* ((commit (plist-get metadata :commit)))
      (let ((commit-list (gethash commit blame-reveal--overlays-by-commit)))
        (puthash commit (cons overlay commit-list) blame-reveal--overlays-by-commit)))
    (when-let* ((line (plist-get metadata :line)))
      (let ((line-list (gethash line blame-reveal--overlays-by-line)))
        (puthash line (cons overlay line-list) blame-reveal--overlays-by-line)))
    overlay))

(defun blame-reveal--unregister-overlay (overlay)
  "Unregister OVERLAY from all indices and delete it.
Safe to call even if overlay is already deleted or invalid.
Note: This performs immediate deletion, use `blame-reveal--schedule-overlay-deletion'
for flicker-free updates."
  (when (and overlay (overlayp overlay))
    (when-let* ((metadata (gethash overlay blame-reveal--overlay-registry)))
      (let ((type (plist-get metadata :type))
            (commit (plist-get metadata :commit))
            (line (plist-get metadata :line)))
        (when type
          (let ((type-list (gethash type blame-reveal--overlays-by-type)))
            (puthash type (delq overlay type-list) blame-reveal--overlays-by-type)))
        (when commit
          (let ((commit-list (gethash commit blame-reveal--overlays-by-commit)))
            (puthash commit (delq overlay commit-list) blame-reveal--overlays-by-commit)))
        (when line
          (let ((line-list (gethash line blame-reveal--overlays-by-line)))
            (puthash line (delq overlay line-list) blame-reveal--overlays-by-line)))
        (remhash overlay blame-reveal--overlay-registry)))
    (ignore-errors
      (when (overlay-buffer overlay)
        (delete-overlay overlay)))))

;;; Overlay Query Functions

(defun blame-reveal--get-overlays-by-type (type)
  "Get all overlays of TYPE."
  (gethash type blame-reveal--overlays-by-type))

(defun blame-reveal--get-overlays-by-commit (commit)
  "Get all overlays for COMMIT."
  (gethash commit blame-reveal--overlays-by-commit))

(defun blame-reveal--get-overlays-by-line (line)
  "Get all overlays at LINE."
  (gethash line blame-reveal--overlays-by-line))

(defun blame-reveal--get-overlay-metadata (overlay)
  "Get metadata for OVERLAY."
  (gethash overlay blame-reveal--overlay-registry))

(defun blame-reveal--get-overlay-type (overlay)
  "Get type of OVERLAY."
  (plist-get (blame-reveal--get-overlay-metadata overlay) :type))

;;; Reuse and Update Utilities

(defun blame-reveal--create-managed-overlay (start end type &optional metadata)
  "Create and register an overlay from START to END of TYPE with METADATA.
Returns the created and registered overlay."
  (let ((overlay (make-overlay start end)))
    (blame-reveal--register-overlay overlay type metadata)
    overlay))

(defun blame-reveal--find-reusable-overlay (type line)
  "Find a reusable overlay of TYPE at LINE.
Returns overlay if found, nil otherwise."
  (cl-find-if (lambda (ov)
                (and (eq (blame-reveal--get-overlay-type ov) type)
                     (overlay-buffer ov)
                     (= (line-number-at-pos (overlay-start ov)) line)))
              (blame-reveal--get-overlays-by-line line)))

(defun blame-reveal--update-overlay-metadata (overlay metadata)
  "Update OVERLAY's metadata with new METADATA (plist).
Preserves :type field, and updates commit/line indices if they change."
  (when-let* ((old-metadata (gethash overlay blame-reveal--overlay-registry)))
    (let* ((type (plist-get old-metadata :type))
           (old-commit (plist-get old-metadata :commit))
           (old-line (plist-get old-metadata :line))
           (new-commit (plist-get metadata :commit))
           (new-line (plist-get metadata :line))
           (new-metadata (plist-put (copy-sequence metadata) :type type)))
      (when (and old-commit (not (equal old-commit new-commit)))
        (puthash old-commit
                 (delq overlay (gethash old-commit blame-reveal--overlays-by-commit))
                 blame-reveal--overlays-by-commit)
        (when new-commit
          (let ((commit-list (gethash new-commit blame-reveal--overlays-by-commit)))
            (puthash new-commit (cons overlay commit-list)
                     blame-reveal--overlays-by-commit))))
      (when (and old-line (not (eql old-line new-line)))
        (puthash old-line
                 (delq overlay (gethash old-line blame-reveal--overlays-by-line))
                 blame-reveal--overlays-by-line)
        (when new-line
          (let ((line-list (gethash new-line blame-reveal--overlays-by-line)))
            (puthash new-line (cons overlay line-list)
                     blame-reveal--overlays-by-line))))
      (puthash overlay new-metadata blame-reveal--overlay-registry))))

;;; Batch Deletion Operations (Immediate)

(defun blame-reveal--clear-overlays-by-type (type)
  "Clear all overlays of TYPE immediately."
  (dolist (overlay (blame-reveal--get-overlays-by-type type))
    (blame-reveal--unregister-overlay overlay))
  (puthash type nil blame-reveal--overlays-by-type))

(defun blame-reveal--clear-all-overlays ()
  "Clear all blame-reveal overlays immediately."
  (when blame-reveal--overlay-registry
    (maphash (lambda (overlay _metadata)
               (ignore-errors (delete-overlay overlay)))
             blame-reveal--overlay-registry))
  (blame-reveal--init-overlay-registry))

;;; Flicker-Free Core Mechanism (Delayed Deletion)

(defun blame-reveal--schedule-overlay-deletion (overlay)
  "Schedule OVERLAY for delayed deletion.
The overlay will be deleted after the next redisplay, preventing flicker."
  (when (overlayp overlay)
    (push overlay blame-reveal--pending-delete-overlays)
    ;; Schedule deletion after next redisplay (using a short timer)
    (run-with-timer 0.01 nil #'blame-reveal--execute-pending-deletions
                    (current-buffer))))

(defun blame-reveal--execute-pending-deletions (buffer)
  "Execute all pending overlay deletions for BUFFER.
Also removes overlays from registry if they were registered."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (ov blame-reveal--pending-delete-overlays)
        (when (overlayp ov)
          ;; Remove from registry first (if registered)
          (when (and (boundp 'blame-reveal--overlay-registry)
                     blame-reveal--overlay-registry)
            (remhash ov blame-reveal--overlay-registry))
          (delete-overlay ov)))
      (setq blame-reveal--pending-delete-overlays nil))))

(defmacro blame-reveal--with-no-flicker (&rest body)
  "Execute BODY with flicker prevention.
Wraps operations in `inhibit-redisplay' and forces a single `redisplay' at the end."
  `(prog1
       (let ((inhibit-redisplay t))
         ,@body)
     (redisplay)))

;;; Fringe Overlay Management

(defun blame-reveal--ensure-fringe-face (color)
  "Ensure fringe face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-face-%s" color))))
    (unless (facep face-name)
      (custom-declare-face face-name
                           `((t :background ,color :foreground ,color))
                           (format "Face for git blame color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (fringe-face (blame-reveal--ensure-fringe-face color))
             (overlay (or (blame-reveal--find-reusable-overlay 'fringe line-number)
                          (blame-reveal--create-managed-overlay
                            pos pos 'fringe
                            (list :commit commit-hash :line line-number)))))
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-fringe-side
                                       'blame-reveal-full
                                       fringe-face)))
        (when (overlay-buffer overlay)
          (blame-reveal--update-overlay-metadata
            overlay
            (list :commit commit-hash :line line-number)))
        overlay))))

;;; Temp Overlay Management

(defun blame-reveal--create-temp-fringe-overlay (line-number color commit-hash)
  "Create temporary fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (fringe-face (blame-reveal--ensure-fringe-face color))
             (overlay (blame-reveal--create-managed-overlay
                        pos pos 'temp-fringe
                        (list :commit commit-hash :line line-number))))
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-fringe-side
                                       'blame-reveal-full
                                       fringe-face)))
        overlay))))

(defun blame-reveal--render-temp-overlays-for-commit (commit-hash color)
  "Render temporary overlays for COMMIT-HASH with COLOR.
Only renders in visible range."
  (let* ((range (blame-reveal--get-visible-line-range))
         (start-line (car range))
         (end-line (cdr range))
         (visible-blocks (blame-reveal--find-block-boundaries
                           blame-reveal--blame-data
                           start-line
                           end-line)))
    (dolist (block visible-blocks)
      (let ((blk-start (nth 0 block))
            (blk-commit (nth 1 block))
            (blk-length (nth 2 block)))
        (when (equal blk-commit commit-hash)
          (dotimes (i blk-length)
            (let ((line-num (+ blk-start i)))
              (when (and (>= line-num start-line)
                         (<= line-num end-line))
                (blame-reveal--create-temp-fringe-overlay
                  line-num color commit-hash)))))))))

(defun blame-reveal--clear-temp-overlays ()
  "Clear all temporary fringe overlays immediately."
  (blame-reveal--clear-overlays-by-type 'temp-fringe))

(defun blame-reveal--clear-temp-overlays-for-commit (commit-hash)
  "Clear temporary overlays only for specific COMMIT-HASH immediately."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit-hash))
    (when (eq (blame-reveal--get-overlay-type overlay) 'temp-fringe)
      (blame-reveal--unregister-overlay overlay))))

(defun blame-reveal--temp-overlay-renderer (buf hash col)
  "Render temporary overlays for old commit HASH with color COL in buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (equal blame-reveal--current-block-commit hash)
        (blame-reveal--render-temp-overlays-for-commit hash col)))))

;;; Header No-Flicker Utilities

(defun blame-reveal--replace-header-overlay (new-overlay)
  "Replace current header overlay with NEW-OVERLAY without flicker.
Safely handles the transition from the old to the new header."
  (let ((old-header blame-reveal--header-overlay))
    (setq blame-reveal--header-overlay new-overlay)
    (when old-header
      (blame-reveal--schedule-overlay-deletion old-header))))

(defun blame-reveal--replace-sticky-header-overlay (new-overlay)
  "Replace current sticky header overlay with NEW-OVERLAY without flicker."
  (let ((old-sticky blame-reveal--sticky-header-overlay))
    (setq blame-reveal--sticky-header-overlay new-overlay)
    (when old-sticky
      (blame-reveal--schedule-overlay-deletion old-sticky))))

(defun blame-reveal--clear-header-no-flicker ()
  "Clear header overlay without flicker.
Uses delayed deletion to prevent visible gap."
  (when blame-reveal--header-overlay
    (blame-reveal--schedule-overlay-deletion blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil)))

(defun blame-reveal--clear-sticky-header-no-flicker ()
  "Clear sticky header overlay without flicker."
  (when blame-reveal--sticky-header-overlay
    (blame-reveal--schedule-overlay-deletion blame-reveal--sticky-header-overlay)
    (setq blame-reveal--sticky-header-overlay nil
          blame-reveal--sticky-header-state nil)))

(defun blame-reveal--clear-header ()
  "Clear all overlays (headers and stickies) without flicker."
  (blame-reveal--with-no-flicker
   (blame-reveal--clear-header-no-flicker)
   (blame-reveal--clear-sticky-header-no-flicker)
   ;; Add other flicker-free clearing calls here if needed,
   ;; e.g., for `fringe` overlays, though they are usually cleared immediately.
   ))

;;; Statistics and Debugging

(defun blame-reveal--overlay-stats ()
  "Return statistics about current overlays as a plist."
  (let ((stats nil))
    (dolist (type blame-reveal--overlay-types)
      (let ((count (length (blame-reveal--get-overlays-by-type type))))
        (setq stats (plist-put stats type count))))
    (plist-put stats :total (hash-table-count blame-reveal--overlay-registry))
    stats))

(defun blame-reveal--print-overlay-stats ()
  "Print overlay statistics (for debugging)."
  (interactive)
  (let ((stats (blame-reveal--overlay-stats)))
    (message "Overlay stats: %s"
             (mapconcat (lambda (type)
                          (format "%s=%d" type (plist-get stats type)))
                        (cons :total blame-reveal--overlay-types)
                        ", "))))

;;; Loading Animation

(defun blame-reveal--should-update-header-p ()
  "Check if header update should proceed.
Returns nil when:
- Transient menu is being set up (first-time setup interference)
- Minibuffer is active
- Current buffer is not the selected window's buffer
- Buffer is not visiting a file"
  (and (not blame-reveal--in-transient-setup)
       (not (active-minibuffer-window))
       (eq (current-buffer) (window-buffer (selected-window)))
       (buffer-file-name)))

(defun blame-reveal--get-loading-gradient-color (intensity)
  "Get loading animation color with INTENSITY (0.0 to 1.0).
Dark theme: intensity controls brightness (0=dark, 1=bright).
Light theme: intensity controls darkness (0=light/invisible, 1=dark/visible)."
  (let* ((scheme blame-reveal-color-scheme)
         (is-dark (blame-reveal-color--is-dark-theme-p))
         (hue (plist-get scheme :hue))
         (saturation (plist-get scheme :saturation-max))
         ;; Calculate lightness based on intensity and theme
         (lightness (if is-dark
                        ;; Dark theme: intensity 1.0 = L 0.70 (bright)
                        ;;             intensity 0.0 = L 0.0 (dark/invisible)
                        (* 0.70 intensity)
                      ;; Light theme: intensity 1.0 = L 0.45 (medium dark/visible)
                      ;;              intensity 0.0 = L 0.95 (very light/invisible)
                      (- 0.97 (* 0.45 intensity))))
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

(defun blame-reveal--calculate-loading-intensity (index current-pos num-indicators)
  "Calculate loading animation intensity for INDEX given CURRENT-POS and NUM-INDICATORS."
  (let* ((raw-offset (- index current-pos))
         (offset (cond
                  ((> raw-offset (/ num-indicators 2.0))
                   (- raw-offset num-indicators))
                  ((< raw-offset (- (/ num-indicators 2.0)))
                   (+ raw-offset num-indicators))
                  (t raw-offset)))
         (distance (abs offset)))
    (cond
     ((< distance 0.5) 1.0)
     ((<= distance 3.5)
      (max 0.1 (* (- 1.0 (* distance 0.25))
                  (- 1.0 (* distance 0.15)))))
     (t 0.05))))

(defun blame-reveal--create-loading-indicator-overlay (index current-pos num-indicators)
  "Create a single loading indicator overlay at INDEX.
CURRENT-POS is the animation position, NUM-INDICATORS is the total count."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos pos))
         (intensity (blame-reveal--calculate-loading-intensity index current-pos num-indicators))
         (bitmap (if (> intensity 0.2)
                     'blame-reveal-loading-bright
                   'blame-reveal-loading-dim))
         (color (blame-reveal--get-loading-gradient-color intensity))
         (face (blame-reveal--ensure-fringe-face-with-bg color)))
    (overlay-put ov 'blame-reveal-loading t)
    (overlay-put ov 'before-string
                 (propertize "!" 'display
                             (list blame-reveal-fringe-side bitmap face)))
    ov))

(defun blame-reveal--create-loading-animation ()
  "Create loading animation overlays with smooth interpolated trail effect."
  (when-let* ((win (get-buffer-window (current-buffer))))
    (let* ((win-height (window-body-height win))
           (center-line (/ win-height 2))
           (num-indicators 6)
           (half-num (/ num-indicators 2))
           (start-line (max 1 (- center-line half-num)))
           (current-pos (+ blame-reveal--loading-animation-step
                           blame-reveal--loading-animation-sub-step))
           (overlays nil))
      (save-excursion
        (goto-char (window-start win))
        (forward-line (1- start-line))
        (dotimes (i num-indicators)
          (unless (eobp)
            (push (blame-reveal--create-loading-indicator-overlay i current-pos num-indicators)
                  overlays)
            (forward-line 1))))
      (nreverse overlays))))

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

(defun blame-reveal--should-skip-uncommitted-p (commit-hash)
  "Return t if COMMIT-HASH represents uncommitted changes that should be skipped."
  (and (blame-reveal--is-uncommitted-p commit-hash)
       (not blame-reveal-show-uncommitted-fringe)))

(defun blame-reveal--render-block-fringe (block start-line end-line rendered-lines)
  "Render fringe overlays for BLOCK within START-LINE to END-LINE range.
Records rendered lines in RENDERED-LINES hash table."
  (pcase-let* ((`(,block-start ,commit-hash ,block-length) block))
    (when (and (not (blame-reveal--should-skip-uncommitted-p commit-hash))
               (blame-reveal--should-render-commit commit-hash))
      (let* ((color (blame-reveal--get-commit-color commit-hash))
             (block-end (+ block-start block-length -1))
             (render-start (max block-start start-line))
             (render-end (min block-end end-line)))
        (cl-loop for line-num from render-start to render-end
                 do (progn
                      (blame-reveal--create-fringe-overlay line-num color commit-hash)
                      (puthash line-num t rendered-lines)))))))

(defun blame-reveal--cleanup-stale-fringe-overlays (rendered-lines start-line end-line)
  "Remove fringe overlays not in RENDERED-LINES within START-LINE to END-LINE."
  (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
    (when-let* (((overlay-buffer overlay))
                (line (plist-get (blame-reveal--get-overlay-metadata overlay) :line))
                ((not (gethash line rendered-lines)))
                ((<= start-line line end-line)))
      (blame-reveal--unregister-overlay overlay))))

(defun blame-reveal--render-visible-region ()
  "Render git blame fringe for visible region.
Reuses existing overlays when possible to minimize visual disruption."
  (when (and blame-reveal--blame-data
             (buffer-file-name)
             (not (= (point-max) 1)))
    (when-let* ((range (blame-reveal--get-visible-line-range)))
      (condition-case err
          (pcase-let* ((`(,start-line . ,end-line) range)
                       (blocks (blame-reveal--find-block-boundaries
                                blame-reveal--blame-data start-line end-line))
                       (rendered-lines (make-hash-table :test 'eql)))
            (run-hook-with-args 'blame-reveal-before-render-hook start-line end-line)
            ;; Ensure commit info is loaded for visible blocks
            (mapc (lambda (block) (blame-reveal--ensure-commit-info (nth 1 block))) blocks)
            (blame-reveal--update-recent-commits)
            ;; Render blocks
            (mapc (lambda (block)
                    (blame-reveal--render-block-fringe block start-line end-line rendered-lines))
                  blocks)
            ;; Clean up stale overlays
            (blame-reveal--cleanup-stale-fringe-overlays rendered-lines start-line end-line)
            (run-hook-with-args 'blame-reveal-after-render-hook start-line end-line)
            (blame-reveal--update-header))
        (error
         (message "Error rendering visible region: %s" (error-message-string err)))))))

(defun blame-reveal--render-expanded-region (start-line end-line)
  "Render blame fringe for newly expanded region.
Reuses existing overlays when possible to avoid flicker."
  (when blame-reveal--blame-data
    (let ((blocks (blame-reveal--find-block-boundaries
                   blame-reveal--blame-data start-line end-line))
          (rendered-lines (make-hash-table :test 'eql)))
      (mapc (lambda (block)
              (blame-reveal--render-block-fringe block start-line end-line rendered-lines))
            blocks))))  ; Keep this overlay (outside expanded region)

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

(defun blame-reveal--cache-valid-p (line-num)
  "Check if current line cache is valid for LINE-NUM."
  (when-let* ((blame-reveal--current-line-cache)
              (cached-commit (nth 1 blame-reveal--current-line-cache))
              (entry (assoc line-num blame-reveal--blame-data)))
    (equal (cdr entry) cached-commit)))

(defun blame-reveal--find-block-at-line (line-num)
  "Find the block containing LINE-NUM.
Returns (COMMIT-HASH . BLOCK-START) or nil."
  (cl-loop for block in (blame-reveal--find-block-boundaries blame-reveal--blame-data)
           for block-start = (nth 0 block)
           for block-commit = (nth 1 block)
           for block-length = (nth 2 block)
           when (and (>= line-num block-start)
                     (< line-num (+ block-start block-length)))
           return (cons block-commit block-start)))

(defun blame-reveal--find-block-from-overlay (line-num)
  "Try to find block info from overlays at point for LINE-NUM.
Returns (COMMIT-HASH . BLOCK-START) or nil."
  (cl-loop for ov in (overlays-at (point))
           for commit = (overlay-get ov 'blame-reveal-commit)
           when commit
           return (cl-loop for block in (blame-reveal--find-block-boundaries
                                         blame-reveal--blame-data)
                           for block-start = (nth 0 block)
                           for block-commit = (nth 1 block)
                           for block-length = (nth 2 block)
                           when (and (equal commit block-commit)
                                     (>= line-num block-start)
                                     (< line-num (+ block-start block-length)))
                           return (cons commit block-start))))

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line.
Uses caching to avoid repeated searches within the same block."
  (let ((line-num (line-number-at-pos)))
    (if (blame-reveal--cache-valid-p line-num)
        ;; Cache hit
        (cons (nth 1 blame-reveal--current-line-cache)
              (nth 2 blame-reveal--current-line-cache))
      ;; Cache miss
      (let ((result (or (blame-reveal--find-block-from-overlay line-num)
                        (blame-reveal--find-block-at-line line-num))))
        (when result
          (setq blame-reveal--current-line-cache
                (list line-num (car result) (cdr result))))
        result))))

(defun blame-reveal--should-update-header-p ()
  "Check if header update should proceed.
Returns nil when:
- Minibuffer is active (any menu/completion system)
- Current buffer is not the selected window's buffer
- Buffer is not visiting a file"
  (and (not (active-minibuffer-window))
       (eq (current-buffer) (window-buffer (selected-window)))
       (buffer-file-name)))

(defun blame-reveal--update-header ()
  "Update header display based on cursor position.
Throttles updates to avoid excessive calls during rapid cursor movement."
  (when (blame-reveal--should-update-header-p)
    (let* ((current-line (line-number-at-pos))
           (current-block (blame-reveal--get-current-block)))
      (when current-block
        (let ((commit-hash (car current-block)))
          ;; Only clear the old overlay when switching blocks.
          (when (and blame-reveal--last-rendered-commit
                     (not (equal blame-reveal--last-rendered-commit commit-hash)))
            (blame-reveal--clear-temp-overlays-for-commit
             blame-reveal--last-rendered-commit)
            (setq blame-reveal--last-rendered-commit nil))
          ;; Update current block marker
          (setq blame-reveal--current-block-commit commit-hash)

          ;; Throttle: Skip updates if they are in the same line or the same commit block.
          (when (or (not blame-reveal--last-update-line)
                    (not (equal blame-reveal--last-update-line current-line))
                    (not (equal blame-reveal--last-rendered-commit commit-hash)))

            (run-with-idle-timer
             blame-reveal--scroll-render-delay nil
             (lambda ()
               (setq blame-reveal--last-update-line current-line)
               (blame-reveal--update-header-impl)))))))))

(defun blame-reveal--header-needs-update-p (commit-hash block-start)
  "Check if header needs update for COMMIT-HASH at BLOCK-START."
  (or (not blame-reveal--header-overlay)
      (not (equal commit-hash blame-reveal--last-rendered-commit))
      (/= (line-number-at-pos (overlay-start blame-reveal--header-overlay))
          block-start)
      (blame-reveal--header-style-changed-p)))

(defun blame-reveal--update-header-overlay (commit-hash block-start color hide-fringe)
  "Update or create header overlay for COMMIT-HASH at BLOCK-START.
Uses COLOR for styling. HIDE-FRINGE controls fringe visibility."
  (let ((style-changed (blame-reveal--header-style-changed-p)))
    (if (and blame-reveal--header-overlay
             (not style-changed)
             (blame-reveal--ensure-header-overlay
              blame-reveal--header-overlay
              block-start commit-hash color hide-fringe))
        ;; In-place update succeeded
        (setq blame-reveal--header-current-style
              (blame-reveal--get-effective-header-style))
      ;; Need to create new overlay
      (let ((new-header (blame-reveal--create-header-overlay
                         block-start commit-hash color hide-fringe)))
        (blame-reveal--replace-header-overlay new-header)
        (setq blame-reveal--header-current-style
              (blame-reveal--get-effective-header-style))))))

(defun blame-reveal--schedule-temp-overlay-if-needed (commit-hash color)
  "Schedule temp overlay rendering for COMMIT-HASH with COLOR if needed."
  (let ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
        (is-old-commit (and (not (blame-reveal--is-uncommitted-p commit-hash))
                            (not (blame-reveal--is-recent-commit-p commit-hash)))))
    (when (or is-old-commit
              (and is-uncommitted blame-reveal-show-uncommitted-fringe))
      (run-with-idle-timer
       blame-reveal-temp-overlay-delay nil
       #'blame-reveal--temp-overlay-renderer
       (current-buffer) commit-hash color))))

(defun blame-reveal--clear-header-state ()
  "Clear all header-related state."
  (blame-reveal--clear-header)
  (setq blame-reveal--current-block-commit nil
        blame-reveal--last-rendered-commit nil
        blame-reveal--header-current-style nil))

(defun blame-reveal--update-header-impl ()
  "Implementation of header update.
Uses in-place update for smooth transitions without flicker."
  (when blame-reveal--blame-data
    (blame-reveal--ensure-visible-commits-loaded)
    (let ((current-block (blame-reveal--get-current-block)))
      (if (null current-block)
          (blame-reveal--clear-header-state)
        (pcase-let* ((`(,commit-hash . ,block-start) current-block)
                     (color (blame-reveal--get-commit-color commit-hash))
                     (is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
                     (hide-header-fringe (and is-uncommitted
                                              (not blame-reveal-show-uncommitted-fringe))))
          (unless (equal commit-hash blame-reveal--current-block-commit)
            (setq blame-reveal--current-block-commit commit-hash))
          (when (blame-reveal--header-needs-update-p commit-hash block-start)
            (blame-reveal--update-header-overlay commit-hash block-start color hide-header-fringe))
          (blame-reveal--schedule-temp-overlay-if-needed commit-hash color)
          (setq blame-reveal--last-rendered-commit commit-hash))))
    (blame-reveal--update-sticky-header)))

(defun blame-reveal--maybe-expand-blame-range ()
  "Expand blame data range if visible area extends beyond loaded range."
  (when-let* ((blame-reveal--blame-data-range)
              (range (blame-reveal--get-visible-line-range)))
    (pcase-let* ((`(,start-line . ,end-line) range)
                 (`(,current-start . ,current-end) blame-reveal--blame-data-range))
      (when (or (< start-line current-start)
                (> end-line current-end))
        (blame-reveal--ensure-range-loaded start-line end-line)))))

(defun blame-reveal--scroll-handler-impl (buf)
  "Implementation of scroll handler for buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and blame-reveal-mode (buffer-file-name))
        (condition-case err
            (progn
              (blame-reveal--maybe-expand-blame-range)
              (unless (and (eq blame-reveal--state-status 'loading)
                           (eq blame-reveal--state-operation 'expansion))
                (blame-reveal--render-visible-region)
                (blame-reveal--update-sticky-header)))
          (error
           (message "Error in scroll handler: %s" (error-message-string err))))))))

(defun blame-reveal--scroll-handler (_win _start)
  "Handle window scroll events with debouncing."
  (let ((current-start (window-start)))
    (unless (equal current-start blame-reveal--last-window-start)
      (setq blame-reveal--last-window-start current-start)
      (when blame-reveal--scroll-timer
        (cancel-timer blame-reveal--scroll-timer))
      (blame-reveal--clear-overlays-by-type 'fringe)
      (unless blame-reveal--in-transient-setup
        (blame-reveal--clear-header))
      (setq blame-reveal--scroll-timer
            (run-with-idle-timer
             blame-reveal--scroll-render-delay nil
             #'blame-reveal--scroll-handler-impl
             (current-buffer))))))

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
            ;; Force header refresh by clearing cache and re-triggering
            (setq blame-reveal--current-block-commit nil)
            (setq blame-reveal--last-rendered-commit nil)
            ;; Clear existing header using No-Flicker system
            (blame-reveal--clear-header)
            ;; Clear temp overlays
            (blame-reveal--clear-temp-overlays)
            ;; Re-trigger header update
            (blame-reveal--update-header)))))))

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

(defun blame-reveal--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  ;; Forcefully cancel the current operation and clean up
  (when (blame-reveal--state-is-busy-p)
    (message "[Update] Cancelling current operation...")
    ;; UI Cleanup
    (blame-reveal--stop-loading-animation)
    ;; State Cleanup
    (blame-reveal--state-cancel "full update requested"))
  ;; A brief delay to ensure cleanup is completed.
  (run-with-timer
   0.1 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (blame-reveal--clear-overlays-by-type 'fringe)
         ;; Reset buffer-specific data
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
