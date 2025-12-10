;;; blame-reveal-overlay.el --- Unified overlay management -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified overlay management system with registry and indexing.
;; Handles flicker-free updates, delayed deletion, and specific overlay types
;; like fringe, header, sticky-header, temp-fringe, and loading indicators.

;;; Code:

(require 'blame-reveal-core)

;;; Buffer-Local State Variables

(defvar-local blame-reveal--pending-delete-overlays nil
  "List of overlays pending deletion.
These overlays will be deleted after the next redisplay cycle to prevent flicker.")

(defvar-local blame-reveal--delete-timer nil
  "Timer for delayed overlay deletion.")

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

(defun blame-reveal--clear-overlays-by-commit (commit)
  "Clear all overlays for COMMIT immediately."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit))
    (blame-reveal--unregister-overlay overlay))
  (puthash commit nil blame-reveal--overlays-by-commit))

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
    ;; Cancel existing timer
    (when blame-reveal--delete-timer
      (cancel-timer blame-reveal--delete-timer))
    ;; Schedule deletion after next redisplay (using a short timer)
    (setq blame-reveal--delete-timer
          (run-with-timer 0.01 nil #'blame-reveal--execute-pending-deletions
                          (current-buffer)))))

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
      (setq blame-reveal--pending-delete-overlays nil)
      (setq blame-reveal--delete-timer nil))))

(defun blame-reveal--cancel-pending-deletions ()
  "Cancel all pending overlay deletions."
  (when blame-reveal--delete-timer
    (cancel-timer blame-reveal--delete-timer)
    (setq blame-reveal--delete-timer nil))
  (setq blame-reveal--pending-delete-overlays nil))

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

(defun blame-reveal--render-block-fringe (block-start block-length commit-hash color)
  "Render fringe for a specific block.
Returns list of created/reused overlays."
  (let ((overlays nil)
        (range (blame-reveal--get-visible-line-range))
        (block-end (+ block-start block-length -1)))
    (let ((render-start (max block-start (car range)))
          (render-end (min block-end (cdr range))))
      (when (<= render-start render-end)
        (dotimes (i (- render-end render-start -1))
          (let* ((line-num (+ render-start i))
                 (fringe-ov (blame-reveal--create-fringe-overlay
                              line-num color commit-hash)))
            (when fringe-ov
              (push fringe-ov overlays))))))
    overlays))

(defun blame-reveal--clear-fringe-overlays ()
  "Clear all fringe overlays immediately."
  (blame-reveal--clear-overlays-by-type 'fringe))

(defun blame-reveal--clear-fringe-overlays-for-commit (commit-hash)
  "Clear fringe overlays only for specific COMMIT-HASH immediately."
  (dolist (overlay (blame-reveal--get-overlays-by-commit commit-hash))
    (when (eq (blame-reveal--get-overlay-type overlay) 'fringe)
      (blame-reveal--unregister-overlay overlay))))

(defun blame-reveal--get-fringe-overlay-at-line (line-number)
  "Get fringe overlay at LINE-NUMBER, if any."
  (cl-find-if (lambda (ov)
                (eq (blame-reveal--get-overlay-type ov) 'fringe))
              (blame-reveal--get-overlays-by-line line-number)))

(defun blame-reveal--update-fringe-overlays-atomic (overlay-list)
  "Update multiple fringe overlays atomically.
OVERLAY-LIST is a list of (old-overlay . create-fn-with-args) pairs.
Returns list of new overlays."
  (let ((new-overlays nil))
    (blame-reveal--with-no-flicker
     (dolist (item overlay-list)
       (let* ((old-overlay (car item))
              (create-fn (cadr item))
              (args (cddr item))
              (new-overlay (apply create-fn args)))
         (push new-overlay new-overlays)
         (when old-overlay
           (blame-reveal--schedule-overlay-deletion old-overlay)))))
    (nreverse new-overlays)))

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
    (setq blame-reveal--sticky-header-overlay nil)))

(defun blame-reveal--clear-all-no-flicker ()
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

;;; Mode Cleanup

(defun blame-reveal--cleanup-no-flicker-system ()
  "Cleanup the no-flicker system when mode is disabled."
  (blame-reveal--cancel-pending-deletions))

(provide 'blame-reveal-overlay)
;;; blame-reveal-overlay.el ends here
