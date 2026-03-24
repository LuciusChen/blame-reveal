;;; blame-reveal-focus.el --- Commit Focus Mode for blame-reveal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lucius Chen

;; Author: Lucius Chen
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (blame-reveal "0.5"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Commit Focus Mode for blame-reveal.
;;
;; This module provides a "focus mode" that allows users to lock onto a specific
;; commit and see all its modifications across the file at a glance.
;;
;; Features:
;; - Toggle focus mode to lock/unlock on a commit
;; - Highlight all lines belonging to the focused commit
;; - Keep a persistent focus badge in the header line
;; - Navigate between focused commit blocks
;; - Hide regular block/sticky headers while focus mode is active
;;
;; Implementation notes:
;; - Reuses existing overlay registry system (blame-reveal-overlay.el)
;; - Reuses existing header system (blame-reveal-header.el)
;; - Reuses existing color system (blame-reveal-color.el)
;; - Reuses existing block boundary detection (blame-reveal-core.el)
;;
;; Usage:
;;   M-x blame-reveal-focus-commit
;;   M-x blame-reveal-next-focus-block
;;   M-x blame-reveal-prev-focus-block

;;; Code:

(require 'cl-lib)
(require 'blame-reveal-core)
(require 'blame-reveal-ui)
(require 'blame-reveal-color)
(require 'blame-reveal-header)

(defvar blame-reveal-mode nil)
(defvar blame-reveal--focus-integration-refcount 0
  "Number of buffers currently using focus integration.")

;;; Customization

(defgroup blame-reveal-focus nil
  "Commit Focus Mode for blame-reveal."
  :group 'blame-reveal
  :prefix "blame-reveal-focus-")

(defcustom blame-reveal-focus-pulse-on-jump t
  "If non-nil, pulse the line after jumping to a focus block.
This provides visual feedback when navigating between blocks."
  :type 'boolean
  :group 'blame-reveal-focus)

(defcustom blame-reveal-focus-use-special-color nil
  "If non-nil, use a special bright color for focused commit fringe.
If nil, use the commit's normal color (from the gradient)."
  :type 'boolean
  :group 'blame-reveal-focus)

(defcustom blame-reveal-focus-color "#6699ff"
  "Color for the focused commit fringe when
`blame-reveal-focus-use-special-color' is non-nil."
  :type 'color
  :group 'blame-reveal-focus)

;;; Buffer-Local State Variables

(defvar-local blame-reveal--focused-commit nil
  "The commit hash that is currently focused/locked.
When non-nil, focus mode is active and only this commit's lines
are highlighted in the fringe.")

(defvar-local blame-reveal--focus-block-cache nil
  "Cached list of blocks belonging to the focused commit.
Each element is `(START-LINE COMMIT-HASH LENGTH)' from
`blame-reveal--find-block-boundaries'. Invalidated when focus changes.")

(defvar-local blame-reveal--focus-saved-header-line-format nil
  "Saved `header-line-format' value from before focus mode was entered.")

(defvar-local blame-reveal--focus-saved-header-line-format-p nil
  "Non-nil when `blame-reveal--focus-saved-header-line-format' is valid.")

(defvar-local blame-reveal--focus-header-line-watch-suppressed nil
  "Non-nil while focus mode is intentionally updating `header-line-format'.")

(defvar-local blame-reveal--focus-header-line-refresh-timer nil
  "Pending timer that restores the focus badge after external header updates.")

;;; Focus Mode State Management

(defun blame-reveal-focus--active-p ()
  "Return non-nil if focus mode is currently active."
  (and blame-reveal-mode
       blame-reveal--focused-commit))

(defun blame-reveal-focus--update-block-cache ()
  "Update the cached block list for the focused commit.
Reuses `blame-reveal--find-block-boundaries' from core module."
  (setq blame-reveal--focus-block-cache
        (when blame-reveal--focused-commit
          ;; Reuse existing block boundary detection
          (cl-remove-if-not
           (lambda (block)
             (equal (nth 1 block) blame-reveal--focused-commit))
           (blame-reveal--find-block-boundaries blame-reveal--blame-data)))))

(defun blame-reveal-focus--count-focused-lines ()
  "Count total lines belonging to the focused commit."
  (if blame-reveal--focus-block-cache
      (cl-reduce #'+ blame-reveal--focus-block-cache
                 :key (lambda (block) (nth 2 block)))  ; block-length
    0))

(defun blame-reveal-focus--get-color ()
  "Get the color to use for focused commit fringe."
  (if blame-reveal-focus-use-special-color
      blame-reveal-focus-color
    ;; Reuse existing color system
    (blame-reveal--get-commit-color blame-reveal--focused-commit)))

(defun blame-reveal-focus--header-line-prefix ()
  "Return padding that aligns the focus badge with buffer text.
This keeps the badge text starting after line numbers instead of at the
far-left edge of the header line."
  (let* ((line-number-width (if (bound-and-true-p display-line-numbers-mode)
                                (line-number-display-width)
                              0))
         (margin-width (or left-margin-width 0))
         (padding (+ line-number-width margin-width 1)))
    (propertize " " 'display `(space :align-to ,padding))))

(defun blame-reveal-focus--format-badge ()
  "Build a compact header-line badge for the active focused commit."
  (when blame-reveal--focused-commit
    (let* ((commit-hash blame-reveal--focused-commit)
           (short-hash (substring commit-hash 0 (min 7 (length commit-hash))))
           (commit-info (gethash commit-hash blame-reveal--commit-info))
           (summary (or (nth 3 commit-info) "Focused commit"))
           (block-count (length blame-reveal--focus-block-cache))
           (line-count (blame-reveal-focus--count-focused-lines))
           (label (propertize "Focus"
                              'face 'mode-line-emphasis))
           (details (propertize
                     (format ": %s  %s  (%d blocks, %d lines)"
                             short-hash summary block-count line-count)
                     'face 'header-line)))
      (concat (blame-reveal-focus--header-line-prefix)
              label
              details))))

(defun blame-reveal-focus--set-header-line-format (value)
  "Set `header-line-format' to VALUE without reprocessing our own writes."
  (let ((blame-reveal--focus-header-line-watch-suppressed t))
    (setq header-line-format value)))

(defun blame-reveal-focus--install-badge ()
  "Install the persistent focus badge into `header-line-format'."
  (unless blame-reveal--focus-saved-header-line-format-p
    (setq blame-reveal--focus-saved-header-line-format header-line-format
          blame-reveal--focus-saved-header-line-format-p t))
  (blame-reveal-focus--set-header-line-format
   (blame-reveal-focus--format-badge)))

(defun blame-reveal-focus--restore-header-line ()
  "Restore the pre-focus `header-line-format' value."
  (when blame-reveal--focus-saved-header-line-format-p
    (blame-reveal-focus--set-header-line-format
     blame-reveal--focus-saved-header-line-format)
    (setq blame-reveal--focus-saved-header-line-format nil
          blame-reveal--focus-saved-header-line-format-p nil)))

(defun blame-reveal-focus--watch-header-line-format (_symbol newval operation where)
  "Keep the focus badge stable when `header-line-format' changes externally.
NEWVAL, OPERATION, and WHERE follow `add-variable-watcher'."
  (when (and (eq operation 'set)
             (bufferp where)
             (buffer-live-p where))
    (with-current-buffer where
      (when (and blame-reveal--focus-saved-header-line-format-p
                 blame-reveal--focus-block-cache
                 (not blame-reveal--focus-header-line-watch-suppressed))
        (setq blame-reveal--focus-saved-header-line-format newval)
        (when blame-reveal--focus-header-line-refresh-timer
          (cancel-timer blame-reveal--focus-header-line-refresh-timer))
        (setq blame-reveal--focus-header-line-refresh-timer
              (run-with-timer
               0 nil
               (lambda (buffer)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq blame-reveal--focus-header-line-refresh-timer nil)
                     (when (and blame-reveal--focus-saved-header-line-format-p
                                blame-reveal--focus-block-cache)
                       (blame-reveal-focus--install-badge)))))
               where))))))

;;; Focus Mode Rendering (Reuses existing overlay system)

(defun blame-reveal-focus--render-focus-block (block vis-start vis-end color commit-hash rendered-lines)
  "Render BLOCK within VIS-START to VIS-END range using COLOR for COMMIT-HASH.
Records rendered lines in RENDERED-LINES hash table."
  (pcase-let* ((`(,block-start ,_ ,block-length) block)
               (block-end (+ block-start block-length -1)))
    (when (and (<= block-start vis-end)
               (>= block-end vis-start))
      (let ((render-start (max block-start vis-start))
            (render-end (min block-end vis-end)))
        (cl-loop for line from render-start to render-end
                 do (progn
                      (blame-reveal--create-fringe-overlay line color commit-hash)
                      (puthash line t rendered-lines)))))))

(defun blame-reveal-focus--render-visible-region ()
  "Render fringe overlays for focused commit in visible region.
Reuses `blame-reveal--create-fringe-overlay' from overlay module."
  (when (and blame-reveal--focused-commit
             blame-reveal--focus-block-cache)
    (pcase-let* ((range (blame-reveal--get-visible-line-range))
                 (`(,vis-start . ,vis-end) range)
                 (color (blame-reveal-focus--get-color))
                 (commit-hash blame-reveal--focused-commit)
                 (rendered-lines (make-hash-table :test 'eql)))
      ;; Render fringe for visible focused lines
      (mapc (lambda (block)
              (blame-reveal-focus--render-focus-block
               block vis-start vis-end color commit-hash rendered-lines))
            blame-reveal--focus-block-cache)
      ;; Clean up fringe overlays outside visible range
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when-let* (((overlay-buffer overlay))
                    (line (plist-get (blame-reveal--get-overlay-metadata overlay) :line))
                    ((not (gethash line rendered-lines))))
          (blame-reveal--unregister-overlay overlay))))))

;;; Focus Mode Entry/Exit

(defun blame-reveal-focus--enter (commit-hash)
  "Enter focus mode, locking onto COMMIT-HASH."
  ;; Set focus state
  (setq blame-reveal--focused-commit commit-hash)

  ;; Build block cache (reuses core function)
  (blame-reveal-focus--update-block-cache)

  ;; Clear existing fringe overlays (reuses overlay registry)
  (blame-reveal--clear-overlays-by-type 'fringe)
  (blame-reveal--clear-overlays-by-type 'temp-fringe)

  ;; Render focus overlays
  (blame-reveal-focus--render-visible-region)

  ;; Keep focus context visible even when point moves away.
  (blame-reveal-focus--install-badge)

  ;; Focus mode owns the top-of-window display and hides regular headers.
  (blame-reveal--clear-header-state)

  ;; Show summary
  (let ((block-count (length blame-reveal--focus-block-cache))
        (line-count (blame-reveal-focus--count-focused-lines))
        (commit-info (gethash commit-hash blame-reveal--commit-info)))
    (message "Focus mode: %s (%d blocks, %d lines) - Use C-c l f to exit, C-c l n/C-c l N to navigate"
             (if commit-info
                 (format "%s - %s"
                         (substring commit-hash 0 7)
                         (nth 3 commit-info))
               (substring commit-hash 0 7))
             block-count
             line-count)))

(defun blame-reveal-focus--exit ()
  "Exit focus mode and restore normal display."
  ;; Clear focus state
  (setq blame-reveal--focused-commit nil)
  (setq blame-reveal--focus-block-cache nil)

  ;; Restore any previous header-line state.
  (blame-reveal-focus--restore-header-line)

  (when blame-reveal--focus-header-line-refresh-timer
    (cancel-timer blame-reveal--focus-header-line-refresh-timer)
    (setq blame-reveal--focus-header-line-refresh-timer nil))

  ;; Clear current fringe overlays
  (blame-reveal--clear-overlays-by-type 'fringe)

  ;; Reset header tracking to force refresh
  (setq blame-reveal--current-block-commit nil
        blame-reveal--last-rendered-commit nil)

  ;; Re-render normal fringe overlays (reuses existing render function)
  (blame-reveal--render-visible-region)
  (blame-reveal--update-header)

  (message "Focus mode exited"))

;;; Focus Mode Integration

(defun blame-reveal-focus--find-block-containing-line (line)
  "Find the focus block containing LINE, or nil if not found."
  (cl-find-if
   (lambda (block)
     (pcase-let ((`(,start ,_ ,len) block))
       (and (>= line start)
            (< line (+ start len)))))
   blame-reveal--focus-block-cache))

;;; Block Navigation

(defun blame-reveal-focus--find-next-block (&optional backward)
  "Find the next (or previous if BACKWARD) focus block from current position.
Returns block (START-LINE COMMIT-HASH LENGTH) or nil if no more blocks."
  (when blame-reveal--focus-block-cache
    (let ((current-line (line-number-at-pos))
          (blocks (if backward
                      (reverse blame-reveal--focus-block-cache)
                    blame-reveal--focus-block-cache)))
      (cl-find-if
       (lambda (block)
         (let ((block-start (nth 0 block)))
           (if backward
               (< (+ block-start (nth 2 block) -1) current-line)
             (> block-start current-line))))
       blocks))))

(defun blame-reveal-focus--find-current-block ()
  "Find the block containing the current line, if any.
Returns block (START-LINE COMMIT-HASH LENGTH) or nil."
  (when blame-reveal--focus-block-cache
    (blame-reveal-focus--find-block-containing-line (line-number-at-pos))))

(defun blame-reveal-focus--navigate (backward wrap-message)
  "Move to the next focused block.
When BACKWARD is non-nil, move to the previous block instead.
WRAP-MESSAGE is shown when navigation wraps around."
  (unless (blame-reveal-focus--active-p)
    (user-error "Focus mode is not active. Use C-c l f to enter focus mode"))
  (let* ((candidate (blame-reveal-focus--find-next-block backward))
         (wrap-target (if backward
                          (car (last blame-reveal--focus-block-cache))
                        (car blame-reveal--focus-block-cache)))
         (current-block (blame-reveal-focus--find-current-block)))
    (cond
     (candidate
      (blame-reveal-focus--goto-block candidate))
     ((and wrap-target
           (not (equal wrap-target current-block)))
      (blame-reveal-focus--goto-block wrap-target)
      (message "%s" wrap-message))
     (t
      (message "Only one block in this file")))))

(defun blame-reveal-focus--goto-block (block)
  "Move point to the start of BLOCK and update display."
  (when block
    (let ((target-line (nth 0 block)))
      ;; Move to target line
      (goto-char (point-min))
      (forward-line (1- target-line))

      ;; Ensure line is visible
      (recenter)

      ;; Pulse if enabled
      (when (and blame-reveal-focus-pulse-on-jump
                 (fboundp 'pulse-momentary-highlight-one-line))
        (pulse-momentary-highlight-one-line (point) 'highlight))

      ;; Re-render overlays for new visible region
      (blame-reveal-focus--render-visible-region)

      ;; Update header
      (blame-reveal--update-header)

      ;; Show position info
      (let* ((block-index (1+ (cl-position block blame-reveal--focus-block-cache
                                           :test #'equal)))
             (total-blocks (length blame-reveal--focus-block-cache))
             (block-start (nth 0 block))
             (block-size (nth 2 block))
             (block-end (+ block-start block-size -1)))
        (message "Block %d/%d (lines %d-%d, %d lines)"
                 block-index total-blocks
                 block-start block-end block-size)))))

;;; Advice Functions for Integration

(defun blame-reveal-focus--around-render-visible-region (orig-fun &rest args)
  "Advice around `blame-reveal--render-visible-region' for focus mode.
When focus mode is active, render only focused commit overlays."
  (if (blame-reveal-focus--active-p)
      (blame-reveal-focus--render-visible-region)
    (apply orig-fun args)))

(defun blame-reveal-focus--around-update-header (orig-fun &rest args)
  "Advice around `blame-reveal--update-header' for focus mode.
Suppress regular headers while focus mode is active."
  (if (blame-reveal-focus--active-p)
      (blame-reveal--clear-header-state)
    (apply orig-fun args)))

(defun blame-reveal-focus--around-update-sticky-header (orig-fun &rest args)
  "Advice around `blame-reveal--update-sticky-header' for focus mode.
Suppress sticky headers while focus mode is active."
  (if (blame-reveal-focus--active-p)
      (blame-reveal--clear-header-state)
    (apply orig-fun args)))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-focus-commit ()
  "Toggle focus mode for the commit at current line.

When entering focus mode:
- All lines belonging to the current commit are highlighted
- Fringe indicators only show for the focused commit
- A persistent focus badge stays visible in the header line
- Regular block and sticky headers are hidden
- Use `C-c l n` and `C-c l N` to navigate between blocks

When exiting focus mode:
- Normal blame display is restored"
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (if (blame-reveal-focus--active-p)
      ;; Already in focus mode - toggle off
      (blame-reveal-focus--exit)
    ;; Not in focus mode - enter it
    ;; Reuse existing function to get commit at point
    (let* ((current-block (blame-reveal--get-current-block))
           (commit-hash (car current-block)))
      (unless commit-hash
        (user-error "No commit at current line"))
      (when (blame-reveal--is-uncommitted-p commit-hash)
        (user-error "Cannot focus on uncommitted changes"))
      (blame-reveal-focus--enter commit-hash))))

;;;###autoload
(defun blame-reveal-next-focus-block ()
  "Jump to the next block of the focused commit.
In focus mode, this navigates to the next occurrence of lines
modified by the locked commit."
  (interactive)
  (blame-reveal-focus--navigate nil "Wrapped to first block"))

;;;###autoload
(defun blame-reveal-prev-focus-block ()
  "Jump to the previous block of the focused commit.
In focus mode, this navigates to the previous occurrence of lines
modified by the locked commit."
  (interactive)
  (blame-reveal-focus--navigate t "Wrapped to last block"))

;;; Mode Setup/Teardown

(defun blame-reveal-focus--setup ()
  "Setup focus mode integration.
Called when blame-reveal-mode is enabled."
  (when (= blame-reveal--focus-integration-refcount 0)
    (advice-add 'blame-reveal--render-visible-region
                :around #'blame-reveal-focus--around-render-visible-region)
    (advice-add 'blame-reveal--update-header
                :around #'blame-reveal-focus--around-update-header)
    (advice-add 'blame-reveal--update-sticky-header
                :around #'blame-reveal-focus--around-update-sticky-header)
    (add-variable-watcher 'header-line-format
                          #'blame-reveal-focus--watch-header-line-format))
  (setq blame-reveal--focus-integration-refcount
        (1+ blame-reveal--focus-integration-refcount)))

(defun blame-reveal-focus--teardown ()
  "Teardown focus mode integration.
Called when blame-reveal-mode is disabled."
  ;; Clear focus state directly (don't use active-p which checks mode status,
  ;; because mode is already nil when off-hook is called)
  (when blame-reveal--focused-commit
    (setq blame-reveal--focused-commit nil
          blame-reveal--focus-block-cache nil))

  (blame-reveal-focus--restore-header-line)

  (when (> blame-reveal--focus-integration-refcount 0)
    (setq blame-reveal--focus-integration-refcount
          (1- blame-reveal--focus-integration-refcount)))

  (when (= blame-reveal--focus-integration-refcount 0)
    (advice-remove 'blame-reveal--render-visible-region
                   #'blame-reveal-focus--around-render-visible-region)
    (advice-remove 'blame-reveal--update-header
                   #'blame-reveal-focus--around-update-header)
    (advice-remove 'blame-reveal--update-sticky-header
                   #'blame-reveal-focus--around-update-sticky-header)
    (remove-variable-watcher 'header-line-format
                             #'blame-reveal-focus--watch-header-line-format)))

;;; Hooks Integration

(add-hook 'blame-reveal-mode-on-hook #'blame-reveal-focus--setup)
(add-hook 'blame-reveal-mode-off-hook #'blame-reveal-focus--teardown)

(provide 'blame-reveal-focus)
;;; blame-reveal-focus.el ends here
