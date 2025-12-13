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
;; - Hide blame info for non-focused commits
;; - Navigate between focused commit blocks
;; - Sticky header always shows focused commit info
;;
;; Implementation notes:
;; - Reuses existing overlay registry system (blame-reveal-overlay.el)
;; - Reuses existing header system (blame-reveal-header.el)
;; - Reuses existing color system (blame-reveal-color.el)
;; - Reuses existing block boundary detection (blame-reveal-core.el)
;;
;; Usage:
;;   M-x blame-reveal-focus-commit  (or press 'F' in blame-reveal-mode)
;;   M-x blame-reveal-next-focus-block (or press 'n' in focus mode)
;;   M-x blame-reveal-prev-focus-block (or press 'N' in focus mode)

;;; Code:

(require 'cl-lib)
(require 'blame-reveal-core)
(require 'blame-reveal-ui)
(require 'blame-reveal-color)
(require 'blame-reveal-header)

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
  "Color used for focused commit fringe when `blame-reveal-focus-use-special-color' is t."
  :type 'color
  :group 'blame-reveal-focus)

;;; Buffer-Local State Variables

(defvar-local blame-reveal--focused-commit nil
  "The commit hash that is currently focused/locked.
When non-nil, focus mode is active and only this commit's lines
are highlighted in the fringe.")

(defvar-local blame-reveal--focus-block-cache nil
  "Cached list of blocks belonging to the focused commit.
Each element is (START-LINE COMMIT-HASH LENGTH) from `blame-reveal--find-block-boundaries'.
Invalidated when focused commit changes.")

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

;;; Focus Mode Rendering (Reuses existing overlay system)

(defun blame-reveal-focus--render-visible-region ()
  "Render fringe overlays for focused commit in visible region.
Reuses `blame-reveal--create-fringe-overlay' from overlay module."
  (when (and blame-reveal--focused-commit
             blame-reveal--focus-block-cache)
    (let* ((range (blame-reveal--get-visible-line-range))
           (vis-start (car range))
           (vis-end (cdr range))
           (color (blame-reveal-focus--get-color))
           (commit-hash blame-reveal--focused-commit)
           (rendered-lines (make-hash-table :test 'eql)))
      ;; Render fringe for visible focused lines
      (dolist (block blame-reveal--focus-block-cache)
        (let* ((block-start (nth 0 block))
               (block-length (nth 2 block))
               (block-end (+ block-start block-length -1)))
          ;; Only render if block overlaps with visible range
          (when (and (<= block-start vis-end)
                     (>= block-end vis-start))
            (let ((render-start (max block-start vis-start))
                  (render-end (min block-end vis-end)))
              (cl-loop for line from render-start to render-end
                       do (progn
                            ;; Reuse existing fringe overlay creation
                            (blame-reveal--create-fringe-overlay line color commit-hash)
                            (puthash line t rendered-lines)))))))
      ;; Clean up fringe overlays outside visible range
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line (not (gethash line rendered-lines)))
              (blame-reveal--unregister-overlay overlay))))))))

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

  ;; Update header (will be intercepted by advice)
  (setq blame-reveal--current-block-commit commit-hash)
  (blame-reveal--update-header)

  ;; Show summary
  (let ((block-count (length blame-reveal--focus-block-cache))
        (line-count (blame-reveal-focus--count-focused-lines))
        (commit-info (gethash commit-hash blame-reveal--commit-info)))
    (message "Focus mode: %s (%d blocks, %d lines) - Press 'F' to exit, 'n/N' to navigate"
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

  ;; Clear current fringe overlays
  (blame-reveal--clear-overlays-by-type 'fringe)

  ;; Reset header tracking to force refresh
  (setq blame-reveal--current-block-commit nil
        blame-reveal--last-rendered-commit nil
        blame-reveal--last-rendered-block-start nil)

  ;; Re-render normal fringe overlays (reuses existing render function)
  (blame-reveal--render-visible-region)

  (message "Focus mode exited"))

;;; Focus Mode Header Integration

(defun blame-reveal-focus--get-current-block-for-header ()
  "Get block info for header display in focus mode.
Returns (COMMIT-HASH . BLOCK-START) like `blame-reveal--get-current-block'."
  (when blame-reveal--focused-commit
    (let* ((current-line (line-number-at-pos))
           ;; Find block containing current line
           (current-block
            (cl-find-if
             (lambda (block)
               (let ((start (nth 0 block))
                     (len (nth 2 block)))
                 (and (>= current-line start)
                      (< current-line (+ start len)))))
             blame-reveal--focus-block-cache)))
      (if current-block
          ;; In a focused block, return it
          (cons blame-reveal--focused-commit (nth 0 current-block))
        ;; Not in any focused block, keep header at previous position
        (when blame-reveal--header-overlay
          (cons blame-reveal--focused-commit
                (line-number-at-pos (overlay-start blame-reveal--header-overlay))))))))

(defun blame-reveal-focus--should-show-sticky-p ()
  "Check if sticky header should show in focus mode.
In focus mode, always show sticky header when regular header is not visible."
  (when (and blame-reveal--focused-commit
             blame-reveal--focus-block-cache)
    (let* ((current-line (line-number-at-pos))
           (window-start-line (line-number-at-pos (window-start)))
           ;; Find block containing current line
           (current-block
            (cl-find-if
             (lambda (block)
               (let ((start (nth 0 block))
                     (len (nth 2 block)))
                 (and (>= current-line start)
                      (< current-line (+ start len)))))
             blame-reveal--focus-block-cache)))
      (when current-block
        (let ((block-start (nth 0 current-block)))
          ;; Show sticky if block header is scrolled out of view
          (> window-start-line block-start))))))

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
    (let ((current-line (line-number-at-pos)))
      (cl-find-if
       (lambda (block)
         (let ((start (nth 0 block))
               (len (nth 2 block)))
           (and (>= current-line start)
                (< current-line (+ start len)))))
       blame-reveal--focus-block-cache))))

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

(defun blame-reveal-focus--around-get-current-block (orig-fun &rest args)
  "Advice around `blame-reveal--get-current-block' for focus mode.
When focus mode is active, always return focused commit info."
  (if (blame-reveal-focus--active-p)
      (blame-reveal-focus--get-current-block-for-header)
    (apply orig-fun args)))

(defun blame-reveal-focus--around-should-show-sticky-header-p (orig-fun &rest args)
  "Advice around `blame-reveal--should-show-sticky-header-p' for focus mode.
When focus mode is active, use focus-specific logic."
  (if (blame-reveal-focus--active-p)
      (blame-reveal-focus--should-show-sticky-p)
    (apply orig-fun args)))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-focus-commit ()
  "Toggle focus mode for the commit at current line.

When entering focus mode:
- All lines belonging to the current commit are highlighted
- Fringe indicators only show for the focused commit
- Header and sticky header always display focused commit info
- Use 'n' and 'N' to navigate between blocks

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
  (unless (blame-reveal-focus--active-p)
    (user-error "Focus mode is not active. Press 'F' to enter focus mode."))

  (let ((next-block (blame-reveal-focus--find-next-block)))
    (if next-block
        (blame-reveal-focus--goto-block next-block)
      ;; Wrap around to first block
      (let ((first-block (car blame-reveal--focus-block-cache)))
        (if (and first-block
                 (not (equal first-block (blame-reveal-focus--find-current-block))))
            (progn
              (blame-reveal-focus--goto-block first-block)
              (message "Wrapped to first block"))
          (message "Only one block in this file"))))))

;;;###autoload
(defun blame-reveal-prev-focus-block ()
  "Jump to the previous block of the focused commit.
In focus mode, this navigates to the previous occurrence of lines
modified by the locked commit."
  (interactive)
  (unless (blame-reveal-focus--active-p)
    (user-error "Focus mode is not active. Press 'F' to enter focus mode."))

  (let ((prev-block (blame-reveal-focus--find-next-block t)))
    (if prev-block
        (blame-reveal-focus--goto-block prev-block)
      ;; Wrap around to last block
      (let ((last-block (car (last blame-reveal--focus-block-cache))))
        (if (and last-block
                 (not (equal last-block (blame-reveal-focus--find-current-block))))
            (progn
              (blame-reveal-focus--goto-block last-block)
              (message "Wrapped to last block"))
          (message "Only one block in this file"))))))

;;; Mode Setup/Teardown

(defun blame-reveal-focus--setup ()
  "Setup focus mode integration.
Called when blame-reveal-mode is enabled."
  ;; Add advice for integration
  (advice-add 'blame-reveal--render-visible-region
              :around #'blame-reveal-focus--around-render-visible-region)
  (advice-add 'blame-reveal--get-current-block
              :around #'blame-reveal-focus--around-get-current-block)
  (advice-add 'blame-reveal--should-show-sticky-header-p
              :around #'blame-reveal-focus--around-should-show-sticky-header-p))

(defun blame-reveal-focus--teardown ()
  "Teardown focus mode integration.
Called when blame-reveal-mode is disabled."
  ;; Clear focus state directly (don't use active-p which checks mode status,
  ;; because mode is already nil when off-hook is called)
  (when blame-reveal--focused-commit
    (setq blame-reveal--focused-commit nil
          blame-reveal--focus-block-cache nil))

  ;; Remove advice
  (advice-remove 'blame-reveal--render-visible-region
                 #'blame-reveal-focus--around-render-visible-region)
  (advice-remove 'blame-reveal--get-current-block
                 #'blame-reveal-focus--around-get-current-block)
  (advice-remove 'blame-reveal--should-show-sticky-header-p
                 #'blame-reveal-focus--around-should-show-sticky-header-p))

;;; Keymap Extensions

;; Add focus mode commands to main keymap
(with-eval-after-load 'blame-reveal
  (define-key blame-reveal-mode-map (kbd "F") #'blame-reveal-focus-commit)
  (define-key blame-reveal-mode-map (kbd "n") #'blame-reveal-next-focus-block)
  (define-key blame-reveal-mode-map (kbd "N") #'blame-reveal-prev-focus-block))

;;; Hooks Integration

(add-hook 'blame-reveal-mode-on-hook #'blame-reveal-focus--setup)
(add-hook 'blame-reveal-mode-off-hook #'blame-reveal-focus--teardown)

(provide 'blame-reveal-focus)
;;; blame-reveal-focus.el ends here
