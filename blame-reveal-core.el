;;; blame-reveal-core.el --- Core data structures and constants -*- lexical-binding: t; -*-

;;; Commentary:
;; Core data structures, constants, and buffer-local variables.
;; This module provides the foundation for the entire package.

;;; Code:

(require 'cl-lib)

;;; Constants

(defconst blame-reveal--state-idle 'idle)
(defconst blame-reveal--state-loading 'loading)
(defconst blame-reveal--state-processing 'processing)
(defconst blame-reveal--state-rendering 'rendering)
(defconst blame-reveal--state-error 'error)

(defconst blame-reveal--op-initial 'initial)
(defconst blame-reveal--op-expansion 'expansion)
(defconst blame-reveal--op-recursive 'recursive)

(defconst blame-reveal--overlay-types
  '(fringe header sticky-header temp-fringe loading)
  "All overlay types used by blame-reveal.")

;;; Data Structures

(cl-defstruct blame-reveal-commit-display
  "Formatted commit information for display."
  lines faces color)

(cl-defstruct blame-reveal-header-context
  "Context for building header overlay."
  commit-hash line-number mode show-fringe-p)

;;; Buffer-Local State Variables

(defvar-local blame-reveal--temp-overlay-timer nil
  "Timer for delayed temp overlay rendering.")

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--header-overlay nil
  "Overlay for the currently displayed header.")

(defvar-local blame-reveal--sticky-header-overlay nil
  "Overlay for sticky header at window top.")

(defvar-local blame-reveal--header-update-timer nil
  "Timer for delayed header update.")

(defvar-local blame-reveal--last-rendered-commit nil
  "Commit hash of the last rendered block (for smooth transition).")

(defvar-local blame-reveal--loading-animation-overlays nil
  "Overlays for loading animation.")

(defvar-local blame-reveal--loading-animation-step 0
  "Current step of loading animation.")

(defvar-local blame-reveal--loading-animation-frame-counter 0
  "Frame counter for animation step control.")

(defvar-local blame-reveal--loading-animation-sub-step 0
  "Sub-step for smooth interpolation (0.0 to 1.0).")

(defvar-local blame-reveal--color-strategy nil
  "Current color strategy instance.")

;;; Data Cache Variables

(defvar-local blame-reveal--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local blame-reveal--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defvar-local blame-reveal--blame-data nil
  "Cached blame data for the file.
List of (LINE-NUMBER . COMMIT-HASH) pairs.")

(defvar-local blame-reveal--blame-data-range nil
  "Range of currently loaded blame data: (START-LINE . END-LINE).
nil means entire file is loaded.")

(defvar-local blame-reveal--timestamps nil
  "Cached min/max timestamps for color calculation.")

(defvar-local blame-reveal--recent-commits nil
  "List of recent commit hashes (most recent first) to colorize.")

(defvar-local blame-reveal--auto-days-cache nil
  "Cached auto-calculated days limit.
Format: (COMMIT-COUNT . DAYS).
Cache is invalidated when commit count changes significantly.")

(defvar-local blame-reveal--last-window-start nil
  "Last window start position to detect scrolling.")

(defvar-local blame-reveal--all-commits-loaded nil
  "Flag indicating if all commits info has been loaded.")

;;; State Machine Variables

(defvar-local blame-reveal--state-status 'idle
  "Current loading state: idle/loading/processing/rendering/error.")

(defvar-local blame-reveal--state-operation nil
  "Current operation: initial/expansion/recursive/nil.")

(defvar-local blame-reveal--state-mode nil
  "Current operation mode: sync/async/nil.")

(defvar-local blame-reveal--state-metadata nil
  "Operation metadata plist: (:start-line N :end-line N :revision REV).")

(defvar-local blame-reveal--state-process nil
  "Current async process.")

(defvar-local blame-reveal--state-buffer nil
  "Current async buffer.")

;;; Recursive Blame Variables

(defvar-local blame-reveal--blame-stack nil
  "Stack for recursive blame history.
Each element is a plist with :revision, :line, :blame-data, :commit-info, etc.")

(defvar-local blame-reveal--current-revision nil
  "Current revision being blamed (nil = HEAD, 'uncommitted = working tree).")

(defvar-local blame-reveal--revision-display nil
  "Display string for current revision (for mode line).")

;;; Overlay Registry Variables

(defvar-local blame-reveal--overlay-registry nil
  "Unified registry for all overlays.
Hash table: overlay -> plist of metadata
  :type - one of `blame-reveal--overlay-types'
  :commit - commit hash (for fringe/temp-fringe/header)
  :line - line number (for positioning)
  :data - type-specific data")

(defvar-local blame-reveal--overlays-by-type nil
  "Index of overlays by type.
Hash table: type -> list of overlays")

(defvar-local blame-reveal--overlays-by-commit nil
  "Index of overlays by commit hash.
Hash table: commit -> list of overlays")

(defvar-local blame-reveal--overlays-by-line nil
  "Index of overlays by line number.
Hash table: line -> list of overlays")

;;; Utility Functions

(defsubst blame-reveal--is-uncommitted-p (commit-hash)
  "Check if COMMIT-HASH represents uncommitted changes."
  (string-match-p "^0+$" commit-hash))

(defun blame-reveal--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE)."
  (let* ((window-start (window-start))
         (window-end (window-end nil t))
         (start-line (max 1 (- (line-number-at-pos window-start)
                               blame-reveal-render-margin)))
         (end-line (+ (line-number-at-pos window-end)
                      blame-reveal-render-margin)))
    (cons start-line end-line)))

(defun blame-reveal--find-block-boundaries (blame-data &optional start-line end-line)
  "Find boundaries of same-commit blocks in BLAME-DATA.
If START-LINE and END-LINE are provided, only return blocks in that range.
Returns list of (START-LINE COMMIT-HASH BLOCK-LENGTH)."
  (let ((blocks nil)
        (current-commit nil)
        (block-start nil)
        (block-length 0))
    (dolist (entry blame-data)
      (let ((line (car entry))
            (commit (cdr entry)))
        (if (equal commit current-commit)
            (setq block-length (1+ block-length))
          (when current-commit
            (when (or (not start-line)
                      (and (>= (+ block-start block-length -1) start-line)
                           (<= block-start end-line)))
              (push (list block-start current-commit block-length) blocks)))
          (setq current-commit commit
                block-start line
                block-length 1))))
    (when current-commit
      (when (or (not start-line)
                (and (>= (+ block-start block-length -1) start-line)
                     (<= block-start end-line)))
        (push (list block-start current-commit block-length) blocks)))
    (nreverse blocks)))

(provide 'blame-reveal-core)
;;; blame-reveal-core.el ends here
