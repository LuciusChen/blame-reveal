;;; blame-reveal-core.el --- Core data structures and constants -*- lexical-binding: t; -*-

;;; Commentary:
;; Core data structures, constants, and buffer-local variables.
;; This module provides the foundation for the entire package.

;;; Code:

(require 'cl-lib)

;;; Constants

;; State Machine Constants


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

(defconst blame-reveal--short-hash-length 7
  "Length of abbreviated commit hash for display.")


;;; Extension Hooks

(defvar blame-reveal-before-load-hook nil
  "Hook run before loading git blame data.
Functions are called with no arguments.")

(defvar blame-reveal-after-load-hook nil
  "Hook run after git blame data is loaded successfully.
Functions are called with one argument: number of lines loaded.")

(defvar blame-reveal-before-render-hook nil
  "Hook run before rendering blame overlays.
Functions are called with two arguments: start-line and end-line.")

(defvar blame-reveal-after-render-hook nil
  "Hook run after rendering blame overlays.
Functions are called with two arguments: start-line and end-line.")

(defvar blame-reveal-mode-on-hook nil
  "Hook run when blame-reveal-mode is enabled.
Functions are called with no arguments.")

(defvar blame-reveal-mode-off-hook nil
  "Hook run when blame-reveal-mode is disabled.
Functions are called with no arguments.")

;;; Data Structures

(cl-defstruct blame-reveal-commit-display
  "Formatted commit information for display."
  lines faces color)

(cl-defstruct blame-reveal-header-context
  "Context for building header overlay."
  commit-hash line-number mode show-fringe-p)

;;; Buffer-Local State Variables

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--header-overlay nil
  "Overlay for the currently displayed header.")

(defvar-local blame-reveal--sticky-header-overlay nil
  "Overlay for sticky header at window top.")

(defvar-local blame-reveal--sticky-header-state nil
  "Cached state of sticky header for optimization.
A plist with :commit, :visible, and :window-start keys.
Used to avoid unnecessary sticky header recreation when state hasn't changed.")

(defvar-local blame-reveal--header-current-style nil
  "Current header style of the existing overlay.
Used to detect style changes that require overlay rebuild.")

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

(defvar-local blame-reveal--margin-width nil
  "Calculated margin width for current buffer in margin header style.")

(defvar-local blame-reveal--original-left-margin-width nil
  "Original left-margin-width before margin style was enabled.")

(defvar-local blame-reveal--original-right-margin-width nil
  "Original right-margin-width before margin style was enabled.")

(defvar-local blame-reveal--detect-moves nil
  "Whether current view is detecting moved/copied lines.
When non-nil, uses git blame -M -C.
Set by interactive command, not a persistent option.")

(defvar-local blame-reveal--move-copy-metadata nil
  "Hash table storing move/copy metadata for commits.
Maps commit-hash to plist with :previous-commit and :previous-file.")

(defvar-local blame-reveal--last-update-line nil
  "Last line number where header was updated.")

;;; Process Tracking (for concurrency safety)

(defvar-local blame-reveal--process-id nil
  "Unique ID for current async process.
Used to verify async callbacks belong to current operation.")


;;; Process ID Management

(defun blame-reveal--generate-process-id ()
  "Generate unique process ID."
  (format "%s-%d" (buffer-name) (float-time)))

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

(defvar-local blame-reveal--current-line-cache nil
  "Cache for current line's block info: (line-num commit-hash block-start).")

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

(defun blame-reveal--safe-line-number-at-pos (&optional pos)
  "Safely get line number at POS.
Returns nil if POS is invalid or out of range."
  (when (or (null pos)
            (and (>= pos (point-min))
                 (<= pos (point-max))))
    (line-number-at-pos pos)))

(defun blame-reveal--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE) or nil if unable to determine."
  (condition-case err
      (let* ((window-start (window-start))
             (window-end (window-end nil t))
             (max-line (blame-reveal--safe-line-number-at-pos (point-max)))
             (start-pos-line (blame-reveal--safe-line-number-at-pos window-start))
             (end-pos-line (blame-reveal--safe-line-number-at-pos window-end)))
        (when (and max-line start-pos-line end-pos-line)
          (let ((start-line (max 1 (- start-pos-line blame-reveal-render-margin)))
                (end-line (min max-line (+ end-pos-line blame-reveal-render-margin))))
            (cons start-line end-line))))
    (error
     (message "Error getting visible line range: %s" (error-message-string err))
     nil)))

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

(defun blame-reveal--ensure-hash-table (value &optional test)
  "Ensure VALUE is a hash table, creating empty one if needed.
TEST specifies the hash table test function (default: 'equal)."
  (if (hash-table-p value)
      value
    (make-hash-table :test (or test 'equal))))

(defun blame-reveal--ensure-move-copy-metadata ()
  "Ensure move-copy-metadata is initialized as a hash table."
  (unless (hash-table-p blame-reveal--move-copy-metadata)
    (setq blame-reveal--move-copy-metadata (make-hash-table :test 'equal)))
  blame-reveal--move-copy-metadata)

(defun blame-reveal--reset-all-caches ()
  "Reset all blame-reveal cache data structures to initial state."
  (setq blame-reveal--commit-info (make-hash-table :test 'equal)
        blame-reveal--color-map (make-hash-table :test 'equal)
        blame-reveal--timestamps nil
        blame-reveal--recent-commits nil
        blame-reveal--all-commits-loaded nil))


;;; State Machine Functions
;; These functions manage the lifecycle of async operations and ensure
;; proper cleanup and error handling.

(defun blame-reveal--state-can-start-p (operation)
  "Check if OPERATION can start in current state.
Returns (CAN-START-P . REASON)."
  (let ((status blame-reveal--state-status)
        (current-op blame-reveal--state-operation))
    (cond
     ((eq status 'idle)
      '(t . "idle"))
     ((and (eq status 'loading)
           (eq current-op operation))
      `(nil . ,(format "already loading %s" operation)))
     ((and (eq status 'loading)
           (eq current-op 'initial)
           (eq operation 'expansion))
      '(t . "upgrade initial->expansion"))
     ((eq operation 'recursive)
      '(t . "recursive cancels current"))
     (t
      `(nil . ,(format "busy: %s %s" status current-op))))))

(defun blame-reveal--state-start (operation mode &optional metadata)
  "Start OPERATION in MODE with optional METADATA.
Returns t if started successfully."
  (pcase-let ((`(,can-start . ,reason)
               (blame-reveal--state-can-start-p operation)))
    (if can-start
        (progn
          (when (not (eq blame-reveal--state-status 'idle))
            (blame-reveal--state-cancel reason))
          ;; Important: Ensure that the old process has been cleaned up before generating a new ID.
          (when blame-reveal--state-process
            (when (process-live-p blame-reveal--state-process)
              (delete-process blame-reveal--state-process))
            (setq blame-reveal--state-process nil))
          ;; Generate a new process ID
          (setq blame-reveal--process-id (blame-reveal--generate-process-id))
          (setq blame-reveal--state-status 'loading
                blame-reveal--state-operation operation
                blame-reveal--state-mode mode
                blame-reveal--state-metadata metadata
                blame-reveal--state-process nil
                blame-reveal--state-buffer nil)
          (blame-reveal--start-loading-animation)
          (message "[State] Start: %s (%s) - %s [ID: %s]"
                   operation mode reason blame-reveal--process-id)
          t)
      (message "[State] Cannot start %s: %s" operation reason)
      nil)))

(defun blame-reveal--state-set-async-resources (process buffer)
  "Set async PROCESS and BUFFER for current operation."
  ;; Ensure there is a process ID
  (unless blame-reveal--process-id
    (setq blame-reveal--process-id (blame-reveal--generate-process-id)))
  ;; Store process ID to the process object.
  (when (and process (processp process))
    (process-put process 'blame-reveal-process-id blame-reveal--process-id)
    (process-put process 'source-file (buffer-file-name)))
  (setq blame-reveal--state-process process
        blame-reveal--state-buffer buffer))

(defun blame-reveal--state-verify-process (process)
  "Verify PROCESS belongs to current operation."
  (and (processp process)
       ;; If the process is no longer in state-process, it means it has been replaced.
       (eq process blame-reveal--state-process)
       (let ((proc-id (process-get process 'blame-reveal-process-id))
             (proc-file (process-get process 'source-file)))
         (and proc-id
              blame-reveal--process-id
              (equal proc-id blame-reveal--process-id)
              (equal proc-file (buffer-file-name))))))

(defun blame-reveal--state-transition (new-status)
  "Transition to NEW-STATUS."
  (let ((old-status blame-reveal--state-status))
    (setq blame-reveal--state-status new-status)
    (message "[State] Transition: %s -> %s (op:%s)"
             old-status new-status blame-reveal--state-operation)))

(defun blame-reveal--state-cleanup-async ()
  "Cleanup async resources (process and buffer)."
  (when blame-reveal--state-process
    (when (process-live-p blame-reveal--state-process)
      (delete-process blame-reveal--state-process))
    (setq blame-reveal--state-process nil))
  (when blame-reveal--state-buffer
    (when (buffer-live-p blame-reveal--state-buffer)
      (kill-buffer blame-reveal--state-buffer))
    (setq blame-reveal--state-buffer nil)))

(defun blame-reveal--state-complete ()
  "Complete current operation successfully.
Ensures cleanup happens even if rendering fails."
  (unwind-protect
      (progn
        (blame-reveal--stop-loading-animation)
        (blame-reveal--state-cleanup-async))
    ;; Always reset state
    (setq blame-reveal--state-status 'idle
          blame-reveal--state-operation nil
          blame-reveal--state-mode nil
          blame-reveal--state-metadata nil
          blame-reveal--process-id nil))
  (message "[State] Complete"))

(defun blame-reveal--state-reset-internal ()
  "Reset all internal state variables and async resources (process/buffer).
DOES NOT handle UI elements or application timers."
  ;; Keep the loading animation stopped
  (ignore-errors (blame-reveal--stop-loading-animation))

  (blame-reveal--state-cleanup-async)

  (setq blame-reveal--state-status 'idle
        blame-reveal--state-operation nil
        blame-reveal--state-mode nil
        blame-reveal--state-metadata nil
        blame-reveal--process-id nil))

(defun blame-reveal--state-error (error-msg)
  "Handle error with ERROR-MSG. Performs emergency UI cleanup defensively."
  (message "[State] Error: %s" error-msg)
  (ignore-errors (blame-reveal--clear-header))

  (blame-reveal--state-reset-internal)
  (setq blame-reveal--state-status 'error)
  (run-with-timer 0.1 nil
                  (lambda ()
                    (when (eq blame-reveal--state-status 'error)
                      (setq blame-reveal--state-status 'idle)))))

(defun blame-reveal--state-cancel (reason)
  "Cancel current operation for REASON. Caller must clean up UI artifacts."
  (message "[State] Cancel: %s" reason)
  (blame-reveal--state-reset-internal))

(defun blame-reveal--state-is-busy-p ()
  "Check if state machine is busy."
  (not (eq blame-reveal--state-status 'idle)))

(provide 'blame-reveal-core)
;;; blame-reveal-core.el ends here
