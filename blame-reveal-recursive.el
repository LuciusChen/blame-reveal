;;; blame-reveal-recursive.el --- Recursive blame navigation -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Recursive blame navigation extension for blame-reveal.
;;
;; Features:
;; - Navigate through line history by recursively blaming previous revisions
;; - Blame stack for navigating back through history
;; - Arbitrary revision blame (any commit/branch/tag)
;; - Mode line indicator showing current revision
;; - Async support using main package settings
;; - Smart error detection (initial commit vs real errors)
;;
;; Keybindings:
;;   b   - Recursively blame (go to parent commit)
;;   p/^ - Go back to previous revision
;;   g   - Blame at specific revision
;;   r   - Reset to HEAD

;;; Code:

(require 'blame-reveal)
(require 'blame-reveal-overlay)
(require 'cl-lib)

;;; Macros

(defmacro blame-reveal--with-git-env (&rest body)
  "Execute BODY with clean git process environment.
Sets GIT_PAGER=cat and PAGER=cat for consistent parsing."
  `(let ((process-environment (cons "GIT_PAGER=cat"
                                    (cons "PAGER=cat" process-environment))))
     ,@body))

;;; Helper Functions: Git Operations

(defun blame-reveal--git-file-exists-p (revision file)
  "Check if FILE exists at REVISION."
  (let ((git-root (vc-git-root file)))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root)))
        (blame-reveal--with-git-env
         (with-temp-buffer
           (zerop (call-process "git" nil nil nil "cat-file" "-e"
                                (concat revision ":" relative-file)))))))))

(defun blame-reveal--is-initial-commit-p (commit-hash)
  "Check if COMMIT-HASH is an initial commit (no parent)."
  (blame-reveal--with-git-env
   (with-temp-buffer
     (not (zerop (call-process "git" nil nil nil "rev-parse"
                               "--verify" "--quiet"
                               (concat commit-hash "^")))))))

(defun blame-reveal--get-base-commit (revision)
  "Extract base commit hash from parent reference REVISION.
For example, 'abc123^' returns 'abc123'."
  (when (string-match "^\\([a-f0-9]+\\)\\^+$" revision)
    (match-string 1 revision)))

(defun blame-reveal--get-short-info (commit-hash)
  "Get short info string for COMMIT-HASH (for display)."
  (blame-reveal--with-git-env
   (with-temp-buffer
     (when (zerop (call-process "git" nil t nil "show"
                                "--no-patch" "--format=%h %s" commit-hash))
       (string-trim (buffer-string))))))

;;; Helper Functions: Move/Copy Metadata

(defun blame-reveal--get-previous-location (commit-hash)
  "Get previous file location for COMMIT-HASH if available.
Returns (PREV-FILE . PREV-COMMIT) or nil."
  (when (and blame-reveal--move-copy-metadata
             (hash-table-p blame-reveal--move-copy-metadata))
    (when-let* ((metadata (gethash commit-hash blame-reveal--move-copy-metadata)))
      (let ((prev-file (plist-get metadata :previous-file))
            (prev-commit (plist-get metadata :previous-commit)))
        (when (and prev-file prev-commit)
          (cons prev-file prev-commit))))))

(defun blame-reveal--format-prev-info (prev-file prev-commit)
  "Format previous location info for display."
  (format "%s at %s"
          (propertize (file-name-nondirectory prev-file)
                      'face 'font-lock-string-face)
          (propertize (substring prev-commit 0 8)
                      'face 'font-lock-constant-face)))

;;; Data Structure Management

(defun blame-reveal--reset-data-structures (blame-data)
  "Reset all data structures with new BLAME-DATA."
  (setq blame-reveal--blame-data blame-data)
  (setq blame-reveal--blame-data-range nil)
  (blame-reveal--reset-all-caches)
  ;; Reset move/copy metadata
  (blame-reveal--ensure-move-copy-metadata))

;;; State Management

(defun blame-reveal--save-current-state ()
  "Save current blame state to stack.
On first recursive blame from HEAD, also saves initial HEAD state at bottom of stack."
  ;; Special case: First recursive blame from HEAD
  ;; Save a pristine HEAD state at the bottom of the stack for smooth reset
  (when (and (null blame-reveal--blame-stack)
             (null blame-reveal--current-revision))
    (let ((head-state (list :revision nil
                            :revision-display nil
                            :detect-moves blame-reveal--detect-moves
                            :blame-data (when blame-reveal--blame-data
                                          (copy-sequence blame-reveal--blame-data))
                            :blame-data-range blame-reveal--blame-data-range
                            :commit-info (when (hash-table-p blame-reveal--commit-info)
                                           (copy-hash-table blame-reveal--commit-info))
                            :color-map (when (hash-table-p blame-reveal--color-map)
                                         (copy-hash-table blame-reveal--color-map))
                            ;; timestamps is a cons cell (min . max), not a list
                            :timestamps (when blame-reveal--timestamps
                                          (cons (car blame-reveal--timestamps)
                                                (cdr blame-reveal--timestamps)))
                            :recent-commits (when blame-reveal--recent-commits
                                              (copy-sequence blame-reveal--recent-commits))
                            :move-copy-metadata (when (hash-table-p blame-reveal--move-copy-metadata)
                                                  (copy-hash-table blame-reveal--move-copy-metadata))
                            :window-start (window-start)
                            :point (point)
                            :is-head-state t)))
      ;; Initialize stack with HEAD state as foundation
      (setq blame-reveal--blame-stack (list head-state))))

  ;; Save current state before transitioning
  (push (list :revision blame-reveal--current-revision
              :revision-display blame-reveal--revision-display
              :detect-moves blame-reveal--detect-moves
              :blame-data blame-reveal--blame-data
              :blame-data-range blame-reveal--blame-data-range
              :commit-info (when (hash-table-p blame-reveal--commit-info)
                             (copy-hash-table blame-reveal--commit-info))
              :color-map (when (hash-table-p blame-reveal--color-map)
                           (copy-hash-table blame-reveal--color-map))
              ;; timestamps is a cons cell (min . max)
              :timestamps (when blame-reveal--timestamps
                            (cons (car blame-reveal--timestamps)
                                  (cdr blame-reveal--timestamps)))
              :move-copy-metadata (when (hash-table-p blame-reveal--move-copy-metadata)
                                    (copy-hash-table blame-reveal--move-copy-metadata))
              :recent-commits blame-reveal--recent-commits
              :window-start (window-start)
              :point (point))
        blame-reveal--blame-stack))

(defun blame-reveal--restore-state (state)
  "Restore blame state from STATE."
  (setq blame-reveal--current-revision (plist-get state :revision)
        blame-reveal--revision-display (plist-get state :revision-display)
        blame-reveal--blame-data (plist-get state :blame-data)
        blame-reveal--blame-data-range (plist-get state :blame-data-range))
  (setq blame-reveal--commit-info
        (blame-reveal--ensure-hash-table (plist-get state :commit-info))
        blame-reveal--color-map
        (blame-reveal--ensure-hash-table (plist-get state :color-map)))
  (setq blame-reveal--timestamps (plist-get state :timestamps)
        blame-reveal--recent-commits (plist-get state :recent-commits)
        blame-reveal--move-copy-metadata
        (blame-reveal--ensure-hash-table (plist-get state :move-copy-metadata)))
  (blame-reveal--smooth-transition-render)
  ;; Safely restore point and window-start
  (let ((saved-point (plist-get state :point))
        (saved-window-start (plist-get state :window-start)))
    (when (and saved-point
               (>= saved-point (point-min))
               (<= saved-point (point-max)))
      (goto-char saved-point))
    (when (and saved-window-start
               (>= saved-window-start (point-min))
               (<= saved-window-start (point-max)))
      (set-window-start nil saved-window-start))))

;;; Smooth Transition Rendering (Performance Critical)

(defun blame-reveal--smooth-transition-render ()
  "Render new blame data using flicker-free update system.
This function is now simplified - all flicker prevention is handled
by the unified no-flicker system."
  (when blame-reveal--blame-data
    ;; Use atomic update to prevent flicker
    (blame-reveal--with-no-flicker
     ;; Render visible region
     (let* ((range (blame-reveal--get-visible-line-range))
            (start-line (car range))
            (end-line (cdr range)))
       ;; Ensure commit info and render
       (let ((blocks (blame-reveal--find-block-boundaries
                      blame-reveal--blame-data start-line end-line)))
         (dolist (block blocks)
           (blame-reveal--ensure-commit-info (nth 1 block))))
       (blame-reveal--update-recent-commits)
       (blame-reveal--render-visible-region))
     ;; Update header (which internally handles sticky header)
     ;; Note: update-header-impl calls update-sticky-header, so we don't call it separately
     (blame-reveal--update-header))))

;;; Error Handling

(defun blame-reveal--handle-load-error (revision file)
  "Handle error when loading blame at REVISION for FILE.
Restores previous state and provides context-aware error messages."
  (let* ((base-commit (blame-reveal--get-base-commit revision))
         (is-initial (and base-commit
                          (blame-reveal--is-initial-commit-p base-commit)))
         (file-exists (and base-commit
                           (blame-reveal--git-file-exists-p base-commit file))))
    ;; First restore to previous state
    (condition-case restore-err
        (when blame-reveal--blame-stack
          (let ((state (pop blame-reveal--blame-stack)))
            (blame-reveal--restore-state state)))
      (error
       (message "Failed to restore state: %s. Resetting to HEAD."
                (error-message-string restore-err))
       (blame-reveal-reset-to-head)
       (cl-return-from blame-reveal--handle-load-error nil)))
    ;; Then provide context-aware error message
    (unwind-protect
        (cond
         ;; Case 1: Initial commit
         (is-initial
          (blame-reveal--state-error
           (format "Reached initial commit %s" (substring base-commit 0 8)))
          (message "Reached initial commit %s - this is where the repository started"
                   (substring base-commit 0 8)))

         ;; Case 2: File doesn't exist at this revision
         ((and base-commit (not file-exists))
          (blame-reveal--state-error
           (format "File doesn't exist at %s" (substring base-commit 0 8)))
          (if blame-reveal--detect-moves
              (message "File doesn't exist at commit %s" (substring base-commit 0 8))
            (message "File doesn't exist at commit %s. Tip: Press [M] to enable move/copy detection"
                     (substring base-commit 0 8))))
         ;; Case 3: Other errors
         (t
          (blame-reveal--state-error (format "No blame data at %s" revision))
          (if blame-reveal--detect-moves
              (message "No blame data at revision %s" revision)
            (message "No blame data at revision %s. Tip: Press [M] to enable move/copy detection"
                     revision))))
      ;; Ensure state is reset in all cases
      (run-with-timer 0.2 nil
                      (lambda ()
                        (when (eq blame-reveal--state-status 'error)
                          (setq blame-reveal--state-status 'idle
                                blame-reveal--state-operation nil
                                blame-reveal--state-mode nil
                                blame-reveal--state-metadata nil
                                blame-reveal--process-id nil)))))))

;;; File Following

(defun blame-reveal--follow-to-previous-file (file commit)
  "Follow blame to FILE at COMMIT (previous location).
Returns non-nil on success, nil if user cancels."
  (let* ((git-root (vc-git-root (buffer-file-name)))
         (full-path (expand-file-name file git-root)))
    (cond
     ;; File still exists
     ((file-exists-p full-path)
      (find-file full-path)
      (unless blame-reveal-mode
        (blame-reveal-mode 1))
      (blame-reveal-blame-at-revision commit)
      (message "Following to %s at %s"
               (file-name-nondirectory file)
               (substring commit 0 8))
      t)

     ;; File deleted, offer to view historical version
     (t
      (if (y-or-n-p (format "File %s no longer exists. View historical version? "
                            (file-name-nondirectory file)))
          (progn
            (blame-reveal--view-file-at-revision file commit git-root)
            t)
        (message "Staying at current location")
        nil)))))

(defun blame-reveal--view-file-at-revision (relative-file commit git-root)
  "View the historical content of RELATIVE-FILE at COMMIT in a new buffer.

This function handles the crucial step of setting the Major Mode for
non-standard buffers (like *file.el @ hash*), ensuring syntax highlighting.

Returns the new buffer on success, nil on failure."
  (let* ((buffer-name (format "*%s @ %s*"
                              (file-name-nondirectory relative-file)
                              (substring commit 0 blame-reveal--short-hash-length)))
         ;; Create a mock file name (e.g., 'init-layout.el') for mode identification
         (mock-file-name (file-name-nondirectory relative-file))
         (new-buffer (get-buffer-create buffer-name)))
    (with-current-buffer new-buffer
      (let ((inhibit-read-only t)
            (original-buffer-file-name buffer-file-name))
        (erase-buffer)
        ;; 1. TEMPORARILY set 'buffer-file-name'.
        ;; This is the critical step for 'normal-mode' to recognize the file type.
        (setq buffer-file-name mock-file-name)
        ;; Use git show to fetch content and insert into buffer
        (blame-reveal--with-git-env
          (call-process "git" nil t nil "show" (format "%s:%s" commit relative-file)))
        (goto-char (point-min))
        ;; 2. FORCE Major Mode check (Syntax Highlighting)
        (normal-mode)
        ;; 3. CLEANUP: Reset 'buffer-file-name' to original (likely nil)
        ;; to prevent the user from accidentally saving the historical content.
        (setq buffer-file-name original-buffer-file-name)
        ;; 4. Set read-only and display
        (setq buffer-read-only t)
        (pop-to-buffer (current-buffer))
        (message "Viewing historical file: %s in %s" relative-file major-mode)
        new-buffer))))

;;; Asynchronous Loading

(defun blame-reveal--load-blame-async (revision)
  "Load blame data at REVISION asynchronously."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file)))
         (source-buffer (current-buffer)))
    (unless git-root
      (user-error "File is not in a git repository"))
    (unless (blame-reveal--state-start 'recursive 'async
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))

    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (temp-buffer (generate-new-buffer " *blame-recursive*"))
           (args (blame-reveal--build-blame-command-args
                  nil nil relative-file revision)))

      (blame-reveal--with-git-env
       (message "Git command: git %s" (mapconcat 'identity args " "))
       (blame-reveal--state-set-async-resources
        (make-process
         :name "blame-recursive"
         :buffer temp-buffer
         :command (cons "git" args)
         :sentinel (blame-reveal--make-async-sentinel
                    source-buffer temp-buffer
                    (lambda (temp-buf)
                      (blame-reveal--handle-async-complete
                       temp-buf revision file))
                    (lambda ()
                      (blame-reveal--handle-load-error revision file)))
         :noquery t)
        temp-buffer)
       ;; Store source file for verification
       (process-put blame-reveal--state-process 'source-file file)))))

(defun blame-reveal--handle-async-complete (temp-buffer revision file)
  "Handle completion of recursive async blame loading from TEMP-BUFFER."
  (unless (and (eq blame-reveal--state-status 'loading)
               (eq blame-reveal--state-operation 'recursive))
    (message "[State] Unexpected recursive complete in state %s/%s"
             blame-reveal--state-status blame-reveal--state-operation)
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (cl-return-from blame-reveal--handle-async-complete nil))

  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (condition-case err
            (progn
              (blame-reveal--state-transition 'processing)
              (let* ((git-root (vc-git-root file))
                     (relative-file (file-relative-name file git-root))
                     (result (blame-reveal--parse-blame-output
                              temp-buffer
                              relative-file))
                     (blame-data (car result))
                     (move-metadata (cdr result)))
                (if blame-data
                    (blame-reveal--finalize-load revision blame-data move-metadata)
                  (blame-reveal--state-error "Failed to parse blame data")
                  (blame-reveal--handle-load-error revision file))))
          (error
           (blame-reveal--state-error (format "Recursive load error: %s" (error-message-string err)))
           (blame-reveal--handle-load-error revision file))))
    ;; Always clean up temp buffer
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))))

;;; Synchronous Loading

(defun blame-reveal--get-blame-data-sync (revision file &optional range)
  "Get git blame data for FILE at REVISION synchronously.
If RANGE is (START-LINE . END-LINE), only blame that range.
REVISION can be commit hash or 'uncommitted for working tree.
Returns (BLAME-DATA . MOVE-METADATA)."
  (let ((git-root (vc-git-root file)))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root)))
        (blame-reveal--with-git-env
         (with-temp-buffer
           (let* ((args (blame-reveal--build-blame-command-args
                         (when range (car range))
                         (when range (cdr range))
                         relative-file
                         revision))
                  (exit-code (apply #'call-process "git" nil t nil args)))
             (message "Git command: git %s (exit: %d)" (mapconcat 'identity args " ") exit-code)
             (when (not (zerop exit-code))
               (message "Git error output: %s" (buffer-string)))
             (when (zerop exit-code)
               (blame-reveal--parse-blame-output
                (current-buffer)
                relative-file)))))))))

(defun blame-reveal--load-blame-sync (revision)
  "Load blame data at REVISION synchronously.
Always loads complete file for proper recursive blame navigation."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "No file associated with buffer"))
    (unless (blame-reveal--state-start 'recursive 'sync
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))

    (condition-case err-outer
        (let* ((result (blame-reveal--get-blame-data-sync revision file))
               (blame-data (car result))
               (move-metadata (cdr result)))
          (if blame-data
              (blame-reveal--finalize-load revision blame-data move-metadata)
            (blame-reveal--handle-load-error revision file)))
      (error
       (let ((err-data err-outer))
         (blame-reveal--state-error (error-message-string err-data))
         (message "Error during recursive blame: %s" (error-message-string err-data))
         (condition-case restore-err
             (when blame-reveal--blame-stack
               (let ((state (pop blame-reveal--blame-stack)))
                 (blame-reveal--restore-state state)))
           (error
            (message "Failed to restore after error: %s"
                     (error-message-string restore-err))
            (blame-reveal-reset-to-head)))
         (signal (car err-data) (cdr err-data)))))))

;;; Common Finalization

(defun blame-reveal--finalize-load (revision blame-data move-metadata)
  "Common success handler for sync and async loading."
  (blame-reveal--state-transition 'processing)
  (setq blame-reveal--current-revision revision)
  (setq blame-reveal--revision-display
        (if (eq revision 'uncommitted)
            "Working Tree"
          (blame-reveal--get-short-info revision)))

  (blame-reveal--reset-data-structures blame-data)

  ;; Save move/copy metadata
  (when (hash-table-p move-metadata)
    (setq blame-reveal--move-copy-metadata move-metadata)
    (when (> (hash-table-count move-metadata) 0)
      (message "Detected %d commits with move/copy history"
               (hash-table-count move-metadata))))

  (blame-reveal--state-transition 'rendering)
  (blame-reveal--clear-overlays-by-type 'fringe)
  (blame-reveal--load-commits-incrementally)
  (blame-reveal--smooth-transition-render)
  (message "Loaded blame at %s (%d lines)"
           (or blame-reveal--revision-display revision)
           (length blame-data))
  (blame-reveal--state-complete))

;;; Unified Interface

(defun blame-reveal--load-blame-at-revision (revision)
  "Load blame data at REVISION using sync or async based on configuration."
  (if (blame-reveal--should-use-async-p)
      (blame-reveal--load-blame-async revision)
    (blame-reveal--load-blame-sync revision)))

;;; Recursive Blame Decision Logic

(defun blame-reveal--analyze-recursive-target (commit-hash)
  "Analyze where to navigate from COMMIT-HASH.
Returns a plist with:
  :action - Symbol: 'stop, 'follow-file, or 'blame-parent
  :message - String to display to user
  :target-file - File to follow (for 'follow-file action)
  :target-commit - Commit to blame (for 'follow-file or 'blame-parent)"
  (let* ((root (vc-git-root (buffer-file-name)))
         (current-file (file-relative-name (buffer-file-name) root))
         (prev-loc (blame-reveal--get-previous-location commit-hash))
         (prev-file (car prev-loc))
         (prev-commit (cdr prev-loc)))

    (cond
     ;; Case 1: Line newly added (same commit in metadata)
     ((and prev-commit (string= prev-commit commit-hash))
      (list :action 'stop
            :message (if (and prev-file (not (string= prev-file current-file)))
                         (format "Line originated from %s in commit %s (boundary)"
                                 prev-file (substring commit-hash 0 8))
                       (format "Line was newly added in commit %s"
                               (substring commit-hash 0 8)))))

     ;; Case 2: Cross-file move/copy
     ((and prev-file (not (string= prev-file current-file)))
      (list :action 'follow-file
            :message (format "Line from %s. Follow? "
                             (blame-reveal--format-prev-info prev-file prev-commit))
            :target-file prev-file
            :target-commit prev-commit))

     ;; Case 3: Same file, different commit (normal history)
     (prev-commit
      (let ((parent (concat prev-commit "^")))
        (list :action 'blame-parent
              :message (format "Recursive blame: %s -> %s"
                               (substring prev-commit 0 8) parent)
              :target-commit parent)))

     ;; Case 4: No metadata - check if file was created in this commit
     (t
      (let ((parent (concat commit-hash "^")))
        (if (blame-reveal--git-file-exists-p parent (buffer-file-name))
            ;; File exists in parent
            (list :action 'blame-parent
                  :message (format "Recursive blame: %s -> %s"
                                   (substring commit-hash 0 8) parent)
                  :target-commit parent)
          ;; File created in this commit
          (list :action 'stop
                :message (format "File was created in commit %s"
                                 (substring commit-hash 0 8)))))))))

(defun blame-reveal--execute-recursive-action (action-info)
  "Execute recursive blame action based on ACTION-INFO plist.
Returns non-nil if action was executed, nil if stopped/cancelled."
  (let ((action (plist-get action-info :action))
        (message-text (plist-get action-info :message)))

    (pcase action
      ;; Stop: display message with optional tip
      ('stop
       (if blame-reveal--detect-moves
           (message "%s" message-text)
         ;; Add tip about M/C detection when not enabled
         (message "%s. Tip: Try enabling move/copy detection [M] to trace file origin"
                  message-text))
       nil)

      ;; Follow to different file
      ('follow-file
       (if (y-or-n-p message-text)
           (progn
             (blame-reveal--follow-to-previous-file
              (plist-get action-info :target-file)
              (plist-get action-info :target-commit))
             t)
         (message "Staying at current location")
         nil))

      ;; Blame parent commit
      ('blame-parent
       (message "%s" message-text)
       (blame-reveal--load-blame-at-revision
        (plist-get action-info :target-commit))
       t)

      ;; Unknown action
      (_
       (error "Unknown recursive action: %s" action)))))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-blame-recursively ()
  "Recursively blame the commit before the one at current line."
  (interactive)
  ;; Pre-flight checks
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
  (when (blame-reveal--state-is-busy-p)
    (user-error "Please wait for current operation to complete"))

  (let* ((current-block (blame-reveal--get-current-block))
         (commit-hash (car current-block)))

    ;; Validate commit
    (unless commit-hash
      (user-error "No commit at current line"))
    (when (blame-reveal--is-uncommitted-p commit-hash)
      (user-error "Cannot recursively blame uncommitted changes"))

    ;; Save state before any navigation
    (blame-reveal--save-current-state)

    ;; Analyze where to go and execute
    (condition-case err
        (let ((action-info (blame-reveal--analyze-recursive-target commit-hash)))
          (unless (blame-reveal--execute-recursive-action action-info)
            ;; Action was stopped/cancelled, pop the saved state
            (pop blame-reveal--blame-stack)))
      (error
       ;; Restore state on error
       (let ((state (pop blame-reveal--blame-stack)))
         (when state (blame-reveal--restore-state state)))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun blame-reveal-blame-back ()
  "Go back to previous blame state in the recursion stack, without flashing."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (when (blame-reveal--state-is-busy-p)
    (blame-reveal--state-cancel "user navigated back"))

  (if (null blame-reveal--blame-stack)
      (message "Already at the newest revision")
    (let ((state (pop blame-reveal--blame-stack)))
      (message "Returned to %s"
               (blame-reveal--restore-state state)))))

;;;###autoload
(defun blame-reveal-blame-at-revision (revision)
  "Show blame at a specific REVISION (commit/branch/tag)."
  (interactive "sBlame at revision (commit/branch/tag): ")
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
  (when (blame-reveal--state-is-busy-p)
    (user-error "Please wait for current operation to complete"))
  (when (string-empty-p (string-trim revision))
    (user-error "Revision cannot be empty"))

  ;; Verify revision is valid
  (blame-reveal--with-git-env
   (with-temp-buffer
     (unless (zerop (call-process "git" nil t nil "rev-parse" "--verify"
                                  (concat revision "^{commit}")))
       (user-error "Invalid revision: %s" revision))))

  (blame-reveal--save-current-state)
  (message "Blame at revision: %s" revision)
  (condition-case err
      (blame-reveal--load-blame-at-revision revision)
    (error
     (message "Error in blame at revision: %s" (error-message-string err))
     (let ((state (pop blame-reveal--blame-stack)))
       (when state
         (blame-reveal--restore-state state)))
     (signal (car err) (cdr err)))))

;;;###autoload
(defun blame-reveal-reset-to-head ()
  "Reset blame to HEAD (newest revision), clearing the stack.
Uses smooth transition to avoid flashing."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
  (when (blame-reveal--state-is-busy-p)
    (blame-reveal--state-cancel "reset to HEAD"))

  (cond
   ;; Case 1: Currently viewing a historical revision
   (blame-reveal--current-revision
    ;; Look for HEAD state at bottom of stack
    (let ((head-state (car (last blame-reveal--blame-stack))))
      (if (and head-state (plist-get head-state :is-head-state))
          ;; Found HEAD state - restore data exactly like TEST A
          (progn
            (setq blame-reveal--blame-stack nil) ; 清空堆栈
            (setq blame-reveal--current-revision nil) ; HEAD 状态
            (setq blame-reveal--revision-display nil) ; HEAD 状态
            (setq blame-reveal--auto-days-cache nil) ; 重置缓存

            (blame-reveal--restore-state head-state) ; 调用抽象函数

            ;; Don't call any render functions - same as TEST A
            (message "Reset to HEAD"))

        ;; No HEAD state found - reload data
        (setq blame-reveal--blame-stack nil)
        (setq blame-reveal--current-revision nil)
        (setq blame-reveal--revision-display nil)
        (setq blame-reveal--auto-days-cache nil)
        (setq blame-reveal--blame-data nil
              blame-reveal--blame-data-range nil
              blame-reveal--commit-info nil
              blame-reveal--color-map nil
              blame-reveal--timestamps nil
              blame-reveal--recent-commits nil
              blame-reveal--move-copy-metadata nil
              blame-reveal--all-commits-loaded nil)
        (blame-reveal--load-blame-data)
        (message "Reset to HEAD (reloaded)"))))

   ;; Case 2: At HEAD but have stack
   (blame-reveal--blame-stack
    (setq blame-reveal--blame-stack nil)
    (message "Cleared blame stack"))

   ;; Case 3: Already at HEAD with empty stack
   (t
    (message "Already at HEAD"))))

;;; Utility Functions

(unless (fboundp 'copy-hash-table)
  (defun copy-hash-table (table)
    "Make a copy of hash TABLE.
If TABLE is not a hash-table, return a new empty hash-table."
    (if (hash-table-p table)
        (let ((new-table (make-hash-table
                          :test (hash-table-test table)
                          :size (hash-table-size table))))
          (maphash (lambda (key value)
                     (puthash key value new-table))
                   table)
          new-table)
      ;; Return empty hash-table if input is not a hash-table
      (make-hash-table :test 'equal))))

(provide 'blame-reveal-recursive)
;;; blame-reveal-recursive.el ends here
