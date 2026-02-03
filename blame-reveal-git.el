;;; blame-reveal-git.el --- Git blame operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Git command execution, blame data parsing, and commit info fetching.
;; Handles both synchronous and asynchronous loading strategies.

;;; Code:

(require 'vc-git)
(require 'blame-reveal-core)

;;; Git Command Building

(defun blame-reveal--build-blame-command-args (start-line end-line relative-file &optional revision)
  "Build git blame command arguments.
START-LINE and END-LINE specify optional line range.
RELATIVE-FILE is the file path relative to git root.
REVISION is optional revision to blame (defaults to current-revision or HEAD)."
  (let* ((args (list "blame" "--porcelain"))
         (target-revision (or revision
                             (and (boundp 'blame-reveal--current-revision)
                                  blame-reveal--current-revision))))
    ;; Add move/copy detection flags
    (when blame-reveal--detect-moves
      (setq args (append args '("-M" "-C" "-C"))))
    ;; Add line range
    (when (and start-line end-line)
      (setq args (append args (list "-L" (format "%d,%d" start-line end-line)))))
    ;; Add revision (skip if uncommitted)
    (when (and target-revision
               (not (eq target-revision 'uncommitted)))
      (setq args (append args (list target-revision))))
    ;; Add file (with -- separator for clarity)
    (append args (list "--" relative-file))))

;;; Unified Helper Functions (ADDED in refactoring)

(defun blame-reveal--merge-move-copy-metadata (new-metadata)
  "Merge NEW-METADATA into global move/copy metadata cache.
Returns t if any metadata was merged, nil otherwise.
Always ensures blame-reveal--move-copy-metadata is a hash table."
  ;; Always ensure metadata is a hash table
  (blame-reveal--ensure-move-copy-metadata)
  ;; Only merge metadata when M/C detection is enabled
  (when (and blame-reveal--detect-moves
             new-metadata
             (hash-table-p new-metadata)
             (> (hash-table-count new-metadata) 0))
    (maphash (lambda (k v)
               (puthash k v blame-reveal--move-copy-metadata))
             new-metadata)
    t))

(defun blame-reveal--initialize-blame-state ()
  "Initialize all blame-related cache structures."
  (blame-reveal--reset-all-caches)
  ;; Always ensure move-copy-metadata is initialized
  (blame-reveal--ensure-move-copy-metadata))

(defun blame-reveal--process-blame-result (result use-lazy range-info)
  "Process blame RESULT uniformly for sync/async operations.
RESULT is (blame-data . move-metadata) from git operations.
USE-LAZY indicates if lazy loading mode is active.
RANGE-INFO is (start-line . end-line) for lazy mode, nil otherwise.
Returns the blame-data if successful, nil otherwise."
  (pcase-let ((`(,blame-data . ,move-metadata) result))
    (when blame-data
      ;; Store blame data and range
      (setq blame-reveal--blame-data blame-data
            blame-reveal--blame-data-range
            (if use-lazy range-info nil))

      ;; Initialize caches
      (blame-reveal--initialize-blame-state)

      ;; Merge move/copy metadata
      (blame-reveal--merge-move-copy-metadata move-metadata)

      ;; Log message
      (if (and use-lazy range-info (car range-info) (cdr range-info))
          (message "Git blame loaded%s: %d lines (range %d-%d)"
                   (if blame-reveal--detect-moves " with M/C detection" "")
                   (length blame-data)
                   (car range-info)
                   (cdr range-info))
        (message "Git blame loaded%s: %d lines"
                 (if blame-reveal--detect-moves " with M/C detection" "")
                 (length blame-data)))

      ;; Return data for further processing
      blame-data)))

(defun blame-reveal--call-git-blame-sync (start-line end-line)
  "Execute git blame synchronously with unified command building.
START-LINE and END-LINE are optional (nil for full file).
Returns (BLAME-DATA . MOVE-METADATA) on success, nil on failure."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))
    (when (and file git-root (vc-git-registered file))
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (let ((args (blame-reveal--build-blame-command-args
                       start-line end-line relative-file)))
            (when (zerop (apply #'call-process "git" nil t nil args))
              (blame-reveal--parse-blame-output (current-buffer) relative-file))))))))

;;; Blame Data Parsing

(defun blame-reveal--normalize-path (path git-root)
  "Normalize PATH relative to GIT-ROOT, handling ./ and ../ correctly."
  (when (and path git-root)
    (file-relative-name (expand-file-name path git-root) git-root)))

(defun blame-reveal--is-cross-file-p (file1 file2 git-root)
  "Check if FILE1 and FILE2 are different files.
Both should be relative to GIT-ROOT."
  (and file1 file2
       (not (string= (blame-reveal--normalize-path file1 git-root)
                     (blame-reveal--normalize-path file2 git-root)))))

(defun blame-reveal--parse-blame-output (output-buffer current-file)
  "Parse git blame porcelain output from OUTPUT-BUFFER.
CURRENT-FILE is the file being blamed (relative to git root).
Returns (BLAME-DATA . MOVE-METADATA) where:
- BLAME-DATA is list of (LINE-NUMBER . COMMIT-HASH)
- MOVE-METADATA is hash table of commit -> previous info

IMPORTANT: The 'filename' field in git blame output indicates the filename
at the time the line was last modified, NOT a cross-file operation.
Only the 'previous' field reliably indicates cross-file move/copy."
  (let* ((blame-data nil)
         (current-commit nil)
         (current-line-filename nil)
         (move-metadata (make-hash-table :test 'equal))
         (git-root (or (and (buffer-file-name)
                            (vc-git-root (buffer-file-name)))
                       default-directory)))
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ;; Commit hash line
         ((looking-at "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)")
          (setq current-commit (match-string 1))
          (setq current-line-filename nil)
          (let ((line-number (string-to-number (match-string 3))))
            (push (cons line-number current-commit) blame-data))
          (forward-line 1))

         ;; Filename field
         ;; This indicates the filename when the line was last modified.
         ;; It may differ from current-file due to renames in the file's history.
         ;; DO NOT use this to detect cross-file operations!
         ((looking-at "^filename \\(.+\\)$")
          (setq current-line-filename (match-string 1))
          ;; Store filename for informational purposes only
          ;; Could be used for display/debugging, but not for logic decisions
          (forward-line 1))

         ;; Previous field - THE ONLY RELIABLE SOURCE for cross-file operations
         ;; Format: "previous <commit-hash> <filename>"
         ;; This indicates where the line came from (different file or commit)
         ((looking-at "^previous \\([a-f0-9]+\\) \\(.+\\)$")
          (when current-commit
            (let ((prev-commit (match-string 1))
                  (prev-file (match-string 2)))
              ;; Only record if this is truly a cross-file operation
              (when (blame-reveal--is-cross-file-p prev-file current-file git-root)
                (puthash current-commit
                         (list :previous-commit prev-commit
                               :previous-file prev-file)
                         move-metadata))))
          (forward-line 1))

         ;; Tab line (content)
         ((looking-at "^\t")
          (forward-line 1))

         ;; Other metadata (author, committer, summary, etc.)
         (t
          (forward-line 1)))))

    (cons (nreverse blame-data) move-metadata)))

;;; Synchronous Loading (REFACTORED)

(defun blame-reveal--get-blame-data ()
  "Get git blame data for current buffer (entire file) synchronously.
Returns (BLAME-DATA . MOVE-METADATA) where:
- BLAME-DATA is list of (LINE-NUMBER . COMMIT-HASH)
- MOVE-METADATA is hash table of commit -> previous info"
  (blame-reveal--call-git-blame-sync nil nil))

(defun blame-reveal--get-blame-data-range (start-line end-line)
  "Get git blame data for specific line range START-LINE to END-LINE synchronously.
Returns (BLAME-DATA . MOVE-METADATA) where:
- BLAME-DATA is list of (LINE-NUMBER . COMMIT-HASH)
- MOVE-METADATA is hash table of commit -> previous info"
  (blame-reveal--call-git-blame-sync start-line end-line))

(defun blame-reveal--load-blame-data-sync ()
  "Load git blame data synchronously (REFACTORED)."
  (when (blame-reveal--state-start 'initial 'sync)
    (run-hooks 'blame-reveal-before-load-hook)
    (condition-case err
        (let* ((file (buffer-file-name))
               (git-root (and file (vc-git-root file))))
          (if (null git-root)
              (blame-reveal--state-error "Not in git repository")
            (let* ((use-lazy (blame-reveal--should-lazy-load-p))
                   (range (when use-lazy (blame-reveal--get-visible-line-range)))
                   (use-lazy-effective (and use-lazy range))
                   (result (if use-lazy-effective
                               (blame-reveal--call-git-blame-sync (car range) (cdr range))
                             (blame-reveal--call-git-blame-sync nil nil))))
              (blame-reveal--state-transition 'loading)
              (if-let* ((blame-data (blame-reveal--process-blame-result result use-lazy-effective range)))
                  (progn
                    (blame-reveal--state-transition 'processing)
                    (run-hook-with-args 'blame-reveal-after-load-hook (length blame-data))
                    (blame-reveal--state-transition 'rendering)
                    (blame-reveal--render-visible-region)
                    (blame-reveal--state-complete))
                (blame-reveal--state-error "No git blame data available")))))
      (error
       (blame-reveal--state-error (error-message-string err))
       nil))))

(defun blame-reveal--merge-new-blame-entries (new-data)
  "Merge NEW-DATA into blame-reveal--blame-data.
Returns the count of added entries."
  (let ((existing-lines (make-hash-table :test 'equal))
        (added-count 0))
    (dolist (entry blame-reveal--blame-data)
      (puthash (car entry) t existing-lines))
    (dolist (entry new-data)
      (unless (gethash (car entry) existing-lines)
        (push entry blame-reveal--blame-data)
        (cl-incf added-count)))
    (when (> added-count 0)
      (setq blame-reveal--blame-data
            (sort blame-reveal--blame-data
                  (lambda (a b) (< (car a) (car b))))))
    added-count))

(defun blame-reveal--update-blame-data-range (start-line end-line)
  "Update blame-reveal--blame-data-range to include START-LINE to END-LINE."
  (setq blame-reveal--blame-data-range
        (if blame-reveal--blame-data-range
            (cons (min start-line (car blame-reveal--blame-data-range))
                  (max end-line (cdr blame-reveal--blame-data-range)))
          (cons start-line end-line))))

(defun blame-reveal--finalize-expansion (start-line end-line added-count sync-p)
  "Finalize blame expansion with START-LINE, END-LINE, ADDED-COUNT.
SYNC-P indicates whether this is synchronous expansion."
  (blame-reveal--update-blame-data-range start-line end-line)
  (mapc #'blame-reveal--ensure-commit-info (blame-reveal--get-visible-commits))
  (blame-reveal--update-recent-commits)
  (blame-reveal--state-transition 'rendering)
  (blame-reveal--render-expanded-region start-line end-line)
  (let ((current-line (line-number-at-pos)))
    (when (and (>= current-line start-line)
               (<= current-line end-line))
      (blame-reveal--update-header)))
  (message "Blame expanded%s: +%d lines (total %d)"
           (if sync-p " (sync)" "")
           added-count
           (length blame-reveal--blame-data)))

(defun blame-reveal--expand-blame-data-sync (start-line end-line)
  "Synchronously expand blame data to include START-LINE to END-LINE."
  (when (blame-reveal--state-start 'expansion 'sync
                                   (list :start-line start-line
                                         :end-line end-line))
    (condition-case err
        (pcase-let* ((`(,new-data . ,move-metadata)
                      (blame-reveal--get-blame-data-range start-line end-line)))
          (if (null new-data)
              (progn
                (message "No new blame data in range %d-%d" start-line end-line)
                (blame-reveal--state-complete))
            (blame-reveal--state-transition 'processing)
            (blame-reveal--merge-move-copy-metadata move-metadata)
            (let ((added-count (blame-reveal--merge-new-blame-entries new-data)))
              (when (> added-count 0)
                (blame-reveal--finalize-expansion start-line end-line added-count t)))
            (blame-reveal--state-complete)))
      (error
       (blame-reveal--state-error (error-message-string err))
       nil))))

;;; Asynchronous Loading

(defun blame-reveal--make-async-sentinel (source-buffer temp-buffer success-handler
                                                        &optional error-handler)
  "Create a process sentinel for async blame loading."
  `(lambda (proc event)
     (cond
      ((string-match-p "finished" event)
       (when (buffer-live-p ,source-buffer)
         (with-current-buffer ,source-buffer
           ;; Verify process belongs to current operation
           (if (not (blame-reveal--state-verify-process proc))
               (progn
                 (message "[Async] Ignoring stale process callback (ID mismatch)")
                 (when (buffer-live-p ,temp-buffer)
                   (kill-buffer ,temp-buffer)))
             ;; Verify buffer state hasn't changed
             (if (not (and blame-reveal-mode
                           (equal (buffer-file-name)
                                  (process-get proc 'source-file))
                           (eq (current-buffer) ,source-buffer)))
                 (progn
                   (message "[Async] Ignoring callback: buffer state changed")
                   (when (buffer-live-p ,temp-buffer)
                     (kill-buffer ,temp-buffer)))
               ;; All checks passed, execute handler
               (funcall ,success-handler ,temp-buffer))))))
      (t
       (when (buffer-live-p ,source-buffer)
         (with-current-buffer ,source-buffer
           ;; Only handle error if process is still valid
           (when (blame-reveal--state-verify-process proc)
             (blame-reveal--state-error (format "Process %s: %s" proc event))
             ,(when error-handler
                `(funcall ,error-handler)))))
       (when (buffer-live-p ,temp-buffer)
         (kill-buffer ,temp-buffer))))))

(defun blame-reveal--start-async-blame (start-line end-line success-handler
                                                   &optional error-handler)
  "Start async git blame process."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file)))
         (source-buffer (current-buffer)))
    (unless git-root
      (blame-reveal--state-error "File is not in a git repository")
      (error "File is not in a git repository"))
    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (temp-buffer (generate-new-buffer " *blame-async*"))
           (process-environment (cons "GIT_PAGER=cat"
                                      (cons "PAGER=cat"
                                            process-environment)))
           (args (blame-reveal--build-blame-command-args
                  start-line end-line relative-file)))
      (blame-reveal--state-set-async-resources
       (make-process
        :name "blame-async"
        :buffer temp-buffer
        :command (cons "git" args)
        :sentinel (blame-reveal--make-async-sentinel
                   source-buffer temp-buffer success-handler error-handler)
        :noquery t)
       temp-buffer)
      ;; Store source file for verification
      (process-put blame-reveal--state-process 'source-file file))))

(defun blame-reveal--load-blame-data-async ()
  "Asynchronously load initial blame data."
  (let* ((use-lazy (blame-reveal--should-lazy-load-p))
         (range (when use-lazy (blame-reveal--get-visible-line-range)))
         (start-line (when range (car range)))
         (end-line (when range (cdr range))))
    (when (blame-reveal--state-start 'initial 'async
                                     (if (and start-line end-line)
                                         (list :start-line start-line
                                               :end-line end-line
                                               :use-lazy t)
                                       (list :use-lazy nil)))
      (if use-lazy
          (if blame-reveal--detect-moves
              (message "Loading git blame with M/C detection: lines %d-%d..." start-line end-line)
            (message "Loading git blame (async, lazy): lines %d-%d..." start-line end-line))
        (if blame-reveal--detect-moves
            (message "Loading git blame with M/C detection...")
          (message "Loading git blame (async, full)...")))
      (blame-reveal--start-async-blame
       start-line
       end-line
       (lambda (temp-buffer)
         (blame-reveal--handle-initial-load-complete temp-buffer))))))

(defun blame-reveal--handle-initial-load-complete (temp-buffer)
  "Handle completion of initial async blame loading from TEMP-BUFFER."
  (if (not (eq blame-reveal--state-status 'loading))
      (progn
        (message "[State] Unexpected complete in state %s" blame-reveal--state-status)
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer)))
    (unwind-protect
        (when (buffer-live-p temp-buffer)
          (condition-case err
              (progn
                (blame-reveal--state-transition 'processing)
                (let* ((file (buffer-file-name))
                       (git-root (vc-git-root file))
                       (relative-file (file-relative-name file git-root))
                       (result (blame-reveal--parse-blame-output
                                temp-buffer
                                relative-file))
                       (use-lazy (plist-get blame-reveal--state-metadata :use-lazy))
                       (range-info (when use-lazy
                                     (cons (plist-get blame-reveal--state-metadata :start-line)
                                           (plist-get blame-reveal--state-metadata :end-line)))))
                  (if-let* ((blame-data
                             (blame-reveal--process-blame-result
                              result use-lazy range-info)))
                      (progn
                        (run-hook-with-args 'blame-reveal-after-load-hook (length blame-data))
                        (blame-reveal--state-transition 'rendering)
                        (blame-reveal--load-commits-incrementally)
                        (blame-reveal--render-visible-region)
                        (blame-reveal--state-complete))
                    (blame-reveal--state-error "No git blame data available"))))
            (error
             (blame-reveal--state-error (format "Parse error: %s" (error-message-string err))))))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

(defun blame-reveal--expand-blame-data-async (start-line end-line)
  "Asynchronously expand blame data to include START-LINE to END-LINE."
  (when (blame-reveal--state-start 'expansion 'async
                                   (list :start-line start-line
                                         :end-line end-line))
    (blame-reveal--start-async-blame
     start-line
     end-line
     (lambda (temp-buffer)
       (blame-reveal--handle-expansion-complete temp-buffer start-line end-line)))))

(defun blame-reveal--merge-new-blame-entries-with-commits (new-data)
  "Merge NEW-DATA into blame-reveal--blame-data, tracking new commits.
Returns (ADDED-COUNT . NEW-COMMITS-HASH-TABLE)."
  (let ((existing-lines (make-hash-table :test 'equal))
        (new-commits (make-hash-table :test 'equal))
        (added-count 0))
    (dolist (entry blame-reveal--blame-data)
      (puthash (car entry) t existing-lines))
    (dolist (entry new-data)
      (unless (gethash (car entry) existing-lines)
        (push entry blame-reveal--blame-data)
        (cl-incf added-count)
        (puthash (cdr entry) t new-commits)))
    (when (> added-count 0)
      (setq blame-reveal--blame-data
            (sort blame-reveal--blame-data
                  (lambda (a b) (< (car a) (car b))))))
    (cons added-count new-commits)))

(defun blame-reveal--process-expansion-result (temp-buffer start-line end-line)
  "Process expansion result from TEMP-BUFFER for START-LINE to END-LINE.
Returns t on success, nil on failure."
  (let* ((file (buffer-file-name))
         (git-root (vc-git-root file))
         (relative-file (file-relative-name file git-root)))
    (pcase-let* ((`(,new-data . ,move-metadata)
                  (blame-reveal--parse-blame-output temp-buffer relative-file)))
      (if (null new-data)
          (progn
            (message "No new blame data in range %d-%d" start-line end-line)
            (blame-reveal--state-complete)
            nil)
        (blame-reveal--merge-move-copy-metadata move-metadata)
        (pcase-let* ((`(,added-count . ,new-commits)
                      (blame-reveal--merge-new-blame-entries-with-commits new-data)))
          (when (> added-count 0)
            (blame-reveal--update-blame-data-range start-line end-line)
            (maphash (lambda (commit _) (blame-reveal--ensure-commit-info commit))
                     new-commits)
            (blame-reveal--update-recent-commits)
            (blame-reveal--state-transition 'rendering)
            (blame-reveal--render-expanded-region start-line end-line)
            (let ((current-line (line-number-at-pos)))
              (when (and (>= current-line start-line)
                         (<= current-line end-line))
                (blame-reveal--update-header)))
            (message "Blame expanded: +%d lines (total %d)"
                     added-count (length blame-reveal--blame-data))))
        (blame-reveal--state-complete)
        t))))

(defun blame-reveal--handle-expansion-complete (temp-buffer start-line end-line)
  "Handle completion of async blame expansion from TEMP-BUFFER."
  (if (not (eq blame-reveal--state-status 'loading))
      (progn
        (message "[State] Unexpected complete in state %s" blame-reveal--state-status)
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer)))
    (unwind-protect
        (when (buffer-live-p temp-buffer)
          (condition-case err
              (progn
                (blame-reveal--state-transition 'processing)
                (blame-reveal--process-expansion-result temp-buffer start-line end-line))
            (error
             (blame-reveal--state-error (format "Expansion error: %s" (error-message-string err))))))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

;;; Commit Info

(defun blame-reveal--get-commit-info (commit-hash)
  "Get commit info for COMMIT-HASH.
Returns (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP DESCRIPTION)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "log"
                               "--no-walk"
                               "--no-patch"
                               "--format=%h|%an|%ar|%s|%at%n--BODY--%n%b"
                               commit-hash))
      (goto-char (point-min))
      (when (re-search-forward "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([0-9]+\\)" nil t)
        (let ((short-hash (match-string 1))
              (author (match-string 2))
              (date (match-string 3))
              (summary (match-string 4))
              (timestamp (string-to-number (match-string 5)))
              (description ""))
          ;; Parse description after --BODY-- marker
          (when (re-search-forward "^--BODY--\n" nil t)
            (setq description (string-trim (buffer-substring (point) (point-max)))))
          (list short-hash author date summary timestamp description))))))

(defun blame-reveal--get-commits-info-batch (commit-hashes)
  "Batch retrieve info for multiple COMMIT-HASHES.
Returns alist of (COMMIT-HASH . INFO)."
  (when commit-hashes
    (with-temp-buffer
      (let ((args (append (list "show" "--no-patch" "--no-walk"
                                "--format=%H%x00%h%x00%an%x00%ar%x00%s%x00%at%x00%b%x01")
                          commit-hashes)))
        (when (zerop (apply #'call-process "git" nil t nil args))
          (goto-char (point-min))
          (let ((results nil)
                (commits (split-string (buffer-string) "\001" t)))
            (dolist (commit-str commits)
              (let ((fields (split-string commit-str "\000" t)))
                (when (>= (length fields) 6)
                  (let ((full-hash (nth 0 fields))
                        (short-hash (nth 1 fields))
                        (author (nth 2 fields))
                        (date (nth 3 fields))
                        (summary (nth 4 fields))
                        (timestamp (string-to-number (nth 5 fields)))
                        (description (string-trim (or (nth 6 fields) ""))))
                    (push (cons full-hash
                                (list short-hash author date summary timestamp description))
                          results)))))
            (nreverse results)))))))

(defun blame-reveal--update-timestamp-range (timestamp)
  "Update blame-reveal--timestamps range with TIMESTAMP."
  (unless blame-reveal--timestamps
    (setq blame-reveal--timestamps (cons timestamp timestamp)))
  (setcar blame-reveal--timestamps
          (min (car blame-reveal--timestamps) timestamp))
  (setcdr blame-reveal--timestamps
          (max (cdr blame-reveal--timestamps) timestamp)))

(defun blame-reveal--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  (unless (or (blame-reveal--is-uncommitted-p commit-hash)
              (gethash commit-hash blame-reveal--commit-info))
    (when-let* ((info (blame-reveal--get-commit-info commit-hash)))
      (puthash commit-hash info blame-reveal--commit-info)
      (when-let* ((timestamp (nth 4 info)))
        (blame-reveal--update-timestamp-range timestamp)))))

(defsubst blame-reveal--get-visible-commits ()
  "Get list of commit hashes in visible area."
  (when blame-reveal--blame-data
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (visible-blocks (blame-reveal--find-block-boundaries
                            blame-reveal--blame-data
                            start-line
                            end-line)))
      (mapcar #'cadr visible-blocks))))

(defun blame-reveal--store-commit-info-entry (entry)
  "Store a single commit info ENTRY into cache and update timestamps.
ENTRY is (HASH . INFO) from batch results."
  (pcase-let* ((`(,hash . ,info) entry)
               (timestamp (nth 4 info)))
    (puthash hash info blame-reveal--commit-info)
    (when timestamp
      (blame-reveal--update-timestamp-range timestamp))))

(defun blame-reveal--ensure-visible-commits-loaded ()
  "Ensure commit info is loaded for all visible commits (Optimized Batch Version)."
  (when-let* ((visible-commits (blame-reveal--get-visible-commits)))
    (let ((missing-commits
           (cl-remove-if (lambda (h)
                           (or (blame-reveal--is-uncommitted-p h)
                               (gethash h blame-reveal--commit-info)))
                         visible-commits)))
      (when-let* ((batch-results (and missing-commits
                                      (blame-reveal--get-commits-info-batch missing-commits))))
        (mapc #'blame-reveal--store-commit-info-entry batch-results))
      (blame-reveal--update-recent-commits))))

(defun blame-reveal--load-commits-incrementally ()
  "Load commit info for visible area only (lazy loading)."
  (blame-reveal--ensure-visible-commits-loaded))

;;; Lazy Loading

(defun blame-reveal--should-lazy-load-p ()
  "Check if current file should use lazy loading based on size."
  (> (line-number-at-pos (point-max))
     blame-reveal-lazy-load-threshold))

(defun blame-reveal--is-range-loaded-p (start-line end-line)
  "Check if range START-LINE to END-LINE is already loaded."
  (if (not blame-reveal--blame-data-range)
      t
    (let ((current-start (car blame-reveal--blame-data-range))
          (current-end (cdr blame-reveal--blame-data-range)))
      (and (>= start-line current-start)
           (<= end-line current-end)))))

(defun blame-reveal--ensure-range-loaded (start-line end-line)
  "Ensure blame data is loaded for range START-LINE to END-LINE."
  (unless (blame-reveal--is-range-loaded-p start-line end-line)
    (when blame-reveal--blame-data-range
      (let ((current-start (car blame-reveal--blame-data-range))
            (current-end (cdr blame-reveal--blame-data-range)))
        (when (or (< start-line current-start)
                  (> end-line current-end))
          (when (not (blame-reveal--state-is-busy-p))
            (let ((new-start (min start-line current-start))
                  (new-end (max end-line current-end)))
              (if (blame-reveal--should-use-async-p)
                  (blame-reveal--expand-blame-data-async new-start new-end)
                (blame-reveal--expand-blame-data-sync new-start new-end)))))))))

(defun blame-reveal--should-use-async-p ()
  "Determine if async loading should be used based on configuration."
  (or blame-reveal--detect-moves
      (pcase blame-reveal-async-blame
        ('auto (blame-reveal--should-lazy-load-p))
        ('t t)
        (_ nil))))

(provide 'blame-reveal-git)
;;; blame-reveal-git.el ends here
