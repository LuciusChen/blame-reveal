;;; blame-reveal-git.el --- Git blame operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Git command execution, blame data parsing, and commit info fetching.
;; Handles both synchronous and asynchronous loading strategies.

;;; Code:

(require 'vc-git)
(require 'blame-reveal-core)
(require 'blame-reveal-state)

;;; Git Command Building

(defun blame-reveal--build-blame-command-args (start-line end-line relative-file)
  "Build git blame command arguments.
START-LINE and END-LINE specify optional line range.
RELATIVE-FILE is the file path relative to git root."
  (let ((args (list "blame" "--porcelain")))
    (when (and start-line end-line)
      (setq args (append args (list "-L" (format "%d,%d" start-line end-line)))))
    (when (and (boundp 'blame-reveal--current-revision)
               blame-reveal--current-revision
               (not (eq blame-reveal--current-revision 'uncommitted)))
      (setq args (append args (list blame-reveal--current-revision))))
    (append args (list relative-file))))

;;; Blame Data Parsing

(defun blame-reveal--parse-blame-output (output-buffer)
  "Parse git blame porcelain output from OUTPUT-BUFFER.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let ((blame-data nil)
        (current-commit nil))
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)")
          (setq current-commit (match-string 1))
          (let ((line-number (string-to-number (match-string 3))))
            (push (cons line-number current-commit) blame-data))
          (forward-line 1))
         ((looking-at "^\t")
          (forward-line 1))
         (t
          (forward-line 1)))))
    (nreverse blame-data)))

;;; Synchronous Loading

(defun blame-reveal--get-blame-data ()
  "Get git blame data for current buffer (entire file) synchronously.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))
    (if (not (and file git-root (vc-git-registered file)))
        nil
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (if (zerop (call-process "git" nil t nil "blame" "--porcelain" relative-file))
              (blame-reveal--parse-blame-output (current-buffer))
            nil))))))

(defun blame-reveal--get-blame-data-range (start-line end-line)
  "Get git blame data for specific line range START-LINE to END-LINE synchronously.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))
    (if (not (and file git-root (vc-git-registered file)))
        nil
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (if (zerop (call-process "git" nil t nil "blame"
                                   "--porcelain"
                                   "-L" (format "%d,%d" start-line end-line)
                                   relative-file))
              (blame-reveal--parse-blame-output (current-buffer))
            nil))))))

(defun blame-reveal--load-blame-data-sync ()
  "Load git blame data synchronously."
  (unless (blame-reveal--state-start 'initial 'sync)
    (cl-return-from blame-reveal--load-blame-data-sync nil))
  (run-hooks 'blame-reveal-before-load-hook)
  (condition-case err
      (let* ((file (buffer-file-name))
             (git-root (and file (vc-git-root file))))
        (unless git-root
          (blame-reveal--state-error "Not in git repository")
          (cl-return-from blame-reveal--load-blame-data-sync nil))
        (let ((use-lazy (blame-reveal--should-lazy-load-p))
              (blame-data nil))
          (blame-reveal--state-transition 'loading)
          (if use-lazy
              (let* ((range (blame-reveal--get-visible-line-range))
                     (start-line (car range))
                     (end-line (cdr range)))
                (setq blame-data (blame-reveal--get-blame-data-range start-line end-line))
                (when blame-data
                  (setq blame-reveal--blame-data-range (cons start-line end-line))
                  (message "Git blame loaded (sync, lazy): %d lines (range %d-%d)"
                           (length blame-data) start-line end-line)))
            (setq blame-data (blame-reveal--get-blame-data))
            (when blame-data
              (setq blame-reveal--blame-data-range nil)
              (message "Git blame loaded (sync, full): %d lines" (length blame-data))))
          (if blame-data
              (progn
                (blame-reveal--state-transition 'processing)
                (setq blame-reveal--blame-data blame-data)
                (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                (setq blame-reveal--color-map (make-hash-table :test 'equal))
                (setq blame-reveal--timestamps nil)
                (setq blame-reveal--recent-commits nil)
                (setq blame-reveal--all-commits-loaded nil)
                (run-hook-with-args 'blame-reveal-after-load-hook (length blame-data))
                (blame-reveal--state-transition 'rendering)
                (blame-reveal--load-commits-incrementally)
                (blame-reveal--render-visible-region)
                (blame-reveal--state-complete))
            (blame-reveal--state-error "No git blame data available"))))
    (error
     (blame-reveal--state-error (error-message-string err))
     nil)))

(defun blame-reveal--expand-blame-data-sync (start-line end-line)
  "Synchronously expand blame data to include START-LINE to END-LINE."
  (unless (blame-reveal--state-start 'expansion 'sync
                                     (list :start-line start-line
                                           :end-line end-line))
    (cl-return-from blame-reveal--expand-blame-data-sync nil))
  (condition-case err
      (let ((new-data (blame-reveal--get-blame-data-range start-line end-line)))
        (if (null new-data)
            (progn
              (message "No new blame data in range %d-%d" start-line end-line)
              (blame-reveal--state-complete))
          (blame-reveal--state-transition 'processing)
          (let ((existing-lines (make-hash-table :test 'equal))
                (added-count 0))
            (dolist (entry blame-reveal--blame-data)
              (puthash (car entry) t existing-lines))
            (dolist (entry new-data)
              (unless (gethash (car entry) existing-lines)
                (push entry blame-reveal--blame-data)
                (setq added-count (1+ added-count))))
            (when (> added-count 0)
              (setq blame-reveal--blame-data
                    (sort blame-reveal--blame-data
                          (lambda (a b) (< (car a) (car b)))))
              (if blame-reveal--blame-data-range
                  (setq blame-reveal--blame-data-range
                        (cons (min start-line (car blame-reveal--blame-data-range))
                              (max end-line (cdr blame-reveal--blame-data-range))))
                (setq blame-reveal--blame-data-range
                      (cons start-line end-line)))
              (let ((visible-commits (blame-reveal--get-visible-commits)))
                (dolist (commit visible-commits)
                  (blame-reveal--ensure-commit-info commit)))
              (blame-reveal--update-recent-commits)
              (blame-reveal--state-transition 'rendering)
              (blame-reveal--render-expanded-region start-line end-line)
              (let ((current-line (line-number-at-pos)))
                (when (and (>= current-line start-line)
                           (<= current-line end-line))
                  (blame-reveal--update-header)))
              (message "Blame expanded (sync): +%d lines (total %d)"
                       added-count (length blame-reveal--blame-data)))))
        (blame-reveal--state-complete))
    (error
     (blame-reveal--state-error (error-message-string err))
     nil)))

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
                                  (process-get proc 'source-file))))
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
  (unless (blame-reveal--state-start 'initial 'async)
    (cl-return-from blame-reveal--load-blame-data-async nil))
  (let* ((use-lazy (blame-reveal--should-lazy-load-p))
         (range (when use-lazy (blame-reveal--get-visible-line-range)))
         (start-line (when use-lazy (car range)))
         (end-line (when use-lazy (cdr range))))
    (if use-lazy
        (message "Loading git blame (async, lazy): lines %d-%d..." start-line end-line)
      (message "Loading git blame (async, full)..."))
    (blame-reveal--start-async-blame
     start-line
     end-line
     (lambda (temp-buffer)
       (blame-reveal--handle-initial-load-complete temp-buffer use-lazy)))))

(defun blame-reveal--handle-initial-load-complete (temp-buffer use-lazy)
  "Handle completion of initial async blame loading from TEMP-BUFFER."
  (unless (eq blame-reveal--state-status 'loading)
    (message "[State] Unexpected complete in state %s" blame-reveal--state-status)
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (cl-return-from blame-reveal--handle-initial-load-complete nil))

  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (condition-case err
            (progn
              (blame-reveal--state-transition 'processing)
              (let ((blame-data (blame-reveal--parse-blame-output temp-buffer)))
                (if blame-data
                    (progn
                      (setq blame-reveal--blame-data blame-data)
                      (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                      (setq blame-reveal--color-map (make-hash-table :test 'equal))
                      (setq blame-reveal--timestamps nil)
                      (setq blame-reveal--recent-commits nil)
                      (setq blame-reveal--all-commits-loaded nil)
                      (if use-lazy
                          (let* ((range (blame-reveal--get-visible-line-range))
                                 (start-line (car range))
                                 (end-line (cdr range)))
                            (setq blame-reveal--blame-data-range (cons start-line end-line))
                            (message "Git blame loaded (async, lazy): %d lines (range %d-%d)"
                                     (length blame-data) start-line end-line))
                        (setq blame-reveal--blame-data-range nil)
                        (message "Git blame loaded (async, full): %d lines" (length blame-data)))
                      (blame-reveal--state-transition 'rendering)
                      (blame-reveal--load-commits-incrementally)
                      (blame-reveal--render-visible-region)
                      (blame-reveal--state-complete))
                  (blame-reveal--state-error "No git blame data available"))))
          (error
           (blame-reveal--state-error (format "Parse error: %s" (error-message-string err))))))
    ;; Always clean up temp buffer
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))))

(defun blame-reveal--expand-blame-data-async (start-line end-line)
  "Asynchronously expand blame data to include START-LINE to END-LINE."
  (unless (blame-reveal--state-start 'expansion 'async
                                     (list :start-line start-line
                                           :end-line end-line))
    (cl-return-from blame-reveal--expand-blame-data-async nil))
  (blame-reveal--start-async-blame
   start-line
   end-line
   (lambda (temp-buffer)
     (blame-reveal--handle-expansion-complete temp-buffer start-line end-line))))

(defun blame-reveal--handle-expansion-complete (temp-buffer start-line end-line)
  "Handle completion of async blame expansion from TEMP-BUFFER."
  (unless (eq blame-reveal--state-status 'loading)
    (message "[State] Unexpected complete in state %s" blame-reveal--state-status)
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (cl-return-from blame-reveal--handle-expansion-complete nil))

  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (condition-case err
            (progn
              (blame-reveal--state-transition 'processing)
              (let ((new-data (blame-reveal--parse-blame-output temp-buffer)))
                (if (null new-data)
                    (progn
                      (message "No new blame data in range %d-%d" start-line end-line)
                      (blame-reveal--state-complete))
                  (let ((existing-lines (make-hash-table :test 'equal))
                        (added-count 0)
                        (new-commits (make-hash-table :test 'equal)))
                    (dolist (entry blame-reveal--blame-data)
                      (puthash (car entry) t existing-lines))
                    (dolist (entry new-data)
                      (unless (gethash (car entry) existing-lines)
                        (push entry blame-reveal--blame-data)
                        (setq added-count (1+ added-count))
                        (puthash (cdr entry) t new-commits)))
                    (when (> added-count 0)
                      (setq blame-reveal--blame-data
                            (sort blame-reveal--blame-data
                                  (lambda (a b) (< (car a) (car b)))))
                      (if blame-reveal--blame-data-range
                          (setq blame-reveal--blame-data-range
                                (cons (min start-line (car blame-reveal--blame-data-range))
                                      (max end-line (cdr blame-reveal--blame-data-range))))
                        (setq blame-reveal--blame-data-range
                              (cons start-line end-line)))
                      (maphash (lambda (commit _)
                                 (blame-reveal--ensure-commit-info commit))
                               new-commits)
                      (blame-reveal--update-recent-commits)
                      (blame-reveal--state-transition 'rendering)
                      (blame-reveal--render-expanded-region start-line end-line)
                      (let ((current-line (line-number-at-pos)))
                        (when (and (>= current-line start-line)
                                   (<= current-line end-line))
                          (blame-reveal--update-header)))
                      (message "Blame expanded: +%d lines (total %d)"
                               added-count (length blame-reveal--blame-data)))))
                (blame-reveal--state-complete)))
          (error
           (blame-reveal--state-error (format "Expansion error: %s" (error-message-string err))))))
    ;; Always clean up temp buffer
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))))

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

(defun blame-reveal--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  (unless (or (blame-reveal--is-uncommitted-p commit-hash)
              (gethash commit-hash blame-reveal--commit-info))
    (let ((info (blame-reveal--get-commit-info commit-hash)))
      (when info
        (puthash commit-hash info blame-reveal--commit-info)
        (let ((timestamp (nth 4 info)))
          (when timestamp
            (unless blame-reveal--timestamps
              (setq blame-reveal--timestamps (cons timestamp timestamp)))
            (setcar blame-reveal--timestamps
                    (min (car blame-reveal--timestamps) timestamp))
            (setcdr blame-reveal--timestamps
                    (max (cdr blame-reveal--timestamps) timestamp))))))))

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

(defun blame-reveal--ensure-visible-commits-loaded ()
  "Ensure commit info is loaded for all visible commits (Optimized Batch Version)."
  (when-let ((visible-commits (blame-reveal--get-visible-commits)))
    (let ((missing-commits
           (cl-remove-if (lambda (h)
                           (or (blame-reveal--is-uncommitted-p h)
                               (gethash h blame-reveal--commit-info)))
                         visible-commits)))
      (when missing-commits
        (let ((batch-results (blame-reveal--get-commits-info-batch missing-commits)))
          (dolist (entry batch-results)
            (let* ((hash (car entry))
                   (info (cdr entry))
                   (timestamp (nth 4 info)))
              (puthash hash info blame-reveal--commit-info)
              (when timestamp
                (unless blame-reveal--timestamps
                  (setq blame-reveal--timestamps (cons timestamp timestamp)))
                (setcar blame-reveal--timestamps
                        (min (car blame-reveal--timestamps) timestamp))
                (setcdr blame-reveal--timestamps
                        (max (cdr blame-reveal--timestamps) timestamp)))))))
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
  (pcase blame-reveal-async-blame
    ('auto (blame-reveal--should-lazy-load-p))
    ('t t)
    (_ nil)))

(provide 'blame-reveal-git)
;;; blame-reveal-git.el ends here
