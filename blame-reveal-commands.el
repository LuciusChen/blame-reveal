;;; blame-reveal-commands.el --- Interactive commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands for blame-reveal.
;; Includes commit inspection, history browsing, and diagnostics.

;;; Code:

(require 'blame-reveal-core)
(require 'blame-reveal-git)

;;; Helper Functions

(defun blame-reveal--should-use-magit-p ()
  "Check if magit should be used based on configuration."
  (pcase blame-reveal-use-magit
    ('auto (and (fboundp 'magit-show-commit)
                (fboundp 'magit-log-buffer-file)))
    ('t (if (and (fboundp 'magit-show-commit)
                 (fboundp 'magit-log-buffer-file))
            t
          (error "Magit is not available but blame-reveal-use-magit is set to t")))
    (_ nil)))

(defun blame-reveal--revert-commit-diff (commit-hash)
  "Revert function for commit diff buffer showing COMMIT-HASH."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "show" "--color=never" commit-hash)
    (goto-char (point-min))))

(defun blame-reveal--revert-file-log (file)
  "Revert function for git log buffer of FILE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "log"
                  "--color=always"
                  "--follow"
                  "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                  "--" file)
    (goto-char (point-min))
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun blame-reveal--revert-line-log (line-num file)
  "Revert function for git log buffer of LINE-NUM in FILE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "git" nil t nil "log"
                  "--color=always"
                  "-L" (format "%d,%d:%s" line-num line-num file))
    (goto-char (point-min))
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max))))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-copy-commit-hash ()
  "Copy the commit hash of the current line to kill ring."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit (car current-block)))
      (progn
        (kill-new commit)
        (message "Copied commit hash: %s" (substring commit 0 8)))
    (message "No git blame info at current line")))

;;;###autoload
(defun blame-reveal-show-commit-diff ()
  "Show the diff of the commit at current line.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block)))
      (if (blame-reveal--should-use-magit-p)
          (progn
            (magit-show-commit commit-hash)
            (with-current-buffer (magit-get-mode-buffer 'magit-revision-mode)
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "q") #'quit-window)))
        (let ((buffer-name (format "*Commit Diff: %s*" (substring commit-hash 0 8))))
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (call-process "git" nil t nil "show" "--color=never" commit-hash)
              (goto-char (point-min))
              (diff-mode)
              (view-mode 1)
              (setq-local revert-buffer-function
                          (lambda (&rest _)
                            (blame-reveal--revert-commit-diff commit-hash))))
            (pop-to-buffer (current-buffer)))))
    (message "No commit info at current line")))

;;;###autoload
(defun blame-reveal-show-commit-details ()
  "Show detailed information about commit at current line in a popup buffer."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
  (let* ((current-block (blame-reveal--get-current-block))
         (commit-hash (car current-block)))
    (unless commit-hash
      (user-error "No commit at current line"))
    (if (blame-reveal--is-uncommitted-p commit-hash)
        (message "Uncommitted changes")
      (let ((info (gethash commit-hash blame-reveal--commit-info)))
        (unless info
          (blame-reveal--ensure-commit-info commit-hash)
          (setq info (gethash commit-hash blame-reveal--commit-info)))
        (when info
          (pcase-let ((`(,short-hash ,author ,date ,summary ,_timestamp ,description) info))
            (let* ((color (blame-reveal--get-commit-color commit-hash))
                   (buffer-name (format "*Commit: %s*" summary))
                   (move-icon (blame-reveal--icon "nf-md-arrow_right_bottom" color "󱞩"))
                   (move-meta (blame-reveal--get-move-copy-metadata commit-hash))
                   (prev-file (when move-meta (plist-get move-meta :previous-file)))
                   (prev-commit (when move-meta (plist-get move-meta :previous-commit))))

              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)

                  ;; Commit info - 标签用默认色，内容用 commit 色
                  (insert "Commit: ")
                  (insert (propertize short-hash 'face `(:foreground ,color :weight bold)))
                  (insert "\n")

                  (insert "Author: ")
                  (insert (propertize author 'face `(:foreground ,color)))
                  (insert "\n")

                  (insert "Date:   ")
                  (insert (propertize date 'face `(:foreground ,color)))
                  (insert "\n\n")

                  ;; Move/Copy info - 图标和内容都用 commit 色，稍淡
                  (when (and prev-file prev-commit)
                    (insert (propertize (format "%s %s · %s\n\n"
                                                move-icon
                                                prev-file
                                                (substring prev-commit 0 blame-reveal--short-hash-length))
                                        'face `(:foreground ,color :slant italic :height 0.95))))

                  ;; Summary - 用 commit 色，加粗突出
                  (insert (propertize summary 'face `(:foreground ,color :weight bold)))
                  (insert "\n\n")

                  ;; Description - 用默认色，降低视觉权重
                  (when (and description (not (string-empty-p description)))
                    (insert description))

                  (goto-char (point-min))
                  (view-mode 1)
                  (setq buffer-read-only t))

                (pop-to-buffer (current-buffer))))))))))

;;;###autoload
(defun blame-reveal-show-file-history ()
  "Show the git log history of current file.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (if (blame-reveal--should-use-magit-p)
              (magit-log-buffer-file)
            (let ((buffer-name (format "*Git Log: %s*" (file-name-nondirectory file))))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (call-process "git" nil t nil "log"
                                "--color=always"
                                "--follow"
                                "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                                "--" file)
                  (goto-char (point-min))
                  (require 'ansi-color)
                  (ansi-color-apply-on-region (point-min) (point-max))
                  (special-mode)
                  (setq-local revert-buffer-function
                              (lambda (&rest _)
                                (blame-reveal--revert-file-log file))))
                (pop-to-buffer (current-buffer)))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))

;;;###autoload
(defun blame-reveal-show-line-history ()
  "Show the git log history of current line.
This function always uses built-in git."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (let* ((line-num (line-number-at-pos))
                 (buffer-name (format "*Git Log: %s:%d*"
                                      (file-name-nondirectory file)
                                      line-num)))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (call-process "git" nil t nil "log"
                              "--color=always"
                              "-L" (format "%d,%d:%s" line-num line-num file))
                (goto-char (point-min))
                (require 'ansi-color)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode)
                (setq-local revert-buffer-function
                            (lambda (&rest _)
                              (blame-reveal--revert-line-log line-num file))))
              (pop-to-buffer (current-buffer))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))

;;; Diagnostic Commands

;;;###autoload
(defun blame-reveal-show-auto-calculation ()
  "Show how auto days limit is calculated for current file.

Displays diagnostic information including:
- Current mode (normal or recursive blame)
- Reference time used for age calculation
- Sample size and time span
- Calculated days limit
- Number of commits in range
- Gradient quality assessment

Useful for understanding the auto-calculation algorithm
and debugging color gradient issues."
  (interactive)
  (if (not blame-reveal--commit-info)
      (message "No blame data loaded yet")
    (let* ((timestamps nil)
           (reference-time (blame-reveal--get-reference-time))
           (is-recursive (and (boundp 'blame-reveal--current-revision)
                              blame-reveal--current-revision
                              (not (eq blame-reveal--current-revision 'uncommitted)))))
      ;; Collect timestamps
      (maphash (lambda (_commit info)
                 (when-let ((ts (nth 4 info)))
                   (push ts timestamps)))
               blame-reveal--commit-info)
      (setq timestamps (sort timestamps #'>))
      (let* ((sample-size (cond
                           ((>= (length timestamps) 15) 15)
                           ((>= (length timestamps) 10) 10)
                           ((>= (length timestamps) 5) 5)
                           (t (length timestamps))))
             (sample (seq-take timestamps sample-size))
             (span-seconds (when (> (length sample) 1)
                             (- (car sample) (car (last sample)))))
             (span-days (when span-seconds (/ span-seconds 86400.0)))
             (calculated-days (blame-reveal--auto-calculate-days-limit))
             (commits-in-range
              (if calculated-days
                  (length (cl-remove-if
                           (lambda (ts)
                             (> (- reference-time ts) (* calculated-days 86400)))
                           timestamps))
                0))
             (gradient-quality
              (when (> commits-in-range 0)
                (blame-reveal--calculate-gradient-quality commits-in-range)))
             (quality-desc
              (cond
               ((null gradient-quality) "N/A")
               ((>= gradient-quality 0.8) "Excellent")
               ((>= gradient-quality 0.5) "Good")
               ((>= gradient-quality 0.3) "Acceptable")
               (t "Poor")))
             (lightness-range
              (let ((scheme blame-reveal-color-scheme)
                    (is-dark (blame-reveal-color--is-dark-theme-p)))
                (if is-dark
                    (- (plist-get scheme :dark-newest)
                       (plist-get scheme :dark-oldest))
                  (- (plist-get scheme :light-oldest)
                     (plist-get scheme :light-newest)))))
             (per-commit-pct
              (if (> commits-in-range 1)
                  (* (/ lightness-range (1- commits-in-range)) 100)
                0)))
        (message
         (concat
          "Auto calculation for current file:\n"
          "- Mode: %s\n"
          "- Reference time: %s\n"
          "- Total commits loaded: %d\n"
          "- Sample size: %d commits\n"
          "- Sample time span: %.1f days\n"
          "- Calculated limit: %s days\n"
          "- Commits in range: %d\n"
          "- Gradient quality: %s (%.2f%% per commit)\n"
          "- Quality mode: %s")
         (if is-recursive
             (format "Recursive (@%s)" blame-reveal--revision-display)
           "Normal (HEAD)")
         (if is-recursive
             (format-time-string "%Y-%m-%d" reference-time)
           "now")
         (length timestamps)
         sample-size
         (or span-days 0)
         (if calculated-days (format "%d" calculated-days) "N/A")
         commits-in-range
         quality-desc
         per-commit-pct
         blame-reveal-gradient-quality)))))

;;;###autoload
(defun blame-reveal-clear-auto-cache ()
  "Clear auto-calculation cache and force recalculation.

Use this if you want to see the effect of changing
`blame-reveal-gradient-quality' setting, or if you suspect
the cached value is stale.

The cache will be automatically rebuilt on next update."
  (interactive)
  (setq blame-reveal--auto-days-cache nil)
  (when blame-reveal-mode
    (blame-reveal--update-recent-commits)
    (blame-reveal--render-visible-region)
    (message "Auto-calculation cache cleared and recalculated")))

(defun blame-reveal--auto-enable ()
  "Automatically enable blame-reveal-mode if current buffer is appropriate.
This function is called by `blame-reveal-global-mode' via `find-file-hook'.
It checks if the buffer is a git-tracked file and enables blame-reveal-mode.
Uses idle timer to avoid blocking file opening."
  (when (and blame-reveal-global-mode
             (buffer-file-name)
             (not (minibufferp))
             (not (string-prefix-p " " (buffer-name)))  ; Exclude temporary buffers
             (not (string-prefix-p "*" (buffer-name)))) ; Exclude special buffers
    ;; Delay git status check to avoid blocking file opening
    (run-with-idle-timer
     0.1 nil
     (lambda (buf)
       (when (and (buffer-live-p buf)
                  (eq buf (current-buffer)))
         (with-current-buffer buf
           (when (and (buffer-file-name)
                      (vc-git-registered (buffer-file-name))
                      (not blame-reveal-mode))
             (blame-reveal-mode 1)))))
     (current-buffer))))

(provide 'blame-reveal-commands)
;;; blame-reveal-commands.el ends here
