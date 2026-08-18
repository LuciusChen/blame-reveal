;;; blame-reveal-test.el --- Regression tests for blame-reveal -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'vc-git)
(require 'blame-reveal)
(require 'blame-reveal-focus)
(require 'blame-reveal-recursive)

(defun blame-reveal-test--git (&rest args)
  "Run local Git with ARGS in `default-directory'."
  (let ((process-environment (copy-sequence process-environment))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (setenv "GIT_CONFIG_NOSYSTEM" "1")
    (setenv "GIT_CONFIG_NOGLOBAL" "1")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply #'call-process "git" nil nil nil args)))

(defun blame-reveal-test--make-repo ()
  "Create and return an isolated temporary Git repository."
  (let ((directory (make-temp-file "blame-reveal-test-" t)))
    (let ((default-directory directory))
      (unless (zerop (blame-reveal-test--git "init" "-q"))
        (error "Could not initialize test repository")))
    directory))

(defun blame-reveal-test--commit (message &optional body)
  "Commit the current file with MESSAGE and optional BODY."
  (let ((args (list "-c" "user.name=Blame Reveal Test"
                    "-c" "user.email=blame-reveal-test@example.invalid"
                    "commit" "-q" "-m" message)))
    (when body
      (setq args (append args (list "-m" body))))
    (unless (zerop (apply #'blame-reveal-test--git args))
      (error "Could not create test commit"))))

(defun blame-reveal-test--insert-file-buffer (file)
  "Return a new buffer visiting FILE without reusing an existing buffer."
  (let ((buffer (generate-new-buffer
                 (format " *blame-reveal-test-%s*"
                         (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (insert-file-contents file)
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))
      (set-visited-file-modtime)
      (fundamental-mode))
    buffer))

(defun blame-reveal-test--render-block-summaries ()
  "Create test header overlays for each current blame block.
The summary marker makes this test independent of face and icon rendering."
  (dolist (block (blame-reveal--find-block-boundaries blame-reveal--blame-data))
    (let* ((start (nth 0 block))
           (commit (nth 1 block))
           (info (gethash commit blame-reveal--commit-info))
           (overlay (blame-reveal--create-header-overlay
                     start commit "#6699cc" t)))
      (overlay-put overlay 'blame-reveal-test-summary (nth 3 info))
      (overlay-put overlay 'blame-reveal-test-block-start start))))

(defun blame-reveal-test--block-summaries ()
  "Return sorted test header summaries in the current buffer."
  (sort
   (cl-loop for overlay in (overlays-in (point-min) (point-max))
            when (overlay-get overlay 'blame-reveal-test-summary)
            collect (cons (overlay-get overlay 'blame-reveal-test-block-start)
                          (overlay-get overlay 'blame-reveal-test-summary)))
   (lambda (left right) (< (car left) (car right)))))

(ert-deftest blame-reveal-revert-clears-block-overlays-and-refreshes-once ()
  "Reverting a live mode buffer matches a freshly opened buffer exactly.
Before the fix, `normal-mode' discarded the mode's local registry while
leaving block header overlays in the buffer, so stale summaries remained and
new block summaries could be duplicated on the next render."
  (let ((repo (blame-reveal-test--make-repo))
        (file nil)
        (original nil)
        (fresh nil)
        (stale-overlays nil)
        (load-count 0)
        (old-block-format-function blame-reveal-block-format-function))
    (unwind-protect
        (progn
          (setq blame-reveal-block-format-function
                (lambda (_hash info color)
                  (make-blame-reveal-commit-display
                   :lines (list (nth 3 info))
                   :faces (list nil)
                   :color color)))
          (setq file (expand-file-name "sample.txt" repo))
          (let ((default-directory repo))
            (with-temp-file file
              (insert "one\nold two\nthree\nold four\n"))
            (blame-reveal-test--git "add" "sample.txt")
            (blame-reveal-test--commit "old summary")
            (with-temp-file file
              (insert "new one\nold two\nnew three\nold four\n"))
            (blame-reveal-test--git "add" "sample.txt")
            (blame-reveal-test--commit "new summary")
            (setq original (find-file-noselect file))
            ;; The test controls viewport rendering and makes the reload
            ;; synchronous so the hook's single refresh is observable.
            (with-current-buffer original
              (setq-local blame-reveal-async-blame nil)
              (cl-letf (((symbol-function 'blame-reveal--get-visible-line-range)
                         (lambda () (cons 1 (line-number-at-pos (point-max)))))
                        ((symbol-function 'blame-reveal--load-blame-data)
                         (let ((real-load (symbol-function
                                           'blame-reveal--load-blame-data)))
                           (lambda ()
                             (when (eq (current-buffer) original)
                               (cl-incf load-count))
                             (funcall real-load)))))
                (blame-reveal-mode 1)
                (should (local-variable-p 'revert-buffer-restore-functions))
                (should (memq #'blame-reveal--revert-buffer-restore
                              revert-buffer-restore-functions))
                (blame-reveal-test--render-block-summaries)
                (let ((commit (nth 1 (car (blame-reveal--find-block-boundaries
                                           blame-reveal--blame-data)))))
                  (blame-reveal--create-fringe-overlay 1 "#6699cc" commit)
                  (setq blame-reveal--sticky-header-overlay
                        (blame-reveal--ensure-header-overlay
                         nil 1 commit "#6699cc" nil "sticky ")
                        blame-reveal--sticky-header-state
                        (list :commit commit :visible t :window-start 1)))
                (setq stale-overlays (overlays-in (point-min) (point-max)))
                (should (> (length (blame-reveal-test--block-summaries)) 1))
                ;; Move HEAD back so the reverted file has committed blame
                ;; data rather than uncommitted zero hashes.
                (blame-reveal-test--git "reset" "--hard" "HEAD~1")
                (setq fresh (blame-reveal-test--insert-file-buffer file))
                (with-current-buffer fresh
                  (setq-local blame-reveal-async-blame nil)
                  (blame-reveal-mode 1)
                  (blame-reveal-test--render-block-summaries))
                (revert-buffer t t)
                (should (eq blame-reveal-mode t))
                (should (= load-count 2))
                (should-not (cl-some #'overlay-buffer stale-overlays))
                (blame-reveal-test--render-block-summaries)
                (should (equal (blame-reveal-test--block-summaries)
                               (with-current-buffer fresh
                                 (blame-reveal-test--block-summaries))))
                (should (= (length (blame-reveal-test--block-summaries))
                           (length (delete-dups
                                    (mapcar #'cdr
                                            (blame-reveal-test--block-summaries))))))
                (blame-reveal-mode -1)
                (should-not (memq #'blame-reveal--revert-buffer-restore
                                  revert-buffer-restore-functions))))))
      (dolist (buffer (list original fresh))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (bound-and-true-p blame-reveal-mode)
              (blame-reveal-mode -1))
            (kill-buffer buffer))))
      (setq blame-reveal-block-format-function old-block-format-function)
      (when (and repo (file-directory-p repo))
        (delete-directory repo t)))))

(ert-deftest blame-reveal-focus-commands-stay-under-prefix-map ()
  "Bare focus keys remain available for normal text insertion."
  (should-not (lookup-key blame-reveal-mode-map (kbd "n")))
  (should-not (lookup-key blame-reveal-mode-map (kbd "N")))
  (should-not (lookup-key blame-reveal-mode-map (kbd "F")))
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "f"))
              #'blame-reveal-focus-commit))
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "n"))
              #'blame-reveal-next-focus-block))
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "N"))
              #'blame-reveal-prev-focus-block))
  (should (eq (lookup-key blame-reveal-mode-map (kbd "C-c C-l f"))
              #'blame-reveal-focus-commit))
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name "editable.txt" temporary-file-directory))
    (cl-letf (((symbol-function 'blame-reveal--load-blame-data) #'ignore))
      (blame-reveal-mode 1)
      (unwind-protect
          (progn
            (execute-kbd-macro (kbd "nNF"))
            (should (equal (buffer-string) "nNF")))
        (blame-reveal-mode -1)))))

(ert-deftest blame-reveal-git-sync-preserves-utf8-commit-metadata ()
  "Synchronous Git metadata paths decode UTF-8 independent of locale."
  (let ((repo (blame-reveal-test--make-repo))
        (file nil))
    (unwind-protect
        (progn
          (setq file (expand-file-name "metadata.txt" repo))
          (let ((default-directory repo))
            (with-temp-file file
              (insert "Unicode content\n"))
            (blame-reveal-test--git "add" "metadata.txt")
            (blame-reveal-test--commit
             "Grüße 世界 😀"
             "Körper äöü 中文 🚀")
            (let* ((hash
                    (string-trim
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8)
                             (coding-system-for-write 'utf-8))
                         (call-process "git" nil t nil "rev-parse" "HEAD"))
                       (buffer-string)))))
              (with-temp-buffer
                (insert-file-contents file)
                (setq buffer-file-name file)
                (let ((process-environment (copy-sequence process-environment)))
                  (setenv "GIT_CONFIG_NOSYSTEM" "1")
                  (setenv "GIT_CONFIG_NOGLOBAL" "1")
                  (setenv "LC_ALL" "C")
                  (setenv "LANG" "C")
                  (let ((coding-system-for-read 'iso-latin-1)
                        (coding-system-for-write 'iso-latin-1))
                    (pcase-let ((`(,blame-data . ,_)
                                 (blame-reveal--call-git-blame-sync nil nil)))
                      (should (= (length blame-data) 1)))
                    (pcase-let ((`(,recursive-blame-data . ,_)
                                 (blame-reveal--get-blame-data-sync
                                  'uncommitted file)))
                      (should (= (length recursive-blame-data) 1)))
                    (let ((info (blame-reveal--get-commit-info hash)))
                      (should (equal (nth 3 info) "Grüße 世界 😀"))
                      (should (equal (nth 5 info) "Körper äöü 中文 🚀")))
                    (let ((batch
                           (blame-reveal--get-commits-info-batch (list hash))))
                      (pcase-let ((`(,batch-hash . ,info) (car batch)))
                        (should (equal batch-hash hash))
                        (should (equal (nth 3 info) "Grüße 世界 😀"))
                        (should (equal (nth 5 info) "Körper äöü 中文 🚀"))))
                    (let ((short-info (blame-reveal--get-short-info hash)))
                      (should (string-match-p
                               (regexp-quote "Grüße 世界 😀")
                               short-info)))))))))
      (when (and repo (file-directory-p repo))
        (delete-directory repo t)))))

(provide 'blame-reveal-test)
;;; blame-reveal-test.el ends here
