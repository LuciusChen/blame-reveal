;;; blame-reveal-test.el --- Regression tests for blame-reveal -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused ERT coverage for interaction and lifecycle regressions.

;;; Code:

(require 'ert)
(require 'cl-lib)

(setq load-prefer-newer t)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'blame-reveal)
(require 'blame-reveal-focus)
(require 'blame-reveal-recursive)
(require 'blame-reveal-transient)

(ert-deftest blame-reveal-mode-map-keeps-focus-navigation-under-prefix ()
  "Focus commands should stay under the `C-c l' prefix."
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "f"))
              #'blame-reveal-focus-commit))
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "n"))
              #'blame-reveal-next-focus-block))
  (should (eq (lookup-key blame-reveal-prefix-map (kbd "N"))
              #'blame-reveal-prev-focus-block))
  (should-not (lookup-key blame-reveal-mode-map (kbd "F")))
  (should-not (lookup-key blame-reveal-mode-map (kbd "n")))
  (should-not (lookup-key blame-reveal-mode-map (kbd "N"))))

(ert-deftest blame-reveal-heatmap-strategy-uses-six-distinct-warm-to-cool-steps ()
  "Heatmap strategy should keep six recent commits visually distinct."
  (let ((strategy (blame-reveal-heatmap-strategy-create)))
    (should
     (equal
      (cl-loop for rank from 0 below 6
               collect (blame-reveal-color-calculate
                        strategy nil `(:rank ,rank :total-recent 6 :is-dark t)))
      '("#ff6b4a" "#ff9350" "#ffc857" "#93c25f" "#5aa6ad" "#6178ad")))
    (should
     (equal
      (cl-loop for rank from 0 below 6
               collect (blame-reveal-color-calculate
                        strategy nil `(:rank ,rank :total-recent 6 :is-dark nil)))
      '("#c2412d" "#d96a1c" "#b88a00" "#6f8c34" "#3c8088" "#5368a8")))
    (should (equal (blame-reveal-color-old strategy '(:is-dark t)) "#46546a"))
    (should (equal (blame-reveal-color-old strategy '(:is-dark nil)) "#c8ced7"))))

(ert-deftest blame-reveal-apply-color-preset-heatmap-switches-color-mode ()
  "Heatmap preset should switch to the dedicated heatmap color mode."
  (let (customized
        (refresh-calls 0))
    (cl-letf (((symbol-function 'blame-reveal--set-config-value)
               (lambda (symbol value)
                 (push (cons symbol value) customized)))
              ((symbol-function 'blame-reveal--refresh-active-color-displays)
               (lambda ()
                 (setq refresh-calls (1+ refresh-calls))))
              ((symbol-function 'blame-reveal--force-update-header)
               #'ignore)
              ((symbol-function 'message)
               #'ignore))
      (blame-reveal--apply-color-preset 'heatmap)
      (should (member '(blame-reveal-color-mode . heatmap) customized))
      (should (= refresh-calls 1)))))

(ert-deftest blame-reveal-recent-window-settings-prefer-outcomes-over-knobs ()
  "Recent window presets should map to stable days/quality pairs."
  (should (equal (blame-reveal--recent-window-settings 'tight)
                 '(:days-limit 30 :gradient-quality strict)))
  (should (equal (blame-reveal--recent-window-settings 'balanced)
                 '(:days-limit auto :gradient-quality auto)))
  (should (equal (blame-reveal--recent-window-settings 'wide)
                 '(:days-limit 180 :gradient-quality relaxed)))
  (should (equal (blame-reveal--recent-window-settings 'all)
                 '(:days-limit nil :gradient-quality relaxed))))

(ert-deftest blame-reveal-apply-preset-default-selects-green-gradient ()
  "Default preset should use the green gradient baseline."
  (let ((blame-reveal-header-style 'inline)
        (blame-reveal-recent-days-limit 30)
        (blame-reveal-gradient-quality 'strict)
        (blame-reveal-fringe-side 'right-fringe)
        (blame-reveal-color-mode 'heatmap)
        (blame-reveal-color-scheme '(:hue 200))
        (blame-reveal-mode nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (blame-reveal--apply-preset-default)
      (should (eq blame-reveal-header-style 'inline))
      (should (eq blame-reveal-recent-days-limit 'auto))
      (should (eq blame-reveal-gradient-quality 'auto))
      (should (eq blame-reveal-fringe-side 'left-fringe))
      (should (eq blame-reveal-color-mode 'gradient))
      (should (equal blame-reveal-color-scheme
                     '(:hue 120
                       :dark-newest 0.70 :dark-oldest 0.35
                       :light-newest 0.40 :light-oldest 0.75
                       :saturation-min 0.25 :saturation-max 0.60))))))

(ert-deftest blame-reveal-header-style-defaults-to-inline ()
  "Default header style should be inline."
  (should (eq (default-value 'blame-reveal-header-style) 'inline)))

(ert-deftest blame-reveal-color-scheme-defaults-to-green-gradient ()
  "Default gradient scheme should use the green baseline."
  (should (equal (default-value 'blame-reveal-color-scheme)
                 '(:hue 120
                   :dark-newest 0.70 :dark-oldest 0.35
                   :light-newest 0.40 :light-oldest 0.75
                   :saturation-min 0.25 :saturation-max 0.60))))

(ert-deftest blame-reveal-set-color-scheme-component-uses-scope-aware-setter ()
  "High-level color edits should use the shared setter and refresh path."
  (let ((captured nil)
        (refresh-calls 0)
        (blame-reveal-color-scheme
         '(:hue 200 :dark-newest 0.76 :dark-oldest 0.26
           :light-newest 0.26 :light-oldest 0.82
           :saturation-min 0.40 :saturation-max 0.76)))
    (cl-letf (((symbol-function 'blame-reveal--set-config-value)
               (lambda (symbol value)
                 (setq captured (cons symbol value))))
              ((symbol-function 'blame-reveal--refresh-colors-after-setting-change)
               (lambda ()
                 (setq refresh-calls (1+ refresh-calls)))))
      (blame-reveal--set-color-scheme-component :hue 120)
      (should (equal (car captured) 'blame-reveal-color-scheme))
      (should (equal (plist-get (cdr captured) :hue) 120))
      (should (= refresh-calls 1)))))

(ert-deftest blame-reveal-set-header-style-refreshes-all-active-buffers-in-global-scope ()
  "Global-scope header-style changes should refresh every active blame buffer."
  (let ((buf-a (generate-new-buffer " *blame-reveal-a*"))
        (buf-b (generate-new-buffer " *blame-reveal-b*"))
        refreshed)
    (unwind-protect
        (with-current-buffer buf-a
          (setq-local blame-reveal-mode t)
          (setq buffer-file-name "/tmp/a.el")
          (kill-local-variable 'blame-reveal-header-style)
          (with-current-buffer buf-b
            (setq-local blame-reveal-mode t)
            (setq buffer-file-name "/tmp/b.el"))
          (cl-letf (((symbol-function 'transient-setup) #'ignore)
                    ((symbol-function 'blame-reveal--restore-window-margins)
                     #'ignore)
                    ((symbol-function 'blame-reveal--ensure-window-margins)
                     #'ignore)
                    ((symbol-function 'blame-reveal--force-update-header)
                     (lambda ()
                       (push (buffer-name) refreshed))))
            (blame-reveal--set-header-style 'inline)
            (should (equal (sort refreshed #'string<)
                           (sort (list (buffer-name buf-a)
                                       (buffer-name buf-b))
                                 #'string<)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest blame-reveal-apply-recent-window-preset-refreshes-all-active-buffers-in-global-scope ()
  "Global recent-window changes should update all active blame buffers."
  (let ((buf-a (generate-new-buffer " *blame-reveal-a*"))
        (buf-b (generate-new-buffer " *blame-reveal-b*"))
        refreshed)
    (unwind-protect
        (with-current-buffer buf-a
          (setq-local blame-reveal-mode t)
          (setq buffer-file-name "/tmp/a.el")
          (kill-local-variable 'blame-reveal-recent-days-limit)
          (with-current-buffer buf-b
            (setq-local blame-reveal-mode t)
            (setq buffer-file-name "/tmp/b.el"))
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'blame-reveal--update-recent-commits)
                     (lambda ()
                       (push (list (buffer-name) 'recent) refreshed)))
                    ((symbol-function 'blame-reveal--recolor-and-render)
                     (lambda ()
                       (push (list (buffer-name) 'render) refreshed)))
                    ((symbol-function 'blame-reveal--force-update-header)
                     (lambda ()
                       (push (list (buffer-name) 'header) refreshed))))
            (blame-reveal--apply-recent-window-preset 'tight)
            (dolist (buffer (list buf-a buf-b))
              (should (member (list (buffer-name buffer) 'recent) refreshed))
              (should (member (list (buffer-name buffer) 'render) refreshed))
              (should (member (list (buffer-name buffer) 'header) refreshed)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest blame-reveal-apply-preset-default-refreshes-all-active-buffers-in-global-scope ()
  "Global preset changes should fully refresh every active blame buffer."
  (let ((buf-a (generate-new-buffer " *blame-reveal-a*"))
        (buf-b (generate-new-buffer " *blame-reveal-b*"))
        refreshed)
    (unwind-protect
        (with-current-buffer buf-a
          (setq-local blame-reveal-mode t)
          (setq buffer-file-name "/tmp/a.el")
          (kill-local-variable 'blame-reveal-header-style)
          (with-current-buffer buf-b
            (setq-local blame-reveal-mode t)
            (setq buffer-file-name "/tmp/b.el"))
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'blame-reveal--full-update)
                     (lambda ()
                       (push (buffer-name) refreshed))))
            (blame-reveal--apply-preset-default)
            (should (equal (sort refreshed #'string<)
                           (sort (list (buffer-name buf-a)
                                       (buffer-name buf-b))
                                 #'string<)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest blame-reveal-turn-on-mode-skips-ineligible-buffers ()
  "Global enable helper should ignore buffers that should not opt in."
  (dolist (case '(("*scratch*" nil nil)
                  (" temp" "/tmp/file.el" t)
                  ("notes.el" nil nil)))
    (pcase-let ((`(,name ,file ,git-tracked-p) case))
      (with-temp-buffer
        (rename-buffer name t)
        (setq buffer-file-name file)
        (let ((called nil))
          (cl-letf (((symbol-function 'vc-git-registered)
                     (lambda (_file) git-tracked-p))
                    ((symbol-function 'blame-reveal-mode)
                     (lambda (&optional arg)
                       (setq called arg))))
            (blame-reveal--turn-on-mode)
            (should-not called)))))))

(ert-deftest blame-reveal-turn-on-mode-enables-eligible-buffers ()
  "Global enable helper should enable regular git-tracked file buffers."
  (with-temp-buffer
    (rename-buffer "tracked.el" t)
    (setq buffer-file-name "/tmp/tracked.el")
    (let ((called nil))
      (cl-letf (((symbol-function 'vc-git-registered)
                 (lambda (_file) t))
                ((symbol-function 'blame-reveal-mode)
                 (lambda (&optional arg)
                   (setq called arg))))
        (blame-reveal--turn-on-mode)
        (should (equal called 1))))))

(ert-deftest blame-reveal-header-update-respects-transient-setup ()
  "Header updates should stay paused during transient setup."
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (setq buffer-file-name "/tmp/tracked.el")
      (setq-local blame-reveal--in-transient-setup t)
      (should-not (blame-reveal--should-update-header-p))
      (setq-local blame-reveal--in-transient-setup nil)
      (should (eq (current-buffer) (window-buffer (selected-window))))
      (should (blame-reveal--should-update-header-p)))))

(ert-deftest blame-reveal-header-update-rebuilds-missing-header-after-scroll-clear ()
  "Header refresh should rebuild when the overlay is gone but point stayed put."
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (insert "line 1\nline 2\n")
      (goto-char (point-min))
      (forward-line 1)
      (setq buffer-file-name "/tmp/tracked.el"
            blame-reveal--header-overlay nil
            blame-reveal--last-update-line (line-number-at-pos)
            blame-reveal--last-rendered-commit "abc1234")
      (let ((impl-calls 0))
        (cl-letf (((symbol-function 'blame-reveal--get-current-block)
                   (lambda () '("abc1234" . 2)))
                  ((symbol-function 'run-with-idle-timer)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args)
                     'fake-timer))
                  ((symbol-function 'blame-reveal--update-header-impl)
                   (lambda ()
                     (setq impl-calls (1+ impl-calls)))))
          (blame-reveal--update-header)
          (should (= impl-calls 1)))))))

(ert-deftest blame-reveal-get-current-block-invalidates-cache-across-same-commit-blocks ()
  "Cache must not treat separated blocks of the same commit as one block."
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-char (point-min))
    (setq blame-reveal--blame-data
          '((1 . "aaa") (2 . "aaa") (3 . "bbb") (4 . "aaa") (5 . "aaa"))
          blame-reveal--current-line-cache nil)
    (cl-letf (((symbol-function 'blame-reveal--find-block-from-overlay)
               (lambda (_line-num) nil)))
      (forward-line 1)
      (should (equal (blame-reveal--get-current-block) '("aaa" . 1)))
      (forward-line 2)
      (should (equal (blame-reveal--get-current-block) '("aaa" . 4)))))) 

(ert-deftest blame-reveal-scroll-handler-does-not-clear-header-preemptively ()
  "Scrolling should not blank the header before the debounced refresh runs."
  (with-temp-buffer
    (let ((blame-reveal--last-window-start 1)
          (blame-reveal--last-window-vscroll 0)
          (blame-reveal--in-transient-setup nil)
          (cleared-header 0)
          (cleared-fringe 0)
          (scheduled nil))
      (cl-letf (((symbol-function 'window-start)
                 (lambda (&optional _window) 20))
                ((symbol-function 'window-vscroll)
                 (lambda (&optional _window _pixels-p) 0))
                ((symbol-function 'blame-reveal--clear-header)
                 (lambda ()
                   (setq cleared-header (1+ cleared-header))))
                ((symbol-function 'blame-reveal--clear-overlays-by-type)
                 (lambda (type)
                   (when (eq type 'fringe)
                     (setq cleared-fringe (1+ cleared-fringe)))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (setq scheduled (cons fn args))
                   'fake-timer)))
        (blame-reveal--scroll-handler nil nil)
        (should (= cleared-header 0))
        (should (= cleared-fringe 1))
        (should scheduled)))))

(ert-deftest blame-reveal-scroll-handler-reacts-to-window-vscroll-changes ()
  "Pixel scrolling should schedule a refresh even if `window-start' stays put."
  (with-temp-buffer
    (let ((blame-reveal--last-window-start 20)
          (blame-reveal--last-window-vscroll 0)
          (blame-reveal--scroll-timer nil)
          (cleared-fringe 0)
          (scheduled nil))
      (cl-letf (((symbol-function 'window-start)
                 (lambda (&optional _window) 20))
                ((symbol-function 'window-vscroll)
                 (lambda (&optional _window _pixels-p) 12))
                ((symbol-function 'blame-reveal--clear-overlays-by-type)
                 (lambda (type)
                   (when (eq type 'fringe)
                     (setq cleared-fringe (1+ cleared-fringe)))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (setq scheduled (cons fn args))
                   'fake-timer)))
        (blame-reveal--scroll-handler nil nil)
        (should (= blame-reveal--last-window-vscroll 12))
        (should (= cleared-fringe 1))
        (should scheduled)))))

(ert-deftest blame-reveal-scroll-handler-impl-refreshes-render-and-sticky-header ()
  "Debounced scroll refresh should render and then refresh sticky header."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (buffer-file-name "/tmp/tracked.el")
          (blame-reveal--state-status 'idle)
          (blame-reveal--state-operation nil)
          (calls nil))
      (cl-letf (((symbol-function 'blame-reveal--maybe-expand-blame-range)
                 (lambda ()
                   (push 'expand calls)))
                ((symbol-function 'blame-reveal--render-visible-region)
                 (lambda ()
                   (push 'render calls)))
                ((symbol-function 'blame-reveal--update-sticky-header)
                 (lambda ()
                   (push 'sticky calls))))
        (blame-reveal--scroll-handler-impl (current-buffer))
        (should (equal calls '(sticky render expand)))))))

(ert-deftest blame-reveal-update-sticky-header-normalizes-partial-vscroll ()
  "Sticky header should snap away partial line vscroll before showing."
  (with-temp-buffer
    (let ((blame-reveal--sticky-header-state nil)
          (blame-reveal--sticky-header-overlay nil)
          (set-vscroll-args nil))
      (cl-letf (((symbol-function 'blame-reveal--get-current-block)
                 (lambda () '("abc1234" . 5)))
                ((symbol-function 'line-number-at-pos)
                 (lambda (&optional pos)
                   (if pos 5 6)))
                ((symbol-function 'window-start)
                 (lambda (&optional _window) 100))
                ((symbol-function 'window-vscroll)
                 (lambda (&optional _window _pixels-p) 8))
                ((symbol-function 'blame-reveal--should-show-sticky-header-p)
                 (lambda (_commit _block-start _current-line) t))
                ((symbol-function 'blame-reveal--ensure-sticky-header)
                 (lambda (_existing-overlay _commit-hash) 'sticky-overlay))
                ((symbol-function 'blame-reveal--replace-sticky-header-overlay)
                 (lambda (new-overlay)
                   (setq blame-reveal--sticky-header-overlay new-overlay)))
                ((symbol-function 'set-window-vscroll)
                 (lambda (&rest args)
                   (setq set-vscroll-args args))))
        (blame-reveal--update-sticky-header)
        (should (equal set-vscroll-args '(nil 0 t)))
        (should (eq blame-reveal--sticky-header-overlay 'sticky-overlay))
        (should (= (plist-get blame-reveal--sticky-header-state :window-vscroll) 0))))))

(ert-deftest blame-reveal-focus-enter-and-exit-manage-state ()
  "Focus mode entry and exit should update buffer-local state predictably."
  (with-temp-buffer
    (let ((cleared nil)
          (render-calls 0)
          (clear-header-state-calls 0)
          (header-calls 0)
          (messages nil)
          (original-header-line "existing header")
          (blame-reveal-mode t)
          (blame-reveal--current-block-commit nil)
          (blame-reveal--last-rendered-commit 'stale))
      (setq header-line-format original-header-line)
      (setq blame-reveal--commit-info (make-hash-table :test 'equal))
      (puthash "abcdef0123456789"
               '("abcdef0" "Ada" "2d" "Refactor focus mode" 0 "")
               blame-reveal--commit-info)
      (cl-letf (((symbol-function 'blame-reveal-focus--update-block-cache)
                 (lambda ()
                   (setq blame-reveal--focus-block-cache
                         '((10 "abcdef0123456789" 2)
                           (20 "abcdef0123456789" 3)))))
                ((symbol-function 'blame-reveal--clear-overlays-by-type)
                 (lambda (type)
                   (push type cleared)))
                ((symbol-function 'blame-reveal-focus--render-visible-region)
                 (lambda ()
                   (setq render-calls (1+ render-calls))))
                ((symbol-function 'blame-reveal--render-visible-region)
                 (lambda ()
                   (setq render-calls (1+ render-calls))))
                ((symbol-function 'blame-reveal--clear-header-state)
                 (lambda ()
                   (setq clear-header-state-calls (1+ clear-header-state-calls))
                   (setq blame-reveal--current-block-commit nil
                         blame-reveal--last-rendered-commit nil)))
                ((symbol-function 'blame-reveal--update-header)
                 (lambda ()
                   (setq header-calls (1+ header-calls))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (blame-reveal-focus--enter "abcdef0123456789")
        (should (equal blame-reveal--focused-commit "abcdef0123456789"))
        (should-not blame-reveal--current-block-commit)
        (should (stringp header-line-format))
        (should (string-match-p "Focus:" header-line-format))
        (should (string-match-p "abcdef0" header-line-format))
        (should (equal cleared '(temp-fringe fringe)))
        (should (= render-calls 1))
        (should (= clear-header-state-calls 1))
        (should (= header-calls 0))
        (should (string-match-p "C-c l f" (car messages)))

        (setq cleared nil
              messages nil)
        (blame-reveal-focus--exit)
        (should-not blame-reveal--focused-commit)
        (should-not blame-reveal--focus-block-cache)
        (should-not blame-reveal--current-block-commit)
        (should-not blame-reveal--last-rendered-commit)
        (should (equal header-line-format original-header-line))
        (should (equal cleared '(fringe)))
        (should (= render-calls 2))
        (should (= clear-header-state-calls 1))
        (should (= header-calls 1))
        (should (equal (car messages) "Focus mode exited"))))))

(ert-deftest blame-reveal-focus-setup-suppresses-regular-header-displays ()
  "Focus integration should suppress regular header and sticky overlays."
  (unwind-protect
      (progn
        (blame-reveal-focus--setup)
        (should (advice-member-p #'blame-reveal-focus--around-render-visible-region
                                 'blame-reveal--render-visible-region))
        (should (advice-member-p #'blame-reveal-focus--around-update-header
                                 'blame-reveal--update-header))
        (should (advice-member-p #'blame-reveal-focus--around-update-sticky-header
                                 'blame-reveal--update-sticky-header))
        (should-not (advice-member-p #'blame-reveal-focus--around-get-current-block
                                     'blame-reveal--get-current-block)))
    (blame-reveal-focus--teardown)))

(ert-deftest blame-reveal-focus-setup-reference-counts-global-integration ()
  "Focus setup/teardown should keep global advice installed until the last user exits."
  (unwind-protect
      (progn
        (blame-reveal-focus--teardown)
        (blame-reveal-focus--setup)
        (blame-reveal-focus--setup)
        (blame-reveal-focus--teardown)
        (should (advice-member-p #'blame-reveal-focus--around-render-visible-region
                                 'blame-reveal--render-visible-region))
        (should (advice-member-p #'blame-reveal-focus--around-update-header
                                 'blame-reveal--update-header))
        (blame-reveal-focus--teardown)
        (should-not (advice-member-p #'blame-reveal-focus--around-render-visible-region
                                     'blame-reveal--render-visible-region))
        (should-not (advice-member-p #'blame-reveal-focus--around-update-header
                                     'blame-reveal--update-header)))
    (blame-reveal-focus--teardown)))

(ert-deftest blame-reveal-focus-restores-header-line-updates-made-while-active ()
  "Focus mode should not clobber header-line changes made while it is active."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (header-calls 0)
          pending-refresh)
      (unwind-protect
          (progn
            (blame-reveal-focus--setup)
            (setq header-line-format "existing header")
            (setq blame-reveal--commit-info (make-hash-table :test 'equal))
            (puthash "abcdef0123456789"
                     '("abcdef0" "Ada" "2d" "Refactor focus mode" 0 "")
                     blame-reveal--commit-info)
            (cl-letf (((symbol-function 'blame-reveal-focus--update-block-cache)
                      (lambda ()
                         (setq blame-reveal--focus-block-cache
                               '((10 "abcdef0123456789" 2)))))
                      ((symbol-function 'run-with-timer)
                       (lambda (_secs _repeat function &rest args)
                         (setq pending-refresh (lambda () (apply function args)))
                         'focus-test-timer))
                      ((symbol-function 'cancel-timer)
                       #'ignore)
                      ((symbol-function 'blame-reveal-focus--render-visible-region)
                       #'ignore)
                      ((symbol-function 'blame-reveal--clear-overlays-by-type)
                       #'ignore)
                      ((symbol-function 'blame-reveal--clear-header-state)
                       #'ignore)
                      ((symbol-function 'blame-reveal--render-visible-region)
                       #'ignore)
                      ((symbol-function 'blame-reveal--update-header)
                       (lambda ()
                         (setq header-calls (1+ header-calls))))
                      ((symbol-function 'message)
                       #'ignore))
              (blame-reveal-focus--enter "abcdef0123456789")
              (setq header-line-format "external header")
              (funcall pending-refresh)
              (should (stringp header-line-format))
              (should (string-match-p "Focus:" header-line-format))
              (blame-reveal-focus--exit)
              (should (equal header-line-format "external header"))
              (should (= header-calls 1))))
        (blame-reveal-focus--teardown)))))

(ert-deftest blame-reveal-save-current-state-seeds-head-foundation ()
  "First recursive save from HEAD should include a dedicated HEAD base state."
  (with-temp-buffer
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (insert "line 1\nline 2\n")
      (goto-char (point-min))
      (setq blame-reveal--blame-stack nil
            blame-reveal--current-revision nil
            blame-reveal--revision-display nil
            blame-reveal--detect-moves t
            blame-reveal--blame-data '((1 . "a") (2 . "b"))
            blame-reveal--blame-data-range '(1 . 2)
            blame-reveal--timestamps '(1 . 2)
            blame-reveal--recent-commits '("a" "b"))
      (setq blame-reveal--commit-info (make-hash-table :test 'equal))
      (setq blame-reveal--color-map (make-hash-table :test 'equal))
      (setq blame-reveal--move-copy-metadata (make-hash-table :test 'equal))
      (puthash "a" '("a") blame-reveal--commit-info)
      (puthash "a" "#fff" blame-reveal--color-map)
      (puthash "a" '(:previous-file "old.el") blame-reveal--move-copy-metadata)
      (blame-reveal--save-current-state)
      (should (= (length blame-reveal--blame-stack) 2))
      (let ((current-state (car blame-reveal--blame-stack))
            (head-state (cadr blame-reveal--blame-stack)))
        (should-not (plist-get current-state :is-head-state))
        (should (plist-get head-state :is-head-state))
        (should-not (eq (plist-get head-state :commit-info) blame-reveal--commit-info))
        (should-not (eq (plist-get head-state :color-map) blame-reveal--color-map))
        (should-not (eq (plist-get head-state :move-copy-metadata)
                        blame-reveal--move-copy-metadata))
        (should (equal (plist-get head-state :blame-data) blame-reveal--blame-data))
        (should (equal (plist-get head-state :window-start) (window-start)))
        (should (equal (plist-get head-state :point) (point)))))))

(ert-deftest blame-reveal-blame-back-cancels-busy-and-restores-state ()
  "Going back should cancel active work before restoring the previous state."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (blame-reveal--blame-stack '((:revision "old")))
          (cancel-reason nil)
          (restored-state nil)
          (messages nil))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () t))
                ((symbol-function 'blame-reveal--state-cancel)
                 (lambda (reason)
                   (setq cancel-reason reason)))
                ((symbol-function 'blame-reveal--restore-state)
                 (lambda (state)
                   (setq restored-state state)
                   "HEAD"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (blame-reveal-blame-back)
        (should (equal cancel-reason "user navigated back"))
        (should (equal restored-state '(:revision "old")))
        (should-not blame-reveal--blame-stack)
        (should (equal (car messages) "Returned to HEAD"))))))

(ert-deftest blame-reveal-reset-to-head-restores-foundation-state ()
  "Reset to HEAD should prefer the saved HEAD foundation instead of reloading."
  (with-temp-buffer
    (let* ((head-state '(:revision nil :is-head-state t))
           (blame-reveal-mode t)
           (blame-reveal--current-revision "abc123")
           (blame-reveal--revision-display "abc123")
           (blame-reveal--auto-days-cache '(5 . 30))
           (blame-reveal--blame-stack (list '(:revision "def456") head-state))
           (restored-state nil)
           (loaded nil)
           (messages nil))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () nil))
                ((symbol-function 'blame-reveal--restore-state)
                 (lambda (state)
                   (setq restored-state state)
                   "HEAD"))
                ((symbol-function 'blame-reveal--load-blame-data)
                 (lambda ()
                   (setq loaded t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (blame-reveal-reset-to-head)
        (should (equal restored-state head-state))
        (should-not loaded)
        (should-not blame-reveal--blame-stack)
        (should-not blame-reveal--current-revision)
        (should-not blame-reveal--revision-display)
        (should-not blame-reveal--auto-days-cache)
        (should (equal (car messages) "Reset to HEAD"))))))

(ert-deftest blame-reveal-blame-recursively-pops-state-when-action-stops ()
  "A stopped recursive action should discard the just-saved top state."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (blame-reveal--blame-stack '(existing)))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () nil))
                ((symbol-function 'blame-reveal--get-current-block)
                 (lambda () '("abc123" . 1)))
                ((symbol-function 'blame-reveal--is-uncommitted-p)
                 (lambda (_commit) nil))
                ((symbol-function 'blame-reveal--save-current-state)
                 (lambda ()
                   (push 'saved blame-reveal--blame-stack)))
                ((symbol-function 'blame-reveal--analyze-recursive-target)
                 (lambda (_commit)
                   '(:action stop)))
                ((symbol-function 'blame-reveal--execute-recursive-action)
                 (lambda (_action-info) nil)))
        (blame-reveal-blame-recursively)
        (should (equal blame-reveal--blame-stack '(existing)))))))

(ert-deftest blame-reveal-blame-recursively-restores-on-error ()
  "A recursive blame error should restore the just-saved state."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (blame-reveal--blame-stack '(existing))
          (restored nil))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () nil))
                ((symbol-function 'blame-reveal--get-current-block)
                 (lambda () '("abc123" . 1)))
                ((symbol-function 'blame-reveal--is-uncommitted-p)
                 (lambda (_commit) nil))
                ((symbol-function 'blame-reveal--save-current-state)
                 (lambda ()
                   (push 'saved blame-reveal--blame-stack)))
                ((symbol-function 'blame-reveal--analyze-recursive-target)
                 (lambda (_commit)
                   '(:action blame-parent)))
                ((symbol-function 'blame-reveal--execute-recursive-action)
                 (lambda (_action-info)
                   (error "boom")))
                ((symbol-function 'blame-reveal--restore-state)
                 (lambda (state)
                   (setq restored state))))
        (should-error (blame-reveal-blame-recursively)
                      :type 'error)
        (should (eq restored 'saved))
        (should (equal blame-reveal--blame-stack '(existing)))))))

(ert-deftest blame-reveal-blame-at-revision-rejects-invalid-revision ()
  "Invalid revisions should fail before any state is saved."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (saved nil)
          (loaded nil))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () nil))
                ((symbol-function 'call-process)
                 (lambda (&rest _args) 1))
                ((symbol-function 'blame-reveal--save-current-state)
                 (lambda ()
                   (setq saved t)))
                ((symbol-function 'blame-reveal--load-blame-at-revision)
                 (lambda (_revision)
                   (setq loaded t))))
        (should-error (blame-reveal-blame-at-revision "bad-rev")
                      :type 'user-error)
        (should-not saved)
        (should-not loaded)))))

(ert-deftest blame-reveal-blame-at-revision-restores-state-on-load-error ()
  "Loader failure after revision validation should restore the saved state."
  (with-temp-buffer
    (let ((blame-reveal-mode t)
          (blame-reveal--blame-stack '(existing))
          (restored nil)
          (messages nil))
      (cl-letf (((symbol-function 'blame-reveal--state-is-busy-p)
                 (lambda () nil))
                ((symbol-function 'call-process)
                 (lambda (&rest _args) 0))
                ((symbol-function 'blame-reveal--save-current-state)
                 (lambda ()
                   (push 'saved blame-reveal--blame-stack)))
                ((symbol-function 'blame-reveal--load-blame-at-revision)
                 (lambda (_revision)
                   (error "load failed")))
                ((symbol-function 'blame-reveal--restore-state)
                 (lambda (state)
                   (setq restored state)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (should-error (blame-reveal-blame-at-revision "main")
                      :type 'error)
        (should (eq restored 'saved))
        (should (equal blame-reveal--blame-stack '(existing)))
        (should (string-match-p "Error in blame at revision"
                                (car messages)))))))

(provide 'blame-reveal-test)
;;; blame-reveal-test.el ends here
