;;; blame-reveal-config-switcher.el --- Quick configuration switcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility for quickly switching between different blame-reveal configurations.
;; Useful for testing and comparing different settings.

;;; Code:

(require 'blame-reveal)

;;; Configuration Presets

(defvar blame-reveal-config-presets
  '((default
     :description "Default balanced settings"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (strict-recent
     :description "Strict mode - only very recent commits (best color distinction)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . strict)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.75
                                             :dark-oldest 0.30
                                             :light-newest 0.35
                                             :light-oldest 0.85
                                             :saturation-min 0.35
                                             :saturation-max 0.70))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (relaxed-historical
     :description "Relaxed mode - more historical context (more commits)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . relaxed)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.60
                                             :dark-oldest 0.40
                                             :light-newest 0.55
                                             :light-oldest 0.70
                                             :saturation-min 0.20
                                             :saturation-max 0.45))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (show-all
     :description "Show all commits (no time limit)"
     :config ((blame-reveal-recent-days-limit . nil)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (fixed-30-days
     :description "Fixed 30 days window"
     :config ((blame-reveal-recent-days-limit . 30)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (fixed-90-days
     :description "Fixed 90 days window (quarter)"
     :config ((blame-reveal-recent-days-limit . 90)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (green-theme
     :description "Green color scheme"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 120
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.40
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (purple-theme
     :description "Purple color scheme"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 280
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (orange-theme
     :description "Orange/warm color scheme"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 30
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (inline-header
     :description "Inline header style (header after first line)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . after-first-line)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (show-uncommitted
     :description "Show uncommitted changes in fringe (use with diff-hl)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . t)
              (blame-reveal-async-blame . auto)
              (blame-reveal-lazy-load-threshold . 3000)))

    (always-async
     :description "Always use async loading (for large repos)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . t)
              (blame-reveal-lazy-load-threshold . 1000)))

    (minimal-lazy
     :description "Aggressive lazy loading (for very large files)"
     :config ((blame-reveal-recent-days-limit . auto)
              (blame-reveal-gradient-quality . auto)
              (blame-reveal-header-position . before-block)
              (blame-reveal-color-scheme . (:hue 210
                                             :dark-newest 0.70
                                             :dark-oldest 0.35
                                             :light-newest 0.45
                                             :light-oldest 0.75
                                             :saturation-min 0.25
                                             :saturation-max 0.60))
              (blame-reveal-show-uncommitted-fringe . nil)
              (blame-reveal-async-blame . t)
              (blame-reveal-lazy-load-threshold . 500))))
  "Predefined configuration presets for blame-reveal.")

;;; Configuration Management

(defvar blame-reveal--saved-config nil
  "Saved configuration for restoration.")

(defvar blame-reveal--current-preset nil
  "Currently active preset name.")

(defun blame-reveal-save-current-config ()
  "Save current configuration."
  (setq blame-reveal--saved-config
        (list :recent-days-limit blame-reveal-recent-days-limit
              :gradient-quality blame-reveal-gradient-quality
              :header-position blame-reveal-header-position
              :color-scheme blame-reveal-color-scheme
              :show-uncommitted-fringe blame-reveal-show-uncommitted-fringe
              :async-blame blame-reveal-async-blame
              :lazy-load-threshold blame-reveal-lazy-load-threshold)))

(defun blame-reveal-restore-saved-config ()
  "Restore previously saved configuration."
  (when blame-reveal--saved-config
    (setq blame-reveal-recent-days-limit
          (plist-get blame-reveal--saved-config :recent-days-limit))
    (setq blame-reveal-gradient-quality
          (plist-get blame-reveal--saved-config :gradient-quality))
    (setq blame-reveal-header-position
          (plist-get blame-reveal--saved-config :header-position))
    (setq blame-reveal-color-scheme
          (plist-get blame-reveal--saved-config :color-scheme))
    (setq blame-reveal-show-uncommitted-fringe
          (plist-get blame-reveal--saved-config :show-uncommitted-fringe))
    (setq blame-reveal-async-blame
          (plist-get blame-reveal--saved-config :async-blame))
    (setq blame-reveal-lazy-load-threshold
          (plist-get blame-reveal--saved-config :lazy-load-threshold))
    (setq blame-reveal--current-preset nil)))

(defun blame-reveal-apply-config (config)
  "Apply CONFIG settings."
  (dolist (item config)
    (set (car item) (cdr item))))

(defun blame-reveal-refresh-display ()
  "Refresh blame-reveal display with new configuration."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when blame-reveal-mode
        ;; Clear color cache and strategy
        (setq blame-reveal--color-map (make-hash-table :test 'equal))
        (setq blame-reveal--color-strategy nil)
        (setq blame-reveal--recent-commits nil)
        (setq blame-reveal--auto-calculated-days nil)
        ;; Re-initialize color strategy with new settings
        (blame-reveal--init-color-strategy)
        ;; Update recent commits
        (blame-reveal--update-recent-commits)
        ;; Re-render
        (blame-reveal--recolor-and-render)))))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-switch-preset (preset-name)
  "Switch to a predefined configuration PRESET-NAME."
  (interactive
   (list (intern (completing-read
                  "Select preset: "
                  (mapcar (lambda (p)
                            (format "%s - %s"
                                    (car p)
                                    (plist-get (cdr p) :description)))
                          blame-reveal-config-presets)
                  nil t))))
  (let* ((preset-name-symbol (if (symbolp preset-name)
                                 preset-name
                               (intern (car (split-string (symbol-name preset-name) " - ")))))
         (preset (assq preset-name-symbol blame-reveal-config-presets)))
    (if preset
        (let ((config (plist-get (cdr preset) :config))
              (description (plist-get (cdr preset) :description)))
          ;; Save current config if this is first switch
          (unless blame-reveal--saved-config
            (blame-reveal-save-current-config))
          ;; Apply new config
          (blame-reveal-apply-config config)
          (setq blame-reveal--current-preset preset-name-symbol)
          ;; Refresh display
          (blame-reveal-refresh-display)
          (message "Switched to preset: %s - %s" preset-name-symbol description))
      (error "Unknown preset: %s" preset-name))))

;;;###autoload
(defun blame-reveal-restore-default-config ()
  "Restore saved configuration (before preset switching)."
  (interactive)
  (if blame-reveal--saved-config
      (progn
        (blame-reveal-restore-saved-config)
        (blame-reveal-refresh-display)
        (message "Restored saved configuration"))
    (message "No saved configuration to restore")))

;;;###autoload
(defun blame-reveal-show-current-config ()
  "Show current configuration in a readable format."
  (interactive)
  (let ((config-buffer (get-buffer-create "*Blame Reveal Config*")))
    (with-current-buffer config-buffer
      (erase-buffer)
      (insert (propertize "Current Blame Reveal Configuration\n"
                          'face '(:weight bold :height 1.2)))
      (insert (propertize (make-string 50 ?═) 'face '(:foreground "#666")))
      (insert "\n\n")

      (when blame-reveal--current-preset
        (insert (propertize "Active Preset: " 'face '(:weight bold))
                (propertize (format "%s\n" blame-reveal--current-preset)
                            'face '(:foreground "#6c9ef8")))
        (let ((preset (assq blame-reveal--current-preset blame-reveal-config-presets)))
          (when preset
            (insert (propertize "Description: " 'face '(:foreground "#888"))
                    (plist-get (cdr preset) :description) "\n")))
        (insert "\n"))

      (insert (propertize "Settings:\n" 'face '(:weight bold)))
      (insert (propertize (make-string 50 ?─) 'face '(:foreground "#444")))
      (insert "\n\n")

      (let ((settings `(("Time Window" . ,blame-reveal-recent-days-limit)
                        ("Gradient Quality" . ,blame-reveal-gradient-quality)
                        ("Header Position" . ,blame-reveal-header-position)
                        ("Show Uncommitted Fringe" . ,blame-reveal-show-uncommitted-fringe)
                        ("Async Blame" . ,blame-reveal-async-blame)
                        ("Lazy Load Threshold" . ,blame-reveal-lazy-load-threshold))))
        (dolist (setting settings)
          (insert (propertize (format "%-25s: " (car setting))
                              'face '(:foreground "#a0a0a0")))
          (insert (propertize (format "%s\n" (cdr setting))
                              'face '(:foreground "#d0d0d0")))))

      (insert "\n")
      (insert (propertize "Color Scheme:\n" 'face '(:weight bold)))
      (insert (propertize (make-string 50 ?─) 'face '(:foreground "#444")))
      (insert "\n\n")

      (let ((scheme blame-reveal-color-scheme))
        (insert (propertize (format "  Hue: %s°\n" (plist-get scheme :hue))
                            'face '(:foreground "#d0d0d0")))
        (insert (propertize (format "  Dark Theme:\n")
                            'face '(:foreground "#a0a0a0")))
        (insert (propertize (format "    Newest: %.2f, Oldest: %.2f\n"
                                    (plist-get scheme :dark-newest)
                                    (plist-get scheme :dark-oldest))
                            'face '(:foreground "#888")))
        (insert (propertize (format "  Light Theme:\n")
                            'face '(:foreground "#a0a0a0")))
        (insert (propertize (format "    Newest: %.2f, Oldest: %.2f\n"
                                    (plist-get scheme :light-newest)
                                    (plist-get scheme :light-oldest))
                            'face '(:foreground "#888")))
        (insert (propertize (format "  Saturation: %.2f - %.2f\n"
                                    (plist-get scheme :saturation-min)
                                    (plist-get scheme :saturation-max))
                            'face '(:foreground "#888"))))

      (insert "\n")
      (insert (propertize "Quick Actions:\n" 'face '(:weight bold)))
      (insert (propertize (make-string 50 ?─) 'face '(:foreground "#444")))
      (insert "\n\n")
      (insert "  M-x blame-reveal-switch-preset       - Switch to a preset\n")
      (insert "  M-x blame-reveal-restore-default-config - Restore saved config\n")
      (insert "  M-x blame-reveal-show-auto-calculation  - Show auto calculation\n")

      (special-mode)
      (local-set-key (kbd "q") 'quit-window))
    (pop-to-buffer config-buffer)))

;;;###autoload
(defun blame-reveal-quick-switch ()
  "Quick switch between common presets using single key."
  (interactive)
  (let ((choice (read-char-choice
                 (concat "Quick switch:\n"
                         "  [d] Default\n"
                         "  [s] Strict (best distinction)\n"
                         "  [r] Relaxed (more history)\n"
                         "  [a] Show all commits\n"
                         "  [g] Green theme\n"
                         "  [p] Purple theme\n"
                         "  [o] Orange theme\n"
                         "  [i] Inline header\n"
                         "  [q] Quit\n"
                         "Choice: ")
                 '(?d ?s ?r ?a ?g ?p ?o ?i ?q))))
    (pcase choice
      (?d (blame-reveal-switch-preset 'default))
      (?s (blame-reveal-switch-preset 'strict-recent))
      (?r (blame-reveal-switch-preset 'relaxed-historical))
      (?a (blame-reveal-switch-preset 'show-all))
      (?g (blame-reveal-switch-preset 'green-theme))
      (?p (blame-reveal-switch-preset 'purple-theme))
      (?o (blame-reveal-switch-preset 'orange-theme))
      (?i (blame-reveal-switch-preset 'inline-header))
      (?q (message "Cancelled"))
      (_ (message "Invalid choice")))))

;;; Comparison Mode

;;;###autoload
(defun blame-reveal-compare-presets (&optional preset1 preset2)
  "Compare two presets side by side.
If PRESET1 and PRESET2 are not provided, prompt for them."
  (interactive)
  (let ((p1 (or preset1
                (intern (completing-read
                         "First preset: "
                         (mapcar #'car blame-reveal-config-presets)
                         nil t))))
        (p2 (or preset2
                (intern (completing-read
                         "Second preset: "
                         (mapcar #'car blame-reveal-config-presets)
                         nil t)))))
    (let ((config1 (plist-get (cdr (assq p1 blame-reveal-config-presets)) :config))
          (config2 (plist-get (cdr (assq p2 blame-reveal-config-presets)) :config))
          (buffer (get-buffer-create "*Preset Comparison*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (propertize "Preset Comparison\n" 'face '(:weight bold :height 1.2)))
        (insert (propertize (make-string 70 ?═) 'face '(:foreground "#666")))
        (insert "\n\n")
        (insert (format "%-35s | %s\n"
                        (propertize (symbol-name p1) 'face '(:foreground "#6c9ef8"))
                        (propertize (symbol-name p2) 'face '(:foreground "#6c9ef8"))))
        (insert (propertize (make-string 70 ?─) 'face '(:foreground "#444")))
        (insert "\n")

        (dolist (key '(blame-reveal-recent-days-limit
                       blame-reveal-gradient-quality
                       blame-reveal-header-position
                       blame-reveal-show-uncommitted-fringe))
          (let ((val1 (alist-get key config1))
                (val2 (alist-get key config2)))
            (insert (format "%-35s | %s\n"
                            (format "%s" val1)
                            (format "%s" val2)))))

        (special-mode)
        (local-set-key (kbd "q") 'quit-window))
      (pop-to-buffer buffer))))

(provide 'blame-reveal-config-switcher)
;;; blame-reveal-config-switcher.el ends here
