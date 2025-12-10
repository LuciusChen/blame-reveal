;;; blame-reveal-transient.el --- Transient menu for blame-reveal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lucius Chen

;; Author: Lucius Chen
;; Keywords: convenience, vc, git
;; Package-Requires: ((emacs "29.1") (transient "0.4.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Transient menu interface for blame-reveal, providing easy access to
;; configuration options and commands.

;;; Code:

(require 'transient)
(require 'blame-reveal)
(require 'cl-lib)

;;; Cached Constants

(defconst blame-reveal--display-maps
  '((header-style . ((block . "Block")
                     (inline . "Inline")
                     (margin . "Margin")))
    (fringe-side . ((left-fringe . "Left")
                    (right-fringe . "Right")))
    (margin-side . ((left . "Left")
                    (right . "Right")))
    (gradient-quality . ((strict . "Strict")
                         (auto . "Auto")
                         (relaxed . "Relaxed")))
    (async-blame . ((auto . "Auto")
                    (t . "Always")
                    (nil . "Never")))
    (days-limit . ((auto . "Auto"))))
  "Cached display maps for faster lookups.")

(defconst blame-reveal--config-variables
  '(blame-reveal-header-style
    blame-reveal-recent-days-limit
    blame-reveal-gradient-quality
    blame-reveal-fringe-side
    blame-reveal-margin-side
    blame-reveal-show-uncommitted-fringe
    blame-reveal-lazy-load-threshold
    blame-reveal-async-blame)
  "List of configuration variables for scope management.")

;;; Helper Functions

(defsubst blame-reveal--get-display-map (key)
  "Get display map for KEY from cache."
  (cdr (assq key blame-reveal--display-maps)))

(defsubst blame-reveal--buffer-local-p ()
  "Check if settings are buffer-local."
  (local-variable-p 'blame-reveal-header-style))

(defsubst blame-reveal--margin-active-p ()
  "Check if margin mode is active."
  (eq blame-reveal-header-style 'margin))

(defun blame-reveal--format-scope ()
  "Format scope indicator (cached)."
  (if (blame-reveal--buffer-local-p)
      "[Buf]"  ; Shorter for speed
    "[Global]"))

(defun blame-reveal--menu-title ()
  "Get menu title with scope and revision info."
  (let* ((title (propertize "Blame Reveal" 'face 'transient-heading))
         (scope (if (blame-reveal--buffer-local-p)
                    (propertize " [Buf]" 'face 'warning)
                  (propertize " [Global]" 'face 'transient-inactive-value)))
         (revision
          (when (and (boundp 'blame-reveal--revision-display)
                     blame-reveal--revision-display)
            (concat " | @"
                    (propertize blame-reveal--revision-display
                                'face 'font-lock-constant-face)))))

    (concat title scope (or revision ""))))

(defun blame-reveal--refresh-header-after-load (&optional _)
  "Refresh header after data reload.
Now uses unified flicker-free system - no special handling needed."
  (when blame-reveal-mode
    ;; Clear cache to force update-header to rebuild
    (setq blame-reveal--current-block-commit nil)
    (setq blame-reveal--last-rendered-commit nil)
    (setq blame-reveal--last-update-line nil)
    ;; Update header - flicker prevention handled by core system
    (blame-reveal--update-header)))

;;; Custom Variable Class

(defclass blame-reveal-lisp-variable (transient-lisp-variable)
  ((display-nil :initarg :display-nil :initform "nil")
   (display-map :initarg :display-map :initform nil))
  "Lisp variable class for blame-reveal with custom display options.")

(cl-defmethod transient-format-value ((obj blame-reveal-lisp-variable))
  "Format the value of OBJ for display."
  (let* ((value (oref obj value))
         (variable (oref obj variable))
         (display-value
          (cond
           ;; Special handling for color-scheme plist
           ((eq variable 'blame-reveal-color-scheme)
            (format "%d°" (or (plist-get value :hue) 200)))
           ;; Nil value
           ((null value)
            (oref obj display-nil))
           ;; Map lookup
           ((oref obj display-map)
            (or (cdr (assoc value (oref obj display-map)))
                (format "%s" value)))
           ;; Default
           (t (format "%s" value)))))
    ;; Minimize string operations
    (if (local-variable-p variable)
        (concat (propertize display-value 'face 'transient-value)
                (propertize " [buf]" 'face 'font-lock-comment-face))
      (propertize display-value 'face 'transient-value))))

(cl-defmethod transient-infix-set ((obj blame-reveal-lisp-variable) value)
  "Set VALUE for OBJ, respecting current scope."
  (let ((var (oref obj variable)))
    (oset obj value value)
    ;; Set buffer-local if currently buffer-local, otherwise global
    (if (local-variable-p var)
        (set (make-local-variable var) value)
      (set var value))))

;;; Boolean Variable Class

(defclass blame-reveal-boolean-variable (transient-lisp-variable)
  ((on-label :initarg :on-label :initform "On")
   (off-label :initarg :off-label :initform "Off"))
  "Boolean variable class for blame-reveal.")

(cl-defmethod transient-format-value ((obj blame-reveal-boolean-variable))
  "Format boolean value of OBJ."
  (let* ((value (oref obj value))
         (variable (oref obj variable))
         (label (if value (oref obj on-label) (oref obj off-label)))
         (face (if value 'transient-value 'transient-inactive-value)))
    (if (local-variable-p variable)
        (concat (propertize label 'face face)
                (propertize " [buf]" 'face 'font-lock-comment-face))
      (propertize label 'face face))))

(cl-defmethod transient-infix-read ((obj blame-reveal-boolean-variable))
  "Toggle boolean value."
  (not (oref obj value)))

;;; Scope Management

(transient-define-suffix blame-reveal--toggle-scope ()
  "Toggle between buffer-local and global scope for settings."
  :key "="
  :description (lambda ()
                 (if (blame-reveal--buffer-local-p)
                     "Scope: Buffer"
                   "Scope: Global"))
  :transient t
  (interactive)
  (if (blame-reveal--buffer-local-p)
      ;; Currently buffer-local, make global
      (progn
        (dolist (var blame-reveal--config-variables)
          (kill-local-variable var))
        (message "Switched to global scope"))
    ;; Currently global, make buffer-local
    (progn
      (dolist (var blame-reveal--config-variables)
        (make-local-variable var))
      (message "Switched to buffer-local scope"))))

;;; Auto-refresh after header-style change

(cl-defmethod transient-infix-set :after ((obj blame-reveal-lisp-variable) value)
  "Refresh display and transient menu after setting certain variables."
  (let ((var (oref obj variable)))
    (cond
     ;; Days limit or gradient quality changed
     ((or (eq var 'blame-reveal-recent-days-limit)
          (eq var 'blame-reveal-gradient-quality))
      (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
        (blame-reveal--update-recent-commits)
        (blame-reveal--recolor-and-render)
        (blame-reveal--force-update-header)))

     ;; Uncommitted fringe
     ((eq var 'blame-reveal-show-uncommitted-fringe)
      (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
        (blame-reveal--recolor-and-render)
        (blame-reveal--force-update-header)))

     ;; Lazy threshold or async blame
     ((or (eq var 'blame-reveal-lazy-load-threshold)
          (eq var 'blame-reveal-async-blame))
      ;; These don't need immediate refresh
      nil))))

;;; Display Settings Infixes

(transient-define-suffix blame-reveal--set-header-style (style)
  "Set header style to STYLE and update immediately."
  (interactive)
  (setq blame-reveal-header-style style)
  (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
    ;; Restore margins when switching away from margin
    (when (not (eq style 'margin))
      (blame-reveal--restore-window-margins))
    ;; Setup margins when switching to margin
    (when (eq style 'margin)
      (blame-reveal--ensure-window-margins))
    ;; Force immediate header refresh
    (blame-reveal--force-update-header))
  ;; Return to main menu
  (transient-setup 'blame-reveal-menu))

(transient-define-suffix blame-reveal--set-fringe-side (side)
  "Set fringe side to SIDE and update immediately."
  (interactive)
  (setq blame-reveal-fringe-side side)
  (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
    (blame-reveal--recolor-and-render)
    (blame-reveal--force-update-header))
  (transient-setup 'blame-reveal-menu))

(transient-define-suffix blame-reveal--set-margin-side (side)
  "Set margin side to SIDE and update immediately."
  (interactive)
  (setq blame-reveal-margin-side side)
  (when (and (boundp 'blame-reveal-mode) blame-reveal-mode
             (eq blame-reveal-header-style 'margin))
    (blame-reveal--restore-window-margins)
    (blame-reveal--ensure-window-margins)
    (blame-reveal--force-update-header))
  (transient-setup 'blame-reveal-menu))

;;; Move/Copy Detection

(transient-define-suffix blame-reveal-toggle-move-detection ()
  "Toggle move/copy detection and reload."
  :key "M"
  :description (lambda ()
                 (if blame-reveal--detect-moves
                     "Move/copy: Enabled"
                   "Move/copy: Disabled"))
  (interactive)
  (setq blame-reveal--detect-moves (not blame-reveal--detect-moves))
  (unless blame-reveal--detect-moves
    (setq blame-reveal--move-copy-metadata nil))
  ;; Ensure hook is registered (safe to call multiple times)
  (add-hook 'blame-reveal-after-load-hook
            #'blame-reveal--refresh-header-after-load
            nil t)
  (blame-reveal--full-update)
  (message "Move/copy detection: %s"
           (if blame-reveal--detect-moves "enabled" "disabled")))

;;; Color Settings Infixes

(transient-define-infix blame-reveal--infix-days-limit ()
  "Days limit for recent commits."
  :description "Days limit"
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-recent-days-limit
  :display-nil "All"
  :display-map (blame-reveal--get-display-map 'days-limit)
  :key "D"
  :reader (lambda (prompt _ _)
            (let ((choice (completing-read
                           prompt '("auto" "all" "30" "90" "180" "365" "custom")
                           nil t)))
              (pcase choice
                ("auto" 'auto)
                ("all" nil)
                ("custom" (read-number "Days: "))
                (_ (string-to-number choice))))))

(transient-define-infix blame-reveal--infix-gradient-quality ()
  "Gradient quality setting."
  :description "Gradient quality"
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-gradient-quality
  :display-map (blame-reveal--get-display-map 'gradient-quality)
  :key "Q"
  :reader (lambda (prompt _ _)
            (intern (completing-read
                     prompt '("strict" "auto" "relaxed")
                     nil t))))

(transient-define-suffix blame-reveal--edit-hue ()
  "Edit color scheme hue."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :hue))
         (new (read-number (format "Hue (0-360, current: %d): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :hue new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-dark-newest ()
  "Edit dark theme newest lightness."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :dark-newest))
         (new (read-number (format "Dark newest (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :dark-newest new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-dark-oldest ()
  "Edit dark theme oldest lightness."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :dark-oldest))
         (new (read-number (format "Dark oldest (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :dark-oldest new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-light-newest ()
  "Edit light theme newest lightness."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :light-newest))
         (new (read-number (format "Light newest (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :light-newest new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-light-oldest ()
  "Edit light theme oldest lightness."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :light-oldest))
         (new (read-number (format "Light oldest (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :light-oldest new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-sat-min ()
  "Edit saturation minimum."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :saturation-min))
         (new (read-number (format "Saturation min (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :saturation-min new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-suffix blame-reveal--edit-sat-max ()
  "Edit saturation maximum."
  :transient t
  (interactive)
  (let* ((current (plist-get blame-reveal-color-scheme :saturation-max))
         (new (read-number (format "Saturation max (current: %.2f): " current) current)))
    (setq blame-reveal-color-scheme
          (plist-put (copy-sequence blame-reveal-color-scheme) :saturation-max new))
    (when blame-reveal-mode
      (blame-reveal--recolor-and-render))))

(transient-define-prefix blame-reveal-header-style-menu ()
  "Select header display style."
  ["Header Style"
   ("b" "Block (above code)"
    (lambda () (interactive) (blame-reveal--set-header-style 'block)))
   ("i" "Inline (after first line)"
    (lambda () (interactive) (blame-reveal--set-header-style 'inline)))
   ("m" "Margin (left/right margin)"
    (lambda () (interactive) (blame-reveal--set-header-style 'margin)))]
  ["Actions"
   ("q" "Back" transient-quit-one)])

(transient-define-prefix blame-reveal-fringe-side-menu ()
  "Select fringe side for blame indicators."
  ["Fringe Side"
   ("l" "Left fringe"
    (lambda () (interactive) (blame-reveal--set-fringe-side 'left-fringe)))
   ("r" "Right fringe"
    (lambda () (interactive) (blame-reveal--set-fringe-side 'right-fringe)))]
  ["Actions"
   ("q" "Back" transient-quit-one)])

(transient-define-prefix blame-reveal-margin-side-menu ()
  "Select margin side (when header style is margin)."
  [:description
   (lambda ()
     (if (eq blame-reveal-header-style 'margin)
         "Margin Side"
       "Margin Side (only active when header style is 'margin')"))
   ("l" "Left margin"
    (lambda () (interactive) (blame-reveal--set-margin-side 'left))
    :inapt-if-not (lambda () (eq blame-reveal-header-style 'margin)))
   ("r" "Right margin"
    (lambda () (interactive) (blame-reveal--set-margin-side 'right))
    :inapt-if-not (lambda () (eq blame-reveal-header-style 'margin)))]
  ["Actions"
   ("q" "Back" transient-quit-one)])

;;;###autoload
(transient-define-prefix blame-reveal-color-scheme-menu ()
  "Configure color scheme."
  [:description
   (lambda () (format "Color Scheme [H:%d°]"
                      (plist-get blame-reveal-color-scheme :hue)))
   ["Quick Presets"
    ("1" "Blue"   (lambda () (interactive) (blame-reveal--apply-color-preset 'blue))   :transient t)
    ("2" "Green"  (lambda () (interactive) (blame-reveal--apply-color-preset 'green))  :transient t)
    ("3" "Purple" (lambda () (interactive) (blame-reveal--apply-color-preset 'purple)) :transient t)
    ("4" "Orange" (lambda () (interactive) (blame-reveal--apply-color-preset 'orange)) :transient t)
    ("5" "Subtle" (lambda () (interactive) (blame-reveal--apply-color-preset 'subtle)) :transient t)
    ("6" "Vivid"  (lambda () (interactive) (blame-reveal--apply-color-preset 'vivid))  :transient t)]
   ["Hue"
    ("h" "Hue (0-360)" blame-reveal--edit-hue)]
   ["Dark Theme"
    ("n" "Newest"      blame-reveal--edit-dark-newest)
    ("o" "Oldest"      blame-reveal--edit-dark-oldest)]
   ["Light Theme"
    ("N" "Newest"      blame-reveal--edit-light-newest)
    ("O" "Oldest"      blame-reveal--edit-light-oldest)]
   ["Saturation"
    ("s" "Min"         blame-reveal--edit-sat-min)
    ("S" "Max"         blame-reveal--edit-sat-max)]]
  [["Actions"
    ("R" "Refresh"     blame-reveal--full-update :transient t)
    ("q" "Back"        transient-quit-one)]])

;;; Options Infixes

(transient-define-infix blame-reveal--infix-show-uncommitted-fringe ()
  "Show fringe for uncommitted changes."
  :description "Uncommitted fringe"
  :class 'blame-reveal-boolean-variable
  :variable 'blame-reveal-show-uncommitted-fringe
  :on-label "Show"
  :off-label "Hide"
  :key "u")

(transient-define-infix blame-reveal--infix-lazy-threshold ()
  "Lazy load threshold."
  :description "Lazy threshold"
  :class 'transient-lisp-variable
  :variable 'blame-reveal-lazy-load-threshold
  :key "L"
  :reader (lambda (prompt _ _)
            (read-number prompt blame-reveal-lazy-load-threshold)))

(transient-define-infix blame-reveal--infix-async-blame ()
  "Async blame loading."
  :description "Async loading"
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-async-blame
  :display-map (blame-reveal--get-display-map 'async-blame)
  :key "a"
  :reader (lambda (prompt _ _)
            (let ((choice (completing-read
                           prompt '("auto" "always" "never")
                           nil t)))
              (pcase choice
                ("auto" 'auto)
                ("always" t)
                ("never" nil)))))

;;; Preset Management

(transient-define-suffix blame-reveal--apply-preset-default ()
  "Apply default preset."
  :key "1"
  :description "Default preset"
  :transient t
  (interactive)
  (setq blame-reveal-header-style 'block
        blame-reveal-recent-days-limit 'auto
        blame-reveal-gradient-quality 'auto
        blame-reveal-fringe-side 'left-fringe)
  (when blame-reveal-mode
    (blame-reveal--full-update))
  (message "Applied default preset"))

(transient-define-suffix blame-reveal--apply-preset-compact ()
  "Apply compact preset."
  :key "2"
  :description "Compact preset"
  :transient t
  (interactive)
  (setq blame-reveal-header-style 'inline
        blame-reveal-recent-days-limit 90
        blame-reveal-gradient-quality 'strict
        blame-reveal-fringe-side 'left-fringe)
  (when blame-reveal-mode
    (blame-reveal--full-update))
  (message "Applied compact preset"))

(transient-define-suffix blame-reveal--apply-preset-minimal ()
  "Apply minimal preset."
  :key "3"
  :description "Minimal preset"
  :transient t
  (interactive)
  (setq blame-reveal-header-style 'margin
        blame-reveal-recent-days-limit 30
        blame-reveal-show-uncommitted-fringe nil
        blame-reveal-gradient-quality 'relaxed)
  (when blame-reveal-mode
    (blame-reveal--full-update))
  (message "Applied minimal preset"))

;;;###autoload
(transient-define-prefix blame-reveal-presets ()
  "Manage blame-reveal presets."
  ["Presets"
   [("1" "Default" blame-reveal--apply-preset-default)
    ("2" "Compact" blame-reveal--apply-preset-compact)
    ("3" "Minimal" blame-reveal--apply-preset-minimal)]
   [("q" "Back" transient-quit-one)]])

;;; Main Transient Menu
;;;###autoload
(transient-define-prefix blame-reveal-menu ()
  "Transient menu for blame-reveal configuration."
  [:description blame-reveal--menu-title
   ["Display"
    ("="                   blame-reveal--toggle-scope)
    ("h" "Header style..." blame-reveal-header-style-menu)
    ("f" "Fringe side..."  blame-reveal-fringe-side-menu)
    ("m" "Margin side..."  blame-reveal-margin-side-menu)]
   ["Colors"
    (blame-reveal--infix-days-limit)
    (blame-reveal--infix-gradient-quality)
    ("H" "Color scheme..." blame-reveal-color-scheme-menu)]
   ["Options"
    (blame-reveal--infix-show-uncommitted-fringe)
    (blame-reveal--infix-lazy-threshold)
    (blame-reveal--infix-async-blame)
    ("M" blame-reveal-toggle-move-detection)]]

  ["Utilities"
   :pad-keys t
   ("R" "Refresh"        blame-reveal--full-update     :transient t)
   ("P" "Presets"        blame-reveal-presets          :transient t)
   ("?" "Auto calc info" blame-reveal-show-auto-calculation)
   ("C" "Clear cache"    blame-reveal-clear-auto-cache :transient t)
   ("q" "Quit"           transient-quit-one)])

(provide 'blame-reveal-transient)
;;; blame-reveal-transient.el ends here
