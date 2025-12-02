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

(cl-defmethod transient-infix-set ((obj blame-reveal-boolean-variable) value)
  "Set boolean VALUE for OBJ."
  (let ((var (oref obj variable)))
    (oset obj value value)
    (if (local-variable-p var)
        (set (make-local-variable var) value)
      (set var value))))

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
  "Refresh transient menu after setting header-style to show/hide margin-side."
  (when (eq (oref obj variable) 'blame-reveal-header-style)
    ;; Short delay to let current operation complete
    (run-with-idle-timer 0.05 nil
                        (lambda ()
                          (when (and transient-current-command
                                    (eq transient-current-command 'blame-reveal-menu))
                            (transient-setup 'blame-reveal-menu))))))

;;; Display Settings Infixes

(transient-define-infix blame-reveal--infix-header-style ()
  "Header display style."
  :description "Header style"
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-header-style
  :display-map (blame-reveal--get-display-map 'header-style)
  :key "s"
  :reader (lambda (prompt _ _)
            (intern (completing-read
                     prompt '("block" "inline" "margin")
                     nil t))))

(transient-define-infix blame-reveal--infix-fringe-side ()
  "Fringe side for blame indicators."
  :description "Fringe side"
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-fringe-side
  :display-map (blame-reveal--get-display-map 'fringe-side)
  :key "f"
  :reader (lambda (prompt _ _)
            (intern (completing-read
                     prompt '("left-fringe" "right-fringe")
                     nil t))))

(transient-define-infix blame-reveal--infix-margin-side ()
  "Margin side when using margin header style."
  :description (lambda ()
                 (if (eq blame-reveal-header-style 'margin)
                     "Margin side"
                   "Margin side (inactive)"))
  :class 'blame-reveal-lisp-variable
  :variable 'blame-reveal-margin-side
  :display-map (blame-reveal--get-display-map 'margin-side)
  :key "m"
  :inapt-if-not (lambda () (eq blame-reveal-header-style 'margin))
  :reader (lambda (prompt _ _)
            (if (eq blame-reveal-header-style 'margin)
                (intern (completing-read
                         prompt '("left" "right")
                         nil t))
              (user-error "Margin side only applies when header-style is 'margin'"))))

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
  :key "q"
  :reader (lambda (prompt _ _)
            (intern (completing-read
                     prompt '("strict" "auto" "relaxed")
                     nil t))))

(transient-define-infix blame-reveal--infix-color-hue ()
  "Color scheme hue (0-360)."
  :description "Color hue"
  :class 'transient-lisp-variable
  :variable 'blame-reveal-color-scheme
  :key "H"
  :reader (lambda (prompt _ _)
            (let* ((current-hue (plist-get blame-reveal-color-scheme :hue))
                   (new-hue (read-number
                             (format "%s (current: %s): "
                                     prompt
                                     (or current-hue "default"))
                             (or current-hue 200))))
              (plist-put (copy-sequence blame-reveal-color-scheme) :hue new-hue))))

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

(transient-define-prefix blame-reveal-presets ()
  "Manage blame-reveal presets."
  ["Presets"
   [("1" "Default" blame-reveal--apply-preset-default)
    ("2" "Compact" blame-reveal--apply-preset-compact)
    ("3" "Minimal" blame-reveal--apply-preset-minimal)]
   [("q" "Back" transient-quit-one)]])

;;; Main Transient Menu

;;;###autoload (autoload 'blame-reveal-menu "blame-reveal-transient" nil t)
(transient-define-prefix blame-reveal-menu ()
  "Transient menu for blame-reveal configuration and commands."
  [:description blame-reveal--menu-title
   ["Display"
    ("=" blame-reveal--toggle-scope)
    (blame-reveal--infix-header-style)
    (blame-reveal--infix-fringe-side)
    (blame-reveal--infix-margin-side)]
   ["Colors"
    (blame-reveal--infix-days-limit)
    (blame-reveal--infix-gradient-quality)
    (blame-reveal--infix-color-hue)]
   ["Options"
    (blame-reveal--infix-show-uncommitted-fringe)
    (blame-reveal--infix-lazy-threshold)
    (blame-reveal--infix-async-blame)]]
  [["Navigation"
    :pad-keys t
    ("b" "Blame recursively" blame-reveal-blame-recursively :transient t)
    ("p" "Blame back" blame-reveal-blame-back :transient t)
    ("g" "Blame at revision" blame-reveal-blame-at-revision)
    ("r" "Reset to HEAD" blame-reveal-reset-to-head :transient t)]
   ["Inspect"
    :pad-keys t
    ("c" "Copy hash" blame-reveal-copy-commit-hash)
    ("v" "Show diff" blame-reveal-show-commit-diff)
    ("d" "Show details" blame-reveal-show-commit-details)
    ("h" "File history" blame-reveal-show-file-history)
    ("l" "Line history" blame-reveal-show-line-history)]
   ["Actions"
    :pad-keys t
    ("R" "Refresh" blame-reveal--full-update :transient t)
    ("P" "Presets" blame-reveal-presets :transient t)
    ("?" "Auto calc" blame-reveal-show-auto-calculation)
    ("C" "Clear cache" blame-reveal-clear-auto-cache :transient t)
    ("q" "Quit" transient-quit-one)]])

;;; Helper Functions for Integration

(defun blame-reveal-transient-setup-keys ()
  "Setup recommended keybindings for blame-reveal transient menu."
  (when (boundp 'blame-reveal-mode-map)
    (define-key blame-reveal-mode-map (kbd "C-c b m") #'blame-reveal-menu)
    (define-key blame-reveal-mode-map (kbd "C-c b p") #'blame-reveal-presets)))

(provide 'blame-reveal-transient)
;;; blame-reveal-transient.el ends here
