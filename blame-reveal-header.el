;;; blame-reveal-header.el --- Header formatting and rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Header display system with sticky header support.

;;; Code:

(require 'blame-reveal-core)
(require 'blame-reveal-overlay)
(require 'blame-reveal-color)

(defun blame-reveal--get-sticky-indicator (color)
  "Get sticky header indicator with COLOR.
Uses nerd-icons if available, otherwise falls back to unicode arrow."
  (if (and (fboundp 'nerd-icons-octicon)
           (require 'nerd-icons nil t))
      (concat (nerd-icons-octicon "nf-oct-fold_up" :face `(:foreground ,color)) " ")
    (propertize " " 'face `(:foreground ,color))))

(defun blame-reveal-format-header-default (commit-hash commit-info color)
  "Default header formatter."
  (if (string-match-p "^0+$" commit-hash)
      (make-blame-reveal-commit-display
       :lines (list (format "▸ %s" blame-reveal-uncommitted-label))
       :faces (list `(:foreground ,color :weight bold))
       :color color)
    (pcase-let ((`(,short-hash ,author ,_date ,summary ,_timestamp ,_description) commit-info))
      (make-blame-reveal-commit-display
       :lines (list (format "▸ %s · %s · %s" summary short-hash author))
       :faces (list `(:foreground ,color :weight bold))
       :color color))))

(defun blame-reveal--compress-to-inline (display)
  "Compress DISPLAY to single-line format."
  (let* ((lines (blame-reveal-commit-display-lines display))
         (color (blame-reveal-commit-display-color display))
         (non-empty-lines (cl-remove-if
                           (lambda (s) (or (null s) (string-empty-p s))) lines))
         (message-line (car non-empty-lines))
         (metadata-line (cadr non-empty-lines))
         (compressed (if metadata-line
                        (format "%s · %s" (string-trim message-line) (string-trim metadata-line))
                      (string-trim message-line))))
    (make-blame-reveal-commit-display
     :lines (list compressed)
     :faces (list `(:foreground ,color :weight bold :height 0.95))
     :color color)))

(defun blame-reveal--get-formatted-display (commit-hash style)
  "Get formatted display for COMMIT-HASH in STYLE."
  (let* ((info (gethash commit-hash blame-reveal--commit-info))
         (color (blame-reveal--get-commit-color commit-hash))
         (full-display (funcall blame-reveal-header-format-function commit-hash info color)))
    (pcase style
      ('normal full-display)
      ('inline (blame-reveal--compress-to-inline full-display)))))

(defun blame-reveal--format-header-string (lines faces fringe-face show-fringe
                                                 need-leading-newline &optional sticky-indicator)
  "Format header content string."
  (let ((result (if need-leading-newline "\n" "")))
    (dotimes (i (length lines))
      (let ((line (nth i lines)) (face (nth i faces)))
        (when (and line (not (string-empty-p line)))
          (when show-fringe
            (setq result (concat result (propertize "!" 'display
                                                   (list blame-reveal-style 'blame-reveal-full fringe-face)))))
          (when sticky-indicator (setq result (concat result sticky-indicator)))
          (setq result (concat result (propertize line 'face face) "\n")))))
    (when show-fringe
      (setq result (concat result (propertize "!" 'display (list blame-reveal-style 'blame-reveal-full fringe-face)))))
    result))

(defun blame-reveal--build-normal-header (line display fringe-face show-fringe)
  "Build normal header."
  (save-excursion
    (goto-char (point-min)) (forward-line (1- line))
    (let* ((pos (if (= line 1) (point-min) (progn (forward-line -1) (line-end-position))))
           (ov (make-overlay pos pos))
           (need-newline (not (= line 1))))
      (overlay-put ov 'blame-reveal t)
      (overlay-put ov 'blame-reveal-header t)
      (overlay-put ov 'before-string
                   (blame-reveal--format-header-string
                    (blame-reveal-commit-display-lines display)
                    (blame-reveal-commit-display-faces display)
                    fringe-face show-fringe need-newline))
      ov)))

(defun blame-reveal--build-inline-header (line display fringe-face show-fringe)
  "Build inline header."
  (save-excursion
    (goto-char (point-min)) (forward-line (1- line))
    (unless (eobp)
      (let* ((pos (line-end-position)) (ov (make-overlay pos pos)))
        (overlay-put ov 'blame-reveal t)
        (overlay-put ov 'blame-reveal-header t)
        (overlay-put ov 'after-string
                     (concat "  "
                            (propertize (car (blame-reveal-commit-display-lines display))
                                      'face (car (blame-reveal-commit-display-faces display)))
                            (when show-fringe
                              (propertize "\n!" 'display (list blame-reveal-style 'blame-reveal-full fringe-face)))))
        ov))))

(defun blame-reveal--build-sticky-header (display fringe-face show-fringe)
  "Build sticky header."
  (save-excursion
    (goto-char (window-start))
    (let* ((is-inline (eq blame-reveal-header-position 'after-first-line))
           (pos (if is-inline (line-end-position) (line-beginning-position)))
           (ov (make-overlay pos pos))
           (color (blame-reveal-commit-display-color display))
           (sticky-indicator (blame-reveal--get-sticky-indicator color)))
      (overlay-put ov 'blame-reveal-sticky t)
      (if is-inline
          (overlay-put ov 'after-string
                       (concat "  " sticky-indicator
                              (propertize (car (blame-reveal-commit-display-lines display))
                                        'face (car (blame-reveal-commit-display-faces display)))
                              (when show-fringe
                                (propertize "\n!" 'display (list blame-reveal-style 'blame-reveal-full fringe-face)))))
        (overlay-put ov 'before-string
                     (blame-reveal--format-header-string
                      (blame-reveal-commit-display-lines display)
                      (blame-reveal-commit-display-faces display)
                      fringe-face show-fringe nil sticky-indicator)))
      ov)))

(defun blame-reveal--build-header (context)
  "Build header overlay from CONTEXT."
  (let* ((commit (blame-reveal-header-context-commit-hash context))
         (line (blame-reveal-header-context-line-number context))
         (mode (blame-reveal-header-context-mode context))
         (show-fringe (blame-reveal-header-context-show-fringe-p context))
         (style (if (eq mode 'inline) 'inline 'normal))
         (display (blame-reveal--get-formatted-display commit style))
         (color (blame-reveal-commit-display-color display))
         (fringe-face (blame-reveal--ensure-fringe-face color)))
    (pcase mode
      ('normal  (blame-reveal--build-normal-header line display fringe-face show-fringe))
      ('inline  (blame-reveal--build-inline-header line display fringe-face show-fringe))
      ('sticky  (blame-reveal--build-sticky-header display fringe-face show-fringe)))))

(defun blame-reveal--get-header-line-count ()
  "Get header overlay line count."
  (if (eq blame-reveal-header-position 'after-first-line) 2
    (if blame-reveal--header-overlay
        (let ((str (overlay-get blame-reveal--header-overlay 'before-string)))
          (if str (1+ (cl-count ?\n str)) 2))
      2)))

(defun blame-reveal--is-header-visible-p (block-start-line window-start-line)
  "Check if header is visible."
  (if (eq blame-reveal-header-position 'after-first-line)
      (<= window-start-line block-start-line)
    (cond ((= block-start-line 1) (<= window-start-line 1))
          (t (<= window-start-line (1- block-start-line))))))

(defun blame-reveal--find-block-for-commit (commit-hash block-start-line)
  "Find block for COMMIT-HASH at BLOCK-START-LINE."
  (cl-loop for block in (blame-reveal--find-block-boundaries blame-reveal--blame-data)
           when (and (= (nth 0 block) block-start-line) (equal (nth 1 block) commit-hash))
           return block))

(defun blame-reveal--should-show-sticky-header-p (commit-hash block-start-line current-line)
  "Check if sticky header should show."
  (when-let* ((block-info (blame-reveal--find-block-for-commit commit-hash block-start-line))
              (block-length (nth 2 block-info))
              (block-end-line (+ block-start-line block-length -1))
              (window-start-line (line-number-at-pos (window-start))))
    (let* ((header-visible (blame-reveal--is-header-visible-p block-start-line window-start-line))
           (in-this-block (and (>= current-line block-start-line) (<= current-line block-end-line))))
      (and (not header-visible) in-this-block))))

(defun blame-reveal--clear-sticky-header ()
  "Clear sticky header."
  (when blame-reveal--sticky-header-overlay
    (delete-overlay blame-reveal--sticky-header-overlay)
    (setq blame-reveal--sticky-header-overlay nil)))

(defun blame-reveal--update-sticky-header ()
  "Update sticky header."
  (blame-reveal--clear-sticky-header)
  (when-let* ((current-block (blame-reveal--get-current-block))
              (commit-hash (car current-block))
              (block-start-line (cdr current-block))
              (current-line (line-number-at-pos)))
    (when (blame-reveal--should-show-sticky-header-p commit-hash block-start-line current-line)
      (setq blame-reveal--sticky-header-overlay (blame-reveal--create-sticky-header-overlay commit-hash)))))

(defun blame-reveal--create-header-overlay (line-number commit-hash color &optional no-fringe)
  "Create header overlay."
  (blame-reveal--build-header
   (make-blame-reveal-header-context
    :commit-hash commit-hash :line-number line-number
    :mode (if (eq blame-reveal-header-position 'after-first-line) 'inline 'normal)
    :show-fringe-p (not no-fringe))))

(defun blame-reveal--create-sticky-header-overlay (commit-hash)
  "Create sticky header overlay."
  (save-excursion
    (goto-char (window-start))
    (let* ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
           (show-fringe (if is-uncommitted blame-reveal-show-uncommitted-fringe t)))
      (blame-reveal--build-header
       (make-blame-reveal-header-context
        :commit-hash commit-hash :line-number (line-number-at-pos)
        :mode 'sticky :show-fringe-p show-fringe)))))

(provide 'blame-reveal-header)
;;; blame-reveal-header.el ends here
