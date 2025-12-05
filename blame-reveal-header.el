;;; blame-reveal-header.el --- Header formatting and rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Header display system with multiple styles:
;; - Block: traditional header above code
;; - Inline: compact header after first line
;; - Margin: header in window margin (left or right)

;;; Code:

(require 'blame-reveal-core)
(require 'blame-reveal-overlay)
(require 'blame-reveal-color)
(require 'cl-lib)

(defun blame-reveal--ensure-single-line-display (display style)
  "Ensure DISPLAY has exactly one line for inline/margin STYLE.
If DISPLAY has multiple lines, compress them into one line.
Logs a warning message if compression occurs.

Arguments:
  DISPLAY - A `blame-reveal-commit-display' struct
  STYLE   - Symbol, either 'inline or 'margin

Returns:
  Modified `blame-reveal-commit-display' struct with single line."
  (let ((lines (blame-reveal-commit-display-lines display)))
    (if (<= (length lines) 1)
        ;; Already single line or empty, return as-is
        display
      ;; Multiple lines detected: compress them
      (let* ((color (blame-reveal-commit-display-color display))
             (non-empty-lines (cl-remove-if
                               (lambda (s) (or (null s) (string-empty-p s)))
                               lines))
             (compressed (if (eq style 'margin)
                             ;; For margin: only take first line (space is critical)
                             (car non-empty-lines)
                           ;; For inline: join with separator
                           (mapconcat #'string-trim non-empty-lines " · "))))
        (message "Warning: %s format returned %d lines, compressed to 1 line"
                 style (length lines))
        (make-blame-reveal-commit-display
         :lines (list compressed)
         :faces (list `(:foreground ,color
                        :weight bold
                        :height ,(if (eq style 'margin) 0.9 0.95)))
         :color color)))))

(defun blame-reveal--is-margin-style-p ()
  "Check if currently using margin header style."
  (eq blame-reveal-header-style 'margin))

(defun blame-reveal--get-current-margin-side ()
  "Get current margin side (left or right).
Returns the configured side when in margin mode, nil otherwise."
  (when (blame-reveal--is-margin-style-p)
    blame-reveal-margin-side))

(defun blame-reveal--calculate-margin-width ()
  "Calculate needed margin width based on actual margin format function output."
  (if (not blame-reveal--commit-info)
      20  ; Default if no data yet

    (let ((max-width 12))  ; Minimum width
      ;; Calculate width based on actual formatted output
      (maphash (lambda (hash info)
                 (when info
                   (let* ((color (blame-reveal--get-commit-color hash))
                          ;; Get actual formatted display using custom function
                          (display (blame-reveal--get-formatted-display hash 'margin))
                          (formatted-text (car (blame-reveal-commit-display-lines display)))
                          (actual-width (length formatted-text)))
                     (setq max-width (max max-width actual-width)))))
               blame-reveal--commit-info)

      ;; Add padding and apply limit
      (let ((padding 2)
            (max-allowed 90))
        (min (+ max-width padding) max-allowed)))))  ; Cap at 90 to avoid too wide margins

(defun blame-reveal--ensure-window-margins ()
  "Ensure window margins are set correctly for margin style.
Saves original margin widths and applies calculated width to configured side."
  (when-let ((side (blame-reveal--get-current-margin-side)))
    (let ((needed-width (or blame-reveal--margin-width
                            (setq blame-reveal--margin-width
                                  (blame-reveal--calculate-margin-width)))))
      ;; Save and set margins based on side
      (if (eq side 'left)
          (progn
            (unless blame-reveal--original-left-margin-width
              (setq blame-reveal--original-left-margin-width
                    (or left-margin-width 0)))
            (setq-local left-margin-width needed-width)
            ;; Apply to all windows showing this buffer
            (dolist (win (get-buffer-window-list (current-buffer) nil t))
              (set-window-margins win needed-width nil)))
        ;; Right margin
        (unless blame-reveal--original-right-margin-width
          (setq blame-reveal--original-right-margin-width
                (or right-margin-width 0)))
        (setq-local right-margin-width needed-width)
        ;; Apply to all windows
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (set-window-margins win nil needed-width))))))

(defun blame-reveal--restore-window-margins ()
  "Restore original window margins when switching away from margin style."
  ;; Restore left margin if it was saved
  (when blame-reveal--original-left-margin-width
    (setq-local left-margin-width blame-reveal--original-left-margin-width)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (set-window-margins win blame-reveal--original-left-margin-width
                          (cdr (window-margins win))))
    (setq blame-reveal--original-left-margin-width nil))

  ;; Restore right margin if it was saved
  (when blame-reveal--original-right-margin-width
    (setq-local right-margin-width blame-reveal--original-right-margin-width)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (set-window-margins win
                          (car (window-margins win))
                          blame-reveal--original-right-margin-width))
    (setq blame-reveal--original-right-margin-width nil))

  ;; Clear cached width
  (setq blame-reveal--margin-width nil))

(defun blame-reveal--icon (name &optional color fallback)
  "Return icon for NAME using nerd-icons backends.
NAME like 'nf-PREFIX-ICONNAME'. COLOR sets :foreground; FALLBACK if unavailable."
  (let* ((face (when color `(:foreground ,color)))
         (backend (and (string-match "^nf-\\([^-]+\\)-" name)
                       (cdr (assoc (match-string 1 name)
                                   '(("ips" . "ipsicon") ("oct" . "octicon") ("pom" . "pomicon")
                                     ("powerline" . "powerline") ("fa" . "faicon") ("w" . "wicon")
                                     ("suc" . "sucicon") ("dev" . "devicon") ("cod" . "codicon")
                                     ("fl" . "flicon") ("md" . "mdicon"))))))
         (fn (and backend (intern-soft (concat "nerd-icons-" backend)))))
    (or (and fn (fboundp fn) (require 'nerd-icons nil t) (funcall fn name :face face))
        (propertize (or fallback name) 'face face))))

(defun blame-reveal--abbreviate-author (author)
  "Abbreviate AUTHOR name."
  (cond
   ((string-match "\\(.*?\\), *\\(.\\).*" author)
    (replace-match "\\2 \\1" nil nil author))
   ((string-match "\\(.\\).*?[. ]+\\(.*\\)" author)
    (replace-match "\\1 \\2" nil nil author))
   (t author)))

(defun blame-reveal--get-move-copy-meta (commit-hash)
  "Return plist (:previous-file ... :previous-commit ...)
for COMMIT-HASH if moved/copied to a different file, else nil."
  (when (and blame-reveal--move-copy-metadata
             (hash-table-p blame-reveal--move-copy-metadata))
    (let ((meta (gethash commit-hash blame-reveal--move-copy-metadata)))
      (when meta
        (let* ((prev-file (plist-get meta :previous-file))
               (prev-commit (plist-get meta :previous-commit))
               (current-file (file-relative-name (buffer-file-name)
                                                (vc-git-root (buffer-file-name)))))
          (when (and prev-file prev-commit
                     (not (string= current-file prev-file)))
            (list :previous-file prev-file
                  :previous-commit prev-commit)))))))

(defun blame-reveal--format-move-copy-for-header (move-meta color)
  "Format move/copy meta for multi-line header display.
Returns a cons cell (LINE . FACE) or nil."
  (let* ((prev-file (plist-get move-meta :previous-file))
         (prev-commit (plist-get move-meta :previous-commit))
         (icon (blame-reveal--icon "nf-md-arrow_right_bottom" "󱞩"))
         (line (format " %s %s · %s"
                       icon
                       prev-file
                       (substring prev-commit 0 7)))
         (face `(:foreground ,color :height 0.9 :slant italic)))
    (cons line face)))

(defun blame-reveal--format-move-copy-for-inline (move-meta)
  "Format move/copy meta as a short string for inline display.
Returns a string or nil."
  (let* ((prev-file (plist-get move-meta :previous-file))
         (prev-commit (plist-get move-meta :previous-commit))
         (icon (blame-reveal--icon "nf-oct-file_moved" "⇝")))
    (format " %s %s(%s)"
            icon
            (file-name-nondirectory prev-file)
            (substring prev-commit 0 7))))

(defun blame-reveal--format-move-copy-for-margin (move-meta)
  "Format move/copy meta as an icon string for margin prefix.
Returns a string (icon + space) or nil."
  (when move-meta
    (format "%s " (blame-reveal--icon "nf-oct-file_moved" "⇝"))))

(defun blame-reveal-format-header-default (commit-hash commit-info color)
  "Default header formatter with abbreviated author names and move/copy info."
  (if (string-match-p "^0+$" commit-hash)
      (make-blame-reveal-commit-display
       :lines (list (format "▸ %s" blame-reveal-uncommitted-label))
       :faces (list `(:foreground ,color :weight bold))
       :color color)
    (pcase-let ((`(,short-hash ,author ,date ,summary ,_timestamp ,_description) commit-info))
      (let* ((abbrev-author (blame-reveal--abbreviate-author author))
             (short-date (blame-reveal--shorten-time date))
             (icon (blame-reveal--icon "nf-cod-git_commit" ""))
             ;; 1. Base Line/Face
             (base-line (format "%s%s · %s · %s · %s"
                                icon abbrev-author summary short-date short-hash))
             (base-face `(:foreground ,color :weight bold))
             ;; 2. Move/Copy Line
             (move-meta (blame-reveal--get-move-copy-meta commit-hash))
             (move-line-cons (when move-meta
                               (blame-reveal--format-move-copy-for-header move-meta color))))

        (if move-line-cons
            (make-blame-reveal-commit-display
             :lines (list base-line (car move-line-cons))
             :faces (list base-face (cdr move-line-cons))
             :color color)
          (make-blame-reveal-commit-display
           :lines (list base-line)
           :faces (list base-face)
           :color color))))))

(defun blame-reveal-format-inline-default (commit-hash commit-info color)
  "Default inline format function."
  (if (string-match-p "^0+$" commit-hash)
      (make-blame-reveal-commit-display
       :lines (list (format "[%s]" blame-reveal-uncommitted-label))
       :faces (list `(:foreground ,color))
       :color color)
    (pcase-let ((`(,hash ,author ,_date ,summary ,_ ,_) commit-info))
      (let* ((icon (blame-reveal--icon "nf-cod-git_commit" ""))
             (abbrev-author (blame-reveal--abbreviate-author author))
             (base-text (format "%s%s · %s"
                                icon abbrev-author summary))
             (move-meta (blame-reveal--get-move-copy-meta commit-hash))
             (move-info (when move-meta
                          (blame-reveal--format-move-copy-for-inline move-meta)))
             (full-text (concat base-text (or move-info ""))))
        (make-blame-reveal-commit-display
         :lines (list full-text)
         :faces (list `(:foreground ,color :height 0.95))
         :color color)))))

(defun blame-reveal-format-margin-default (commit-hash commit-info color)
  "Default margin format function."
  (if (string-match-p "^0+$" commit-hash)
      (make-blame-reveal-commit-display
       :lines (list "Uncommitted")
       :faces (list `(:foreground ,color))
       :color color)
    (pcase-let ((`(,_hash ,author ,date ,_msg ,_ ,_) commit-info))
      (let* ((icon (blame-reveal--icon "nf-cod-git_commit" ""))
             (abbrev-author (blame-reveal--abbreviate-author author))
             (short-date (blame-reveal--shorten-time date))
             (base-text (format "%s%s · %s"
                                icon abbrev-author short-date))
             (move-meta (blame-reveal--get-move-copy-meta commit-hash))
             (prefix (blame-reveal--format-move-copy-for-margin move-meta))
             (full-text (concat (or prefix "") base-text)))
        (make-blame-reveal-commit-display
         :lines (list full-text)
         :faces (list `(:foreground ,color :height 0.9))
         :color color)))))

(defconst blame-reveal--time-formats
  '(("\\([0-9]+\\) minutes? ago" . "m")
    ("\\([0-9]+\\) hours? ago" . "h")
    ("\\([0-9]+\\) days? ago" . "d")
    ("\\([0-9]+\\) weeks? ago" . "w")
    ;; 使用 'mo' 避免与 'm' (minutes) 混淆
    ("\\([0-9]+\\) months? ago" . "mo")
    ("\\([0-9]+\\) years? ago" . "y"))
  "Alist of regex patterns and their compact suffixes for relative time shortening.")

(defun blame-reveal--shorten-time (date-string)
  "Shorten DATE-STRING to a very compact format using `blame-reveal--time-formats`.
Examples: '5 minutes ago' -> '5m', '2 years ago' -> '2y'."
  (cl-block nil
    (dolist (entry blame-reveal--time-formats)
      (let ((regex (car entry))
            (suffix (cdr entry)))
        (when (string-match regex date-string)
          (let ((number-part (match-string 1 date-string)))
            (when number-part
              (cl-return (concat number-part suffix)))))))
    date-string))

(defun blame-reveal--get-formatted-display (commit-hash style)
  "Get formatted display for COMMIT-HASH in STYLE.

Arguments:
  COMMIT-HASH - Commit hash string
  STYLE       - Symbol: 'block, 'inline, or 'margin

Returns:
  A `blame-reveal-commit-display' struct formatted for the requested style.

Processing flow:
  - Block:  Uses user's block format function
  - Inline: Uses custom inline function if set, otherwise uses default
  - Margin: Uses custom margin function if set, otherwise uses default compact format

For inline and margin styles, ensures the result is single-line."
(let* ((info (gethash commit-hash blame-reveal--commit-info))
         (color (blame-reveal--get-commit-color commit-hash))
         (display
          (pcase style
            ('margin
             ;; Margin: custom function or default (blame-reveal-format-margin-default)
             (if (and (boundp 'blame-reveal-margin-format-function)
                      blame-reveal-margin-format-function)
                 (funcall blame-reveal-margin-format-function commit-hash info color)
               (blame-reveal-format-margin-default commit-hash info color)))

            ('inline
             ;; Inline: custom function or default (blame-reveal-format-inline-default)
             (if (and (boundp 'blame-reveal-inline-format-function)
                      blame-reveal-inline-format-function)
                 (funcall blame-reveal-inline-format-function commit-hash info color)
               (blame-reveal-format-inline-default commit-hash info color)))

            ('block
             ;; Block: use configured block format function (defaults to blame-reveal-format-header-default)
             (funcall blame-reveal-header-format-function commit-hash info color)))))

    ;; Enforce single-line constraint for inline and margin styles
    (if (memq style '(inline margin))
        (blame-reveal--ensure-single-line-display display style)
      display)))

(defun blame-reveal--format-header-string (lines faces fringe-face show-fringe
                                                 need-leading-newline &optional sticky-indicator)
  "Format header content string."
  (let ((result (if need-leading-newline "\n" ""))
        (line-count (length lines)))
    (dotimes (i line-count)
      (let ((line (nth i lines)) (face (nth i faces)))
        (when (and line (not (string-empty-p line)))
          (when show-fringe
            (setq result (concat result (propertize "!" 'display
                                                   (list blame-reveal-fringe-side 'blame-reveal-full fringe-face)))))
          (when sticky-indicator (setq result (concat result sticky-indicator)))
          (setq result (concat result (propertize line 'face face)))
          ;; Only add newline for non-last lines
          (when (< i (1- line-count))
            (setq result (concat result "\n"))))))
    ;; For first line (line 1), add trailing newline to separate from code
    (when (not need-leading-newline)
      (setq result (concat result "\n")))
    (when show-fringe
      (setq result (concat result (propertize "!" 'display (list blame-reveal-fringe-side 'blame-reveal-full fringe-face)))))
    result))

(defun blame-reveal--build-block-header (line display fringe-face show-fringe)
  "Build block-style header (shows above code)."
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
  "Build inline-style header (shows after first line)."
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
                              (propertize "\n!" 'display (list blame-reveal-fringe-side 'blame-reveal-full fringe-face)))))
        ov))))

(defun blame-reveal--build-margin-header (line display fringe-face show-fringe)
  "Build margin-style header - displays in left or right margin."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (unless (eobp)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (ov (make-overlay bol (1+ bol)))
             (margin-text (car (blame-reveal-commit-display-lines display)))
             (margin-face (car (blame-reveal-commit-display-faces display)))
             (side (or (blame-reveal--get-current-margin-side) 'left)))
        (overlay-put ov 'blame-reveal t)
        (overlay-put ov 'blame-reveal-header t)
        ;; Use line-prefix to display in margin
        (overlay-put ov 'line-prefix
                     (propertize " " 'display
                                 `((margin ,(intern (format "%s-margin" side)))
                                   ,(propertize margin-text 'face margin-face))))
        ov))))

(defun blame-reveal--build-sticky-header (display fringe-face show-fringe)
  "Build sticky header."
  (save-excursion
    (goto-char (window-start))
    (let* ((is-inline (eq blame-reveal-header-style 'inline))
           (is-margin (blame-reveal--is-margin-style-p))
           (pos (if is-inline (line-end-position) (line-beginning-position)))
           (ov (make-overlay pos (if is-margin (line-end-position) pos)))
           (color (blame-reveal-commit-display-color display))
           (sticky-indicator (blame-reveal--icon "nf-oct-fold_up" color "")))
      (overlay-put ov 'blame-reveal-sticky t)
      (cond
       (is-margin
        ;; For margin mode sticky header
        (let* ((margin-text (car (blame-reveal-commit-display-lines display)))
               (margin-face (car (blame-reveal-commit-display-faces display)))
               (side (or (blame-reveal--get-current-margin-side) 'left)))
          (overlay-put ov 'line-prefix
                       (propertize " " 'display
                                   `((margin ,(intern (format "%s-margin" side)))
                                     ,(propertize (concat sticky-indicator margin-text)
                                                  'face margin-face))))))
       (is-inline
        (overlay-put ov 'after-string
                     (concat "  " sticky-indicator
                            (propertize (car (blame-reveal-commit-display-lines display))
                                      'face (car (blame-reveal-commit-display-faces display)))
                            (when show-fringe
                              (propertize "\n!" 'display (list blame-reveal-fringe-side 'blame-reveal-full fringe-face))))))
       (t
        (overlay-put ov 'before-string
                     (blame-reveal--format-header-string
                      (blame-reveal-commit-display-lines display)
                      (blame-reveal-commit-display-faces display)
                      fringe-face show-fringe nil sticky-indicator))))
      ov)))

(defun blame-reveal--build-header (context)
  "Build header overlay from CONTEXT."
  ;; Ensure window margins are set for margin style
  (when (blame-reveal--is-margin-style-p)
    (blame-reveal--ensure-window-margins))

  (let* ((commit (blame-reveal-header-context-commit-hash context))
         (line (blame-reveal-header-context-line-number context))
         (mode (blame-reveal-header-context-mode context))
         (show-fringe (blame-reveal-header-context-show-fringe-p context))
         (is-margin (blame-reveal--is-margin-style-p))
         ;; Determine display style based on mode and settings
         (style (pcase mode
                  ('inline 'inline)
                  ('margin 'margin)
                  ('sticky (cond
                            (is-margin 'margin)
                            ((eq blame-reveal-header-style 'inline) 'inline)
                            (t 'block)))
                  (_ 'block)))
         (display (blame-reveal--get-formatted-display commit style))
         (color (blame-reveal-commit-display-color display))
         (fringe-face (blame-reveal--ensure-fringe-face color)))
    (pcase mode
      ('block  (blame-reveal--build-block-header line display fringe-face show-fringe))
      ('inline (blame-reveal--build-inline-header line display fringe-face show-fringe))
      ('margin (blame-reveal--build-margin-header line display fringe-face show-fringe))
      ('sticky (blame-reveal--build-sticky-header display fringe-face show-fringe)))))

(defun blame-reveal--get-header-line-count ()
  "Get header overlay line count."
  (cond
   ((eq blame-reveal-header-style 'inline) 2)
   ((blame-reveal--is-margin-style-p) 1)
   (t (if blame-reveal--header-overlay
          (let ((str (overlay-get blame-reveal--header-overlay 'before-string)))
            (if str (1+ (cl-count ?\n str)) 2))
        2))))

(defun blame-reveal--is-header-visible-p (block-start-line window-start-line)
  "Check if header is visible."
  (cond
   ((blame-reveal--is-margin-style-p)
    (<= window-start-line block-start-line))
   ((eq blame-reveal-header-style 'inline)
    (<= window-start-line block-start-line))
   ((= block-start-line 1)
    (<= window-start-line 1))
   (t (<= window-start-line (1- block-start-line)))))

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
    :mode (cond
           ((eq blame-reveal-header-style 'inline) 'inline)
           ((blame-reveal--is-margin-style-p) 'margin)
           (t 'block))
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
