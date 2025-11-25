;;; blame-reveal.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in the fringe with color blocks.
;; Color intensity based on commit recency (newer = more prominent).
;; Show commit info above each block.
;; Performance optimized: lazy loading and visible-region rendering.
;;
;; Features:
;; - Lazy loading for large files (configurable threshold)
;; - Incremental commit info loading
;; - Recursive blame navigation
;; - Theme-aware color schemes
;; - Customizable display layouts

;;; Code:

(require 'vc-git)


;;;; Fringe Bitmap Definition

(define-fringe-bitmap 'blame-reveal-full
  [#b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111]
  16 8 'center)


;;;; Keymaps

(defvar blame-reveal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'blame-reveal-mode)
    (define-key map (kbd "c") #'blame-reveal-copy-commit-hash)
    (define-key map (kbd "d") #'blame-reveal-show-commit-diff)
    (define-key map (kbd "s") #'blame-reveal-show-commit-details)
    (define-key map (kbd "h") #'blame-reveal-show-file-history)
    (define-key map (kbd "l") #'blame-reveal-show-line-history)
    (define-key map (kbd "b") #'blame-reveal-blame-recursively)
    (define-key map (kbd "p") #'blame-reveal-blame-back)
    (define-key map (kbd "^") #'blame-reveal-blame-back)
    (define-key map (kbd "g") #'blame-reveal-blame-at-revision)
    (define-key map (kbd "r") #'blame-reveal-reset-to-head)
    map)
  "Keymap for blame-reveal-mode.")

(defvar blame-reveal--emulation-alist nil
  "Emulation mode map alist for blame-reveal.")


;;;; Timer Variables

(defvar blame-reveal--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar blame-reveal--scroll-timer nil
  "Timer to debounce scroll updates.")


;;;; Buffer-Local State Variables

(defvar-local blame-reveal--temp-overlay-timer nil
  "Timer for delayed temp overlay rendering.")

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--header-overlay nil
  "Overlay for the currently displayed header.")

(defvar-local blame-reveal--overlays nil
  "List of overlays used for blame display.")

(defvar-local blame-reveal--temp-old-overlays nil
  "Temporary overlays for old commit blocks when cursor is on them.")

(defvar-local blame-reveal--sticky-header-overlay nil
  "Overlay for sticky header at window top.")


;;;; Data Cache Variables

(defvar-local blame-reveal--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local blame-reveal--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defvar-local blame-reveal--blame-data nil
  "Cached blame data for the file.
List of (LINE-NUMBER . COMMIT-HASH) pairs.")

(defvar-local blame-reveal--blame-data-range nil
  "Range of currently loaded blame data: (START-LINE . END-LINE).
nil means entire file is loaded.")

(defvar-local blame-reveal--timestamps nil
  "Cached min/max timestamps for color calculation.")

(defvar-local blame-reveal--recent-commits nil
  "List of recent commit hashes (most recent first) to colorize.")


;;;; Loading State Variables

(defvar-local blame-reveal--last-window-start nil
  "Last window start position to detect scrolling.")

(defvar-local blame-reveal--loading nil
  "Flag to indicate if blame data is being loaded.")

(defvar-local blame-reveal--all-commits-loaded nil
  "Flag indicating if all commits info has been loaded.")


;;;; Recursive Blame Variables
;;;; (These variables support the optional blame-reveal-recursive.el extension)

(defvar-local blame-reveal--blame-stack nil
  "Stack for recursive blame history.
Each element is a plist with :revision, :line, :blame-data, :commit-info, etc.")

(defvar-local blame-reveal--current-revision nil
  "Current revision being blamed (nil = HEAD, 'uncommitted = working tree).")

(defvar-local blame-reveal--revision-display nil
  "Display string for current revision (for mode line).")


;;;; Customization Group

(defgroup blame-reveal nil
  "Show git blame in fringe with colors.

Quick start:
  M-x blame-reveal-mode

Common customizations:
  - `blame-reveal-color-scheme': Change color scheme
  - `blame-reveal-recent-commit-count': How many commits to highlight
  - `blame-reveal-display-layout': Header display layout
  - `blame-reveal-use-magit': Use magit for commit details
  - `blame-reveal-lazy-load-threshold': Lazy loading threshold

See all options: M-x customize-group RET blame-reveal"
  :group 'vc
  :prefix "blame-reveal-")


;;;; Display Customization

(defcustom blame-reveal-style 'left-fringe
  "Which fringe to use for blame indicators."
  :type '(choice (const left-fringe)
                 (const right-fringe))
  :group 'blame-reveal)

(defcustom blame-reveal-display-layout 'compact
  "Format for commit message header.
- `none': no header (only fringe indicators, use 's' to view details)
- `line': show only the commit message line
- `compact': show message + metadata (hash, author, date)
- `full': show message + metadata + description

When set to `none', no header overlay is created when cursor moves
between blocks, providing a cleaner visual experience. You can still
view commit details using `blame-reveal-show-commit-details' (s key)."
  :type '(choice (const :tag "No header (fringe only)" none)
                 (const :tag "Message line only" line)
                 (const :tag "Message + metadata" compact)
                 (const :tag "Message + metadata + description" full))
  :group 'blame-reveal)

(defcustom blame-reveal-display-style 'background
  "Style for commit message header.
- 'background: No background color
- 'box: Use box border
- 'inverse: Inverse video"
  :type '(choice (const background)
                 (const box)
                 (const inverse))
  :group 'blame-reveal)

;;;; Typography Customization

(defcustom blame-reveal-header-weight 'bold
  "Font weight for the commit message line in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Extra-bold" extra-bold))
  :group 'blame-reveal)

(defcustom blame-reveal-header-height 1.0
  "Font height for the commit message line in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-weight 'normal
  "Font weight for the metadata line in blame header (hash, author, date)."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-height 0.9
  "Font height for the metadata line in blame header (hash, author, date).
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-description-weight 'normal
  "Font weight for the description lines in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-description-height 0.9
  "Font height for the description lines in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)


;;;; Commit Selection Customization

(defcustom blame-reveal-recent-commit-count 20
  "Number of most recent unique commits to highlight with colors.
Only the N most recent commits in the file will be shown with age-based
gradient colors, AND they must be within `blame-reveal-recent-days-limit'.
Older commits will be shown in gray (or when cursor is on them).

For active projects, you may want to increase this (e.g., 30-50).
For inactive projects, a smaller number (e.g., 5-10) may be sufficient."
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-recent-days-limit 90
  "Maximum age in days for a commit to be considered 'recent'.
Even if a commit is in the top N most recent commits, it will only
be colorized if it's within this many days. Set to nil to disable
the time limit (only use commit count).

Common values:
- 30: Only show commits from last month
- 90: Show commits from last quarter (default)
- 180: Show commits from last half year
- nil: No time limit, only use commit count"
  :type '(choice (const :tag "No time limit" nil)
                 (integer :tag "Days"))
  :group 'blame-reveal)

(defcustom blame-reveal-auto-expand-recent t
  "Automatically expand recent commit count to include all commits within time limit.
When t, if there are more than N commits within the time limit,
all of them will be shown (ignoring the commit count limit).
When nil, strictly use the commit count limit."
  :type 'boolean
  :group 'blame-reveal)

(defcustom blame-reveal-recursive-ignore-time-limit t
  "Whether to ignore time limit in recursive blame mode.

When t (recommended):
  - Recursive blame shows top N commits at that revision
  - Time limit (=blame-reveal-recent-days-limit') is ignored
  - Useful for exploring old revisions (e.g., 3 years ago)

When nil:
  - Recursive blame respects time limit
  - May show all gray if revision is too old
  - Only useful if you want strict time filtering

Example:
  Viewing revision from 2021-05-20 (3 years ago)
  - With t: Shows top 20 commits at 2021-05-20 (colorized)
  - With nil: All commits >90 days from now (all gray)

Most users should keep this as t for meaningful historical exploration."
  :type 'boolean
  :group 'blame-reveal)


;;;; Uncommitted Changes Customization

(defcustom blame-reveal-uncommitted-label "Uncommitted changes"
  "Label to show for uncommitted changes."
  :type 'string
  :group 'blame-reveal)

(defcustom blame-reveal-uncommitted-color nil
  "Color for uncommitted changes header and fringe.

If nil, uses automatic color based on theme:
  - Dark theme: #d9a066 (muted orange)
  - Light theme: #e6b380 (light muted orange)

These colors are designed to have similar visual prominence to
committed changes while being clearly distinguishable.

Set to a color string to use a fixed color.

Note: If you find the fringe too prominent, consider setting
`blame-reveal-show-uncommitted-fringe' to nil and using diff-hl
instead for showing uncommitted changes."
  :type '(choice (const :tag "Auto (theme-based muted orange)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

(defcustom blame-reveal-show-uncommitted-fringe nil
  "Whether to show fringe indicators for uncommitted changes.

When nil (default and recommended):
  - Only show header when cursor is on uncommitted lines
  - No fringe indicators for uncommitted changes
  - Works well with diff-hl or git-gutter for showing changes

When t:
  - Show fringe indicators for uncommitted changes
  - Only recommended if you don't use diff-hl/git-gutter
  - May cause visual redundancy with those tools

Tip: Use this package with diff-hl-mode for the best experience:
  - blame-reveal shows git blame for committed changes
  - diff-hl shows uncommitted changes in fringe"
  :type 'boolean
  :group 'blame-reveal)


;;;; Color Customization

(defcustom blame-reveal-recent-commit-color nil
  "Color for recent commits (within top N and time limit).
If nil, uses automatic gradient based on commit rank.
Can be:
- nil: Auto gradient (continuous gradient based on rank)
- Color string: Fixed color like \"#6699cc\" for all recent commits
- Function: Takes timestamp, returns color for custom gradient logic"
  :type '(choice (const :tag "Auto (gradient based on rank)" nil)
                 (color :tag "Fixed color")
                 (function :tag "Function (timestamp -> color)"))
  :group 'blame-reveal)

(defcustom blame-reveal-old-commit-color nil
  "Color for old commits (not in top N or beyond time limit).
If nil, uses automatic gray color based on theme (dark theme: #4a4a4a, light theme: #d0d0d0).
Set to a color string like \"#888888\" to use a fixed color."
  :type '(choice (const :tag "Auto (theme-based gray)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

(defcustom blame-reveal-color-scheme
  '(:hue 210
         :dark-newest 0.70
         :dark-oldest 0.35
         :light-newest 0.45
         :light-oldest 0.75
         :saturation-min 0.25
         :saturation-max 0.60)
  "Color scheme for blame visualization.
All lightness values are between 0.0 and 1.0.

Dark theme: newest commits are brighter (higher lightness) to stand out
Light theme: newest commits are darker (lower lightness) to stand out

Example schemes:

  High contrast:
  '(:hue 210
    :dark-newest 0.75 :dark-oldest 0.30
    :light-newest 0.35 :light-oldest 0.85
    :saturation-min 0.35 :saturation-max 0.70)

  Green:
  '(:hue 120
    :dark-newest 0.70 :dark-oldest 0.35
    :light-newest 0.40 :light-oldest 0.75
    :saturation-min 0.25 :saturation-max 0.60)

  Purple:
  '(:hue 280
    :dark-newest 0.70 :dark-oldest 0.35
    :light-newest 0.45 :light-oldest 0.75
    :saturation-min 0.25 :saturation-max 0.60)

  Subtle:
  '(:hue 210
    :dark-newest 0.60 :dark-oldest 0.40
    :light-newest 0.55 :light-oldest 0.70
    :saturation-min 0.20 :saturation-max 0.45)"
  :type 'plist
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when blame-reveal-mode
                 (blame-reveal--recolor-and-render))))))
  :group 'blame-reveal)


;;;; Performance Customization

(defcustom blame-reveal-render-margin 50
  "Number of lines to render above and below visible window."
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-temp-overlay-delay 0.05
  "Delay in seconds before rendering temp overlays for old commits.
Lower values (e.g., 0.02) make overlays appear faster but may cause lag
when moving cursor quickly. Higher values (e.g., 0.1) reduce lag but
overlays appear with more delay."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-lazy-load-threshold 5000
  "File size threshold (in lines) for lazy loading.
Files larger than this will use lazy loading (only blame visible region).
Smaller files will load completely for better recursive blame experience.

Recommended values:
- 3000-5000: Good balance for most cases
- 1000-2000: If you have slow git repository
- 10000+: If you have fast SSD and want full loading for larger files"
  :type 'integer
  :group 'blame-reveal)


;;;; Integration Customization

(defcustom blame-reveal-use-magit 'auto
  "Whether to use magit for showing commit details.
- 'auto: Use magit if available, otherwise use built-in
- t: Always use magit (error if not available)
- nil: Always use built-in git commands"
  :type '(choice (const :tag "Auto (use magit if available)" auto)
                 (const :tag "Always use magit" t)
                 (const :tag "Always use built-in" nil))
  :group 'blame-reveal)


;;;; Git Blame Data Functions

(defun blame-reveal--get-blame-data ()
  "Get git blame data for current buffer (entire file).
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let ((file (buffer-file-name))
        (blame-data nil))
    (when (and file (vc-git-registered file))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "blame" "--porcelain" file))
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
            (let ((commit-hash (match-string 1))
                  (line-number (string-to-number (match-string 3))))
              (push (cons line-number commit-hash) blame-data))))))
    (nreverse blame-data)))

(defun blame-reveal--get-blame-data-range (start-line end-line)
  "Get git blame data for specific line range START-LINE to END-LINE.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let ((file (buffer-file-name))
        (blame-data nil))
    (when (and file (vc-git-registered file))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "blame"
                                   "--porcelain"
                                   "-L" (format "%d,%d" start-line end-line)
                                   file))
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
            (let ((commit-hash (match-string 1))
                  (line-number (string-to-number (match-string 3))))
              (push (cons line-number commit-hash) blame-data))))))
    (nreverse blame-data)))

(defun blame-reveal--get-commit-info (commit-hash)
  "Get commit info for COMMIT-HASH.
Returns (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP DESCRIPTION)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "show"
                               "--no-patch"
                               "--format=%h|%an|%ar|%s|%at"
                               commit-hash))
      (goto-char (point-min))
      (when (re-search-forward "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([0-9]+\\)" nil t)
        (let ((short-hash (match-string 1))
              (author (match-string 2))
              (date (match-string 3))
              (summary (match-string 4))
              (timestamp (string-to-number (match-string 5))))
          ;; Get description separately
          (erase-buffer)
          (if (zerop (call-process "git" nil t nil "show"
                                   "--no-patch"
                                   "--format=%b"
                                   commit-hash))
              (list short-hash author date summary timestamp (string-trim (buffer-string)))
            (list short-hash author date summary timestamp "")))))))

(defun blame-reveal--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  ;; Skip uncommitted changes - no need to fetch info
  (unless (or (blame-reveal--is-uncommitted-p commit-hash)
              (gethash commit-hash blame-reveal--commit-info))
    (let ((info (blame-reveal--get-commit-info commit-hash)))
      (when info
        (puthash commit-hash info blame-reveal--commit-info)
        ;; Update timestamp range
        (let ((timestamp (nth 4 info)))
          (when timestamp
            (unless blame-reveal--timestamps
              (setq blame-reveal--timestamps (cons timestamp timestamp)))
            (setcar blame-reveal--timestamps
                    (min (car blame-reveal--timestamps) timestamp))
            (setcdr blame-reveal--timestamps
                    (max (cdr blame-reveal--timestamps) timestamp))))))))


;;;; Lazy Loading Functions

(defun blame-reveal--should-lazy-load-p ()
  "Check if current file should use lazy loading based on size."
  (> (line-number-at-pos (point-max))
     blame-reveal-lazy-load-threshold))

(defun blame-reveal--ensure-range-loaded (start-line end-line)
  "Ensure blame data is loaded for range START-LINE to END-LINE.
If using lazy loading and range is not loaded, expand the data."
  (when blame-reveal--blame-data-range
    ;; Using lazy loading, check if we need to expand
    (let ((current-start (car blame-reveal--blame-data-range))
          (current-end (cdr blame-reveal--blame-data-range)))
      (when (or (< start-line current-start)
                (> end-line current-end))
        ;; Need to load more data
        (let ((new-start (min start-line current-start))
              (new-end (max end-line current-end)))
          (blame-reveal--expand-blame-data new-start new-end))))))

(defun blame-reveal--expand-blame-data (start-line end-line)
  "Expand blame data to include START-LINE to END-LINE."
  (message "Expanding blame data: lines %d-%d..." start-line end-line)
  (let* ((file (buffer-file-name))
         (new-data (blame-reveal--get-blame-data-range start-line end-line)))
    (when new-data
      ;; Merge new data with existing data
      (let ((existing-lines (make-hash-table :test 'equal)))
        ;; Mark existing lines
        (dolist (entry blame-reveal--blame-data)
          (puthash (car entry) t existing-lines))

        ;; Add new lines
        (dolist (entry new-data)
          (unless (gethash (car entry) existing-lines)
            (push entry blame-reveal--blame-data))))

      ;; Sort by line number
      (setq blame-reveal--blame-data
            (sort blame-reveal--blame-data
                  (lambda (a b) (< (car a) (car b)))))

      ;; Update range
      (setq blame-reveal--blame-data-range
            (cons start-line end-line))

      ;; Load commit info for newly visible commits
      (blame-reveal--ensure-visible-commits-loaded)

      (message "Blame data expanded to %d lines" (length blame-reveal--blame-data)))))


;;;; Color Calculation Functions

(defun blame-reveal--is-dark-theme-p ()
  "Check if current theme is dark based on default background."
  (let* ((bg (or (face-background 'default) "white"))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      ;; Calculate relative luminance
      (let ((luminance (+ (* 0.299 (nth 0 rgb))
                          (* 0.587 (nth 1 rgb))
                          (* 0.114 (nth 2 rgb)))))
        (< luminance 0.5)))))

(defun blame-reveal--ease-out-cubic (x)
  "Ease-out cubic function for more dramatic color transitions.
Makes newer commits stand out more.
X should be between 0.0 and 1.0."
  (let ((x1 (- 1.0 x)))
    (- 1.0 (* x1 x1 x1))))

(defun blame-reveal--hsl-to-hex (h s l)
  "Convert HSL to hex color.
H: 0-360, S: 0.0-1.0, L: 0.0-1.0"
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1)))))
         (m (- l (/ c 2.0)))
         (rgb (cond
               ((< h 60)  (list c x 0))
               ((< h 120) (list x c 0))
               ((< h 180) (list 0 c x))
               ((< h 240) (list 0 x c))
               ((< h 300) (list x 0 c))
               (t         (list c 0 x))))
         (r (round (* 255 (+ (nth 0 rgb) m))))
         (g (round (* 255 (+ (nth 1 rgb) m))))
         (b (round (* 255 (+ (nth 2 rgb) m)))))
    (format "#%02x%02x%02x" r g b)))

(defun blame-reveal--relative-color-by-rank (commit-hash is-dark)
  "Calculate color based on commit's rank in recent commits list.
Newest commit = most prominent, oldest in list = less prominent.
Returns nil if commit is not in recent list."
  (when-let ((rank (cl-position commit-hash blame-reveal--recent-commits
                                :test 'equal)))
    (let* ((total-recent (length blame-reveal--recent-commits))
           ;; rank: 0 = newest, (total-recent - 1) = oldest in list
           (age-ratio (if (= total-recent 1)
                          0.0
                        (/ (float rank) (- total-recent 1))))
           ;; Apply ease-out curve
           (eased-ratio (blame-reveal--ease-out-cubic age-ratio))
           ;; Get values from color scheme
           (hue (plist-get blame-reveal-color-scheme :hue))
           (sat-min (plist-get blame-reveal-color-scheme :saturation-min))
           (sat-max (plist-get blame-reveal-color-scheme :saturation-max))
           (saturation (+ sat-min (* (- sat-max sat-min) (- 1.0 eased-ratio))))
           ;; Lightness based on theme
           (lightness (if is-dark
                          ;; Dark: newer = brighter
                          (let ((newest (plist-get blame-reveal-color-scheme :dark-newest))
                                (oldest (plist-get blame-reveal-color-scheme :dark-oldest)))
                            (+ oldest (* (- newest oldest) (- 1.0 eased-ratio))))
                        ;; Light: newer = darker
                        (let ((newest (plist-get blame-reveal-color-scheme :light-newest))
                              (oldest (plist-get blame-reveal-color-scheme :light-oldest)))
                          (- oldest (* (- oldest newest) (- 1.0 eased-ratio)))))))
      (blame-reveal--hsl-to-hex hue saturation lightness))))

(defun blame-reveal--get-fallback-color ()
  "Get fallback color when color calculation fails.
Returns a neutral color based on current theme."
  (if (blame-reveal--is-dark-theme-p)
      "#6699cc"  ; Blue for dark theme
    "#7799bb"))

(defun blame-reveal--get-old-commit-color ()
  "Get color for old commits (not in recent list).
The color is derived from the color scheme to be:
- Dark theme: darker than all recent commits
- Light theme: lighter than all recent commits"
  (or blame-reveal-old-commit-color
      (let* ((is-dark (blame-reveal--is-dark-theme-p))
             (hue (plist-get blame-reveal-color-scheme :hue))
             ;; Use minimum saturation for old commits
             (saturation (plist-get blame-reveal-color-scheme :saturation-min))
             ;; Lightness: make it darker/lighter than the oldest recent commit
             (lightness (if is-dark
                            ;; Dark: make it darker than oldest recent
                            (let ((oldest (plist-get blame-reveal-color-scheme :dark-oldest)))
                              (* oldest 0.7))  ; 30% darker
                          ;; Light: make it lighter than oldest recent
                          (let ((oldest (plist-get blame-reveal-color-scheme :light-oldest)))
                            (+ oldest (* (- 1.0 oldest) 0.5))))))  ; 50% towards white
        (blame-reveal--hsl-to-hex hue saturation lightness))))

(defun blame-reveal--is-uncommitted-p (commit-hash)
  "Check if COMMIT-HASH represents uncommitted changes."
  (string-match-p "^0+$" commit-hash))

(defun blame-reveal--get-uncommitted-color ()
  "Get color for uncommitted changes.
Uses orange tones with similar visual prominence to old commit colors."
  (or blame-reveal-uncommitted-color
      (if (blame-reveal--is-dark-theme-p)
          "#d9a066"  ; Orange for dark theme (H:30, S:60%, L:63%)
        "#e6b380"))) ; Lighter orange for light theme (H:30, S:60%, L:70%)

(defun blame-reveal--timestamp-to-color (timestamp commit-hash)
  "Convert TIMESTAMP to color based on commit age.

For commits not in recent list (either not in top N or too old):
  Returns `blame-reveal-old-commit-color' or theme-based color.

For commits in recent list (in top N AND within time limit):
  If `blame-reveal-recent-commit-color' is:
    - Function: Calls it with timestamp
    - Color string: Uses that fixed color
    - nil: Uses gradient based on rank in recent list"
  (if (not blame-reveal--timestamps)
      (blame-reveal--get-fallback-color)
    (let ((is-dark (blame-reveal--is-dark-theme-p)))

      ;; Check if this is a recent commit (in top N AND within time limit)
      (if (not (blame-reveal--is-recent-commit-p commit-hash))
          ;; Old commit (not in recent list)
          (blame-reveal--get-old-commit-color)

        ;; Recent commit (in top N AND within time limit)
        (cond
         ;; Custom function
         ((functionp blame-reveal-recent-commit-color)
          (funcall blame-reveal-recent-commit-color timestamp))

         ;; Fixed color
         ((stringp blame-reveal-recent-commit-color)
          blame-reveal-recent-commit-color)

         ;; Auto gradient based on rank
         (t
          (or (blame-reveal--relative-color-by-rank commit-hash is-dark)
              (blame-reveal--get-fallback-color))))))))

(defun blame-reveal--get-commit-color (commit-hash)
  "Get color for COMMIT-HASH, calculating if necessary."
  ;; Use special color for uncommitted changes
  (if (blame-reveal--is-uncommitted-p commit-hash)
      (blame-reveal--get-uncommitted-color)
    (or (gethash commit-hash blame-reveal--color-map)
        (let* ((info (gethash commit-hash blame-reveal--commit-info))
               (timestamp (and info (nth 4 info)))
               (color (if timestamp
                          (blame-reveal--timestamp-to-color timestamp commit-hash)
                        (blame-reveal--get-fallback-color))))
          (puthash commit-hash color blame-reveal--color-map)
          color))))


;;;; Commit Selection Functions

(defun blame-reveal--is-recent-commit-p (commit-hash)
  "Check if COMMIT-HASH is one of the recent commits to colorize."
  (member commit-hash blame-reveal--recent-commits))

(defun blame-reveal--get-unique-commits ()
  "Get list of unique commit hashes in current file (in order of first appearance)."
  (when blame-reveal--blame-data
    (let ((commits nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (entry blame-reveal--blame-data)
        (let ((commit (cdr entry)))
          (unless (gethash commit seen)
            (puthash commit t seen)
            (push commit commits))))
      (nreverse commits))))

(defun blame-reveal--update-recent-commits ()
  "Update the list of recent commits based on currently loaded info.

For normal blame: respects both count and time limits based on `blame-reveal-auto-expand-recent'
For recursive blame: respects `blame-reveal-recursive-ignore-time-limit'"
  (when blame-reveal--commit-info
    (let* ((commit-timestamps nil)
           (current-time (float-time))
           ;; Check if we're in recursive blame mode by examining current revision
           (skip-time-filter (and (boundp 'blame-reveal--current-revision)
                                  blame-reveal--current-revision
                                  (not (eq blame-reveal--current-revision 'uncommitted))
                                  blame-reveal-recursive-ignore-time-limit)))

      ;; Collect all commits with loaded timestamps
      (maphash (lambda (commit info)
                 (when-let ((timestamp (nth 4 info)))
                   (push (cons commit timestamp) commit-timestamps)))
               blame-reveal--commit-info)

      ;; Sort by timestamp (newest first)
      (setq commit-timestamps
            (sort commit-timestamps
                  (lambda (a b) (> (cdr a) (cdr b)))))

      (let ((recent-commits nil))
        (if blame-reveal-auto-expand-recent
            ;; Auto-expand mode
            (if (and blame-reveal-recent-days-limit
                     (not skip-time-filter))
                (let ((age-limit-seconds (* blame-reveal-recent-days-limit 86400)))
                  (setq recent-commits
                        (cl-remove-if
                         (lambda (commit-ts)
                           (> (- current-time (cdr commit-ts))
                              age-limit-seconds))
                         commit-timestamps)))
              (setq recent-commits
                    (seq-take commit-timestamps
                              blame-reveal-recent-commit-count)))

          ;; Strict mode
          (setq recent-commits
                (seq-take commit-timestamps
                          blame-reveal-recent-commit-count))

          (when (and blame-reveal-recent-days-limit
                     (not skip-time-filter))
            (let ((age-limit-seconds (* blame-reveal-recent-days-limit 86400)))
              (setq recent-commits
                    (cl-remove-if
                     (lambda (commit-ts)
                       (> (- current-time (cdr commit-ts))
                          age-limit-seconds))
                     recent-commits)))))

        (setq blame-reveal--recent-commits
              (mapcar #'car recent-commits))))))


;;;; Sticky Header Support

(defun blame-reveal--clear-sticky-header ()
  "Clear sticky header overlay."
  (when blame-reveal--sticky-header-overlay
    (delete-overlay blame-reveal--sticky-header-overlay)
    (setq blame-reveal--sticky-header-overlay nil)))

(defun blame-reveal--get-header-line-count ()
  "Get the number of lines the header overlay occupies.
Based on current display layout setting."
  (pcase blame-reveal-display-layout
    ('line 2)      ; message line + fringe line
    ('compact 3)   ; message + metadata + fringe
    ('full 5)      ; message + metadata + description + fringe
    (_ 3)))

(defun blame-reveal--is-header-visible-p (block-start-line window-start-line)
  "Check if header at BLOCK-START-LINE is visible given WINDOW-START-LINE.
Returns t if ANY part of the header is visible in window."
  (let* ((header-lines (blame-reveal--get-header-line-count))
         (header-visible-start (- block-start-line header-lines)))
    ;; Header is visible if block-start-line (where header ends) is visible
    ;; This means if window starts before or at block-start-line, header is partially visible
    (<= window-start-line block-start-line)))

(defun blame-reveal--find-block-for-commit (commit-hash block-start-line)
  "Find block info for COMMIT-HASH starting at BLOCK-START-LINE.
Returns the block from blame-reveal--blame-data, or nil if not found."
  (catch 'found
    (dolist (block (blame-reveal--find-block-boundaries blame-reveal--blame-data))
      (when (and (= (nth 0 block) block-start-line)
                 (equal (nth 1 block) commit-hash))
        (throw 'found block)))
    nil))

(defun blame-reveal--should-show-sticky-header-p (commit-hash block-start-line current-line)
  "Determine if sticky header should be shown.
Arguments:
  COMMIT-HASH: Hash of the commit block
  BLOCK-START-LINE: Starting line of the block
  CURRENT-LINE: Current cursor line

Returns t if all conditions are met:
  1. Header is scrolled off-screen
  2. Cursor is within THIS SPECIFIC BLOCK (not just any block of same commit)"
  (when-let* ((block-info (blame-reveal--find-block-for-commit
                           commit-hash block-start-line))
              (block-length (nth 2 block-info))
              (block-end-line (+ block-start-line block-length -1))
              (window-start-line (line-number-at-pos (window-start))))

    (let* ((header-visible (blame-reveal--is-header-visible-p
                            block-start-line window-start-line))
           ;; CRITICAL: cursor must be in THIS specific block
           (in-this-block (and (>= current-line block-start-line)
                               (<= current-line block-end-line))))

      ;; Debug: uncomment to see detailed check
      ;; (message "  -> block: %d-%d, win-start: %d, header-visible: %s, in-block: %s, result: %s"
      ;;          block-start-line block-end-line window-start-line
      ;;          header-visible in-this-block
      ;;          (and (not header-visible) in-this-block))

      ;; Show if header is off-screen AND cursor is in THIS block
      (and (not header-visible) in-this-block))))

(defun blame-reveal--create-sticky-header-overlay (commit-hash)
  "Create sticky header overlay for COMMIT-HASH at window top.
Returns the created overlay."
  (save-excursion
    (goto-char (window-start))
    (let* ((pos (line-beginning-position))
           (is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
           (color (blame-reveal--get-commit-color commit-hash))
           (header-text (blame-reveal--format-header-text commit-hash))
           (overlay (make-overlay pos pos))
           (fringe-face (blame-reveal--ensure-fringe-face color))
           (hide-fringe (and is-uncommitted
                             (not blame-reveal-show-uncommitted-fringe))))

      ;; Use same styling as regular header
      (let* ((header-weight blame-reveal-header-weight)
             (header-height blame-reveal-header-height)
             (metadata-weight blame-reveal-metadata-weight)
             (metadata-height blame-reveal-metadata-height)

             (header-face (pcase blame-reveal-display-style
                            ('background (list :foreground color :weight header-weight
                                               :height header-height :background (face-background 'default)))
                            ('box (list :foreground color :weight header-weight :height header-height
                                        :box (list :line-width 1 :color color)
                                        :background (face-background 'default)))
                            ('inverse (list :background color :weight header-weight :height header-height))
                            (_ (list :foreground color :weight header-weight :height header-height
                                     :background (face-background 'default)))))

             (metadata-face (pcase blame-reveal-display-style
                              ('background (list :foreground color :weight metadata-weight
                                                 :height metadata-height :background (face-background 'default)))
                              ('box (list :foreground color :weight metadata-weight :height metadata-height
                                          :box (list :line-width 1 :color color)
                                          :background (face-background 'default)))
                              ('inverse (list :background color :weight metadata-weight :height metadata-height))
                              (_ (list :foreground color :weight metadata-weight :height metadata-height
                                       :background (face-background 'default)))))

             (header-lines (split-string header-text "\n"))
             (sticky-indicator (propertize " " 'face `(:foreground ,color))))

        (overlay-put overlay 'blame-reveal-sticky t)
        (overlay-put overlay 'before-string
                     (concat
                      ;; First line with fringe
                      (unless hide-fringe
                        (propertize "!" 'display
                                    (list blame-reveal-style
                                          'blame-reveal-full
                                          fringe-face)))
                      sticky-indicator
                      (propertize (car header-lines) 'face header-face)
                      "\n"
                      ;; Second line with fringe (if exists)
                      (when (cdr header-lines)
                        (concat
                         (unless hide-fringe
                           (propertize "!" 'display
                                       (list blame-reveal-style
                                             'blame-reveal-full
                                             fringe-face)))
                         sticky-indicator
                         (propertize (cadr header-lines) 'face metadata-face)
                         "\n"))
                      ;; Final fringe line
                      (unless hide-fringe
                        (propertize "!" 'display
                                    (list blame-reveal-style
                                          'blame-reveal-full
                                          fringe-face)))))
        overlay))))

(defun blame-reveal--update-sticky-header ()
  "Update sticky header display based on cursor position.
Shows sticky header when the block header is scrolled off-screen
and cursor is still within that block."
  (blame-reveal--clear-sticky-header)

  (when-let* ((current-block (blame-reveal--get-current-block))
              (commit-hash (car current-block))
              (block-start-line (cdr current-block))
              (current-line (line-number-at-pos)))

    ;; Debug: uncomment to diagnose issues
    ;; (message "Sticky check: commit=%s block-start=%d current=%d"
    ;;          (substring commit-hash 0 8) block-start-line current-line)

    (when (blame-reveal--should-show-sticky-header-p
           commit-hash block-start-line current-line)
      (setq blame-reveal--sticky-header-overlay
            (blame-reveal--create-sticky-header-overlay commit-hash)))))


;;;; Overlay Management Functions

(defun blame-reveal--find-block-boundaries (blame-data &optional start-line end-line)
  "Find boundaries of same-commit blocks in BLAME-DATA.
If START-LINE and END-LINE are provided, only return blocks in that range.
Returns list of (START-LINE COMMIT-HASH BLOCK-LENGTH)."
  (let ((blocks nil)
        (current-commit nil)
        (block-start nil)
        (block-length 0))
    (dolist (entry blame-data)
      (let ((line (car entry))
            (commit (cdr entry)))
        (if (equal commit current-commit)
            (setq block-length (1+ block-length))
          (when current-commit
            ;; Add block if no range specified or block overlaps with range
            (when (or (not start-line)
                      (and (>= (+ block-start block-length -1) start-line)
                           (<= block-start end-line)))
              (push (list block-start current-commit block-length) blocks)))
          (setq current-commit commit
                block-start line
                block-length 1))))
    (when current-commit
      (when (or (not start-line)
                (and (>= (+ block-start block-length -1) start-line)
                     (<= block-start end-line)))
        (push (list block-start current-commit block-length) blocks)))
    (nreverse blocks)))

(defun blame-reveal--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE)."
  (let* ((window-start (window-start))
         (window-end (window-end nil t))
         (start-line (max 1 (- (line-number-at-pos window-start)
                               blame-reveal-render-margin)))
         (end-line (+ (line-number-at-pos window-end)
                      blame-reveal-render-margin)))
    (cons start-line end-line)))

(defun blame-reveal--ensure-fringe-face (color)
  "Ensure fringe face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-face-%s" color))))
    (unless (facep face-name)
      (custom-declare-face face-name
                           `((t :background ,color :foreground ,color))
                           (format "Face for git blame color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (fringe-face (blame-reveal--ensure-fringe-face color)))
        (overlay-put overlay 'blame-reveal t)
        (overlay-put overlay 'blame-reveal-commit commit-hash)
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-style
                                       'blame-reveal-full
                                       fringe-face)))
        overlay))))

(defun blame-reveal--format-header-text (commit-hash)
  "Format header text for COMMIT-HASH based on `blame-reveal-display-layout'."
  ;; Check if this is an uncommitted change (all zeros)
  (if (blame-reveal--is-uncommitted-p commit-hash)
      (format "▸ %s" blame-reveal-uncommitted-label)
    (let ((info (gethash commit-hash blame-reveal--commit-info)))
      (if info
          (let ((short-hash (nth 0 info))
                (author (nth 1 info))
                (date (nth 2 info))
                (summary (nth 3 info))
                (description (nth 5 info)))
            (pcase blame-reveal-display-layout
              ('line
               ;; Only commit message
               (format "▸ %s" summary))
              ('compact
               ;; Commit message + metadata (2 lines)
               (format "▸ %s\n  %s · %s · %s"
                       summary short-hash author date))
              ('full
               ;; Commit message + metadata + description
               (let ((desc-trimmed (if description (string-trim description) "")))
                 (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                     (let ((desc-lines (split-string desc-trimmed "\n")))
                       (format "▸ %s\n  %s · %s · %s\n\n%s"
                               summary short-hash author date
                               (mapconcat (lambda (line) (concat "  " line))
                                          desc-lines
                                          "\n")))
                   ;; No description, fall back to compact format
                   (format "▸ %s\n  %s · %s · %s"
                           summary short-hash author date))))
              (_ (format "▸ %s" summary))))
        ;; Fallback for missing info
        (format "▸ Commit %s" (substring commit-hash 0 8))))))

(defun blame-reveal--create-header-overlay (line-number commit-hash color &optional no-fringe)
  "Create header line above LINE-NUMBER with fringe.
The overlay is inserted at the end of the previous line (or beginning of buffer
if LINE-NUMBER is 1) to avoid disrupting diff-hl indicators on LINE-NUMBER.
COLOR is the fringe color, which will also be used for header text foreground.
If NO-FRINGE is non-nil, don't show fringe indicators."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      ;; For line 1, use beginning of buffer
      ;; For other lines, use end of previous line
      (let ((pos (if (= line-number 1)
                     (point-min)
                   (progn
                     (forward-line -1)
                     (line-end-position)))))
        (let* ((overlay (make-overlay pos pos))
               (header-text (blame-reveal--format-header-text commit-hash))
               (fringe-face (blame-reveal--ensure-fringe-face color))
               ;; Get configured weights and heights
               (header-weight blame-reveal-header-weight)
               (header-height blame-reveal-header-height)
               (metadata-weight blame-reveal-metadata-weight)
               (metadata-height blame-reveal-metadata-height)
               (description-weight blame-reveal-description-weight)
               (description-height blame-reveal-description-height)
               ;; Header line style
               (header-face (pcase blame-reveal-display-style
                              ('background (list :foreground color :weight header-weight :height header-height))
                              ('box (list :foreground color :weight header-weight :height header-height
                                          :box (list :line-width 1 :color color)))
                              ('inverse (list :background color :weight header-weight :height header-height))
                              (_ (list :foreground color :weight header-weight :height header-height))))
               ;; Metadata line style
               (metadata-face (pcase blame-reveal-display-style
                                ('background (list :foreground color :weight metadata-weight :height metadata-height))
                                ('box (list :foreground color :weight metadata-weight :height metadata-height
                                            :box (list :line-width 1 :color color)))
                                ('inverse (list :background color :weight metadata-weight :height metadata-height))
                                (_ (list :foreground color :weight metadata-weight :height metadata-height))))
               ;; Description line style
               (description-face (pcase blame-reveal-display-style
                                   ('background (list :foreground color :weight description-weight :height description-height))
                                   ('box (list :foreground color :weight description-weight :height description-height
                                               :box (list :line-width 1 :color color)))
                                   ('inverse (list :background color :weight description-weight :height description-height))
                                   (_ (list :foreground color :weight description-weight :height description-height))))
               ;; Split header text into lines
               (header-lines (split-string header-text "\n"))
               ;; Need leading newline if inserting at end of line (except for line 1)
               (need-leading-newline (not (= line-number 1))))
          (overlay-put overlay 'blame-reveal t)
          (overlay-put overlay 'blame-reveal-commit commit-hash)
          (overlay-put overlay 'blame-reveal-header t)
          (overlay-put overlay 'before-string
                       (concat
                        ;; Leading newline to separate from previous line's content
                        (when need-leading-newline "\n")
                        ;; First line with optional fringe
                        (unless no-fringe
                          (propertize "!" 'display
                                      (list blame-reveal-style
                                            'blame-reveal-full
                                            fringe-face)))
                        (propertize (car header-lines) 'face header-face)
                        "\n"
                        ;; Additional lines
                        (when (cdr header-lines)
                          (let ((remaining-lines (cdr header-lines))
                                (result ""))
                            (dotimes (i (length remaining-lines))
                              (let* ((line (nth i remaining-lines))
                                     (line-face (if (and (eq blame-reveal-display-layout 'full)
                                                         (> i 0)
                                                         (not (string-empty-p (string-trim line))))
                                                    description-face
                                                  metadata-face)))
                                (setq result
                                      (concat result
                                              (unless no-fringe
                                                (propertize "!" 'display
                                                            (list blame-reveal-style
                                                                  'blame-reveal-full
                                                                  fringe-face)))
                                              (propertize line 'face line-face)
                                              "\n"))))
                            result))
                        ;; Final fringe line
                        (unless no-fringe
                          (propertize "!" 'display
                                      (list blame-reveal-style
                                            'blame-reveal-full
                                            fringe-face)))))
          overlay)))))

(defun blame-reveal--clear-overlays ()
  "Remove all blame-reveal overlays."
  (dolist (overlay blame-reveal--overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq blame-reveal--overlays nil)
  (blame-reveal--clear-temp-overlays)
  (when blame-reveal--header-overlay
    (delete-overlay blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil))
  (blame-reveal--clear-sticky-header)
  (setq blame-reveal--current-block-commit nil))

(defun blame-reveal--clear-temp-overlays ()
  "Clear temporary overlays for old commits."
  (dolist (overlay blame-reveal--temp-old-overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq blame-reveal--temp-old-overlays nil))


;;;; Rendering Functions

(defun blame-reveal--should-render-commit (commit-hash)
  "Check if a commit should be rendered in permanent layer.
Only recent commits (top N by recency among loaded commits) are permanently visible."
  (blame-reveal--is-recent-commit-p commit-hash))

(defun blame-reveal--render-block-fringe (block-start block-length commit-hash color)
  "Render fringe for a specific block.
Returns list of created overlays."
  (let ((overlays nil)
        (range (blame-reveal--get-visible-line-range))
        (block-end (+ block-start block-length -1)))
    (let ((render-start (max block-start (car range)))
          (render-end (min block-end (cdr range))))
      (when (<= render-start render-end)
        (dotimes (i (- render-end render-start -1))
          (let* ((line-num (+ render-start i))
                 (fringe-ov (blame-reveal--create-fringe-overlay
                             line-num color commit-hash)))
            (when fringe-ov
              (push fringe-ov overlays))))))
    overlays))

(defun blame-reveal--render-visible-region ()
  "Render git blame fringe for visible region only."
  (when blame-reveal--blame-data
    (blame-reveal--clear-overlays)

    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data
                    start-line
                    end-line)))

      ;; Ensure commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))

      ;; Update recent commits based on what's loaded so far
      (blame-reveal--update-recent-commits)

      ;; Render fringe for ALL recent commits in visible area
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))

          ;; Skip uncommitted changes unless explicitly enabled
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            ;; Render permanent fringe for recent commits
            (when (blame-reveal--should-render-commit commit-hash)
              (let* ((color (blame-reveal--get-commit-color commit-hash))
                     (ovs (blame-reveal--render-block-fringe
                           block-start block-length commit-hash color)))
                (setq blame-reveal--overlays
                      (append ovs blame-reveal--overlays)))))))

      ;; Re-trigger header update
      (blame-reveal--update-header))))

(defun blame-reveal--recolor-and-render ()
  "Recalculate colors and re-render (for theme changes)."
  (when blame-reveal--blame-data
    (setq blame-reveal--color-map (make-hash-table :test 'equal))
    (blame-reveal--render-visible-region)))


;;;; Loading Functions

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
  "Ensure commit info is loaded for all visible commits."
  (when-let ((visible-commits (blame-reveal--get-visible-commits)))
    (dolist (commit visible-commits)
      (blame-reveal--ensure-commit-info commit))
    (blame-reveal--update-recent-commits)))

(defun blame-reveal--load-commits-incrementally ()
  "Load commit info for visible area only (lazy loading)."
  (blame-reveal--ensure-visible-commits-loaded))

(defun blame-reveal--load-blame-data-impl (buffer)
  "Implementation of blame data loading for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((file (buffer-file-name))
             (use-lazy (blame-reveal--should-lazy-load-p))
             (blame-data nil))

        (if use-lazy
            ;; Large file: only load visible region
            (let* ((range (blame-reveal--get-visible-line-range))
                   (start-line (car range))
                   (end-line (cdr range)))
              (setq blame-data (blame-reveal--get-blame-data-range
                                start-line end-line))
              (setq blame-reveal--blame-data-range (cons start-line end-line))
              (when blame-data
                (message "Git blame loaded (lazy): %d lines (range %d-%d)"
                         (length blame-data) start-line end-line)))

          ;; Small file: load entire file
          (setq blame-data (blame-reveal--get-blame-data))
          (setq blame-reveal--blame-data-range nil)  ; nil = full load
          (when blame-data
            (message "Git blame loaded (full): %d lines" (length blame-data))))

        (if blame-data
            (progn
              (setq blame-reveal--blame-data blame-data)
              (setq blame-reveal--commit-info (make-hash-table :test 'equal))
              (setq blame-reveal--color-map (make-hash-table :test 'equal))
              (setq blame-reveal--timestamps nil)
              (setq blame-reveal--recent-commits nil)
              (setq blame-reveal--all-commits-loaded nil)
              (setq blame-reveal--loading nil)

              ;; Load commits incrementally
              (blame-reveal--load-commits-incrementally)

              ;; Initial render
              (blame-reveal--render-visible-region))

          (setq blame-reveal--loading nil)
          (message "No git blame data available"))))))

(defun blame-reveal--load-blame-data ()
  "Load git blame data.
Use lazy loading for large files, full loading for small files."
  (unless blame-reveal--loading
    (setq blame-reveal--loading t)
    (let ((use-lazy (blame-reveal--should-lazy-load-p)))
      (message "Loading git blame data%s..."
               (if use-lazy " (lazy mode)" "")))
    (run-with-idle-timer 0.01 nil
                         #'blame-reveal--load-blame-data-impl
                         (current-buffer))))

(defun blame-reveal--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  (setq blame-reveal--blame-data nil
        blame-reveal--blame-data-range nil
        blame-reveal--commit-info nil
        blame-reveal--color-map nil
        blame-reveal--timestamps nil
        blame-reveal--recent-commits nil
        blame-reveal--all-commits-loaded nil)
  (blame-reveal--load-blame-data))


;;;; Event Handlers

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line."
  (let ((line-num (line-number-at-pos))
        (overlays (overlays-at (point))))
    (catch 'found
      (dolist (ov overlays)
        (when-let ((commit (overlay-get ov 'blame-reveal-commit)))
          (dolist (block (blame-reveal--find-block-boundaries
                          blame-reveal--blame-data))
            (let ((block-start (nth 0 block))
                  (block-commit (nth 1 block))
                  (block-length (nth 2 block)))
              (when (and (equal commit block-commit)
                         (>= line-num block-start)
                         (< line-num (+ block-start block-length)))
                (throw 'found (cons commit block-start)))))))

      (dolist (block (blame-reveal--find-block-boundaries
                      blame-reveal--blame-data))
        (let ((block-start (nth 0 block))
              (block-commit (nth 1 block))
              (block-length (nth 2 block)))
          (when (and (>= line-num block-start)
                     (< line-num (+ block-start block-length)))
            (throw 'found (cons block-commit block-start)))))
      nil)))

(defun blame-reveal--temp-overlay-renderer (buf hash col)
  "Render temporary overlays for old commit HASH with color COL in buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (equal blame-reveal--current-block-commit hash)
        ;; Only find blocks in visible range
        (let* ((range (blame-reveal--get-visible-line-range))
               (start-line (car range))
               (end-line (cdr range))
               (visible-blocks
                (blame-reveal--find-block-boundaries
                 blame-reveal--blame-data
                 start-line
                 end-line)))
          ;; Render temp overlays for matching blocks in visible area
          (dolist (block visible-blocks)
            (let ((blk-start (nth 0 block))
                  (blk-commit (nth 1 block))
                  (blk-length (nth 2 block)))
              (when (equal blk-commit hash)
                (let ((temp-ovs (blame-reveal--render-block-fringe
                                 blk-start blk-length hash col)))
                  (setq blame-reveal--temp-old-overlays
                        (append temp-ovs
                                blame-reveal--temp-old-overlays)))))))))))

(defun blame-reveal--update-header ()
  "Update header display based on current cursor position."
  (when blame-reveal--blame-data
    (let ((current-block (blame-reveal--get-current-block)))
      (if (and current-block
               (not (equal (car current-block) blame-reveal--current-block-commit)))
          (let* ((commit-hash (car current-block))
                 (block-start (cdr current-block))
                 (_ (blame-reveal--ensure-commit-info commit-hash))
                 (is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
                 (is-old-commit (and (not is-uncommitted)
                                     (not (blame-reveal--should-render-commit commit-hash))))
                 (color (cond
                         (is-uncommitted
                          (blame-reveal--get-uncommitted-color))
                         (is-old-commit
                          (blame-reveal--get-old-commit-color))
                         (t
                          (blame-reveal--get-commit-color commit-hash))))
                 (hide-header-fringe (and is-uncommitted
                                          (not blame-reveal-show-uncommitted-fringe))))

            ;; Cancel any pending temp overlay rendering
            (when blame-reveal--temp-overlay-timer
              (cancel-timer blame-reveal--temp-overlay-timer)
              (setq blame-reveal--temp-overlay-timer nil))

            ;; Clear previous header immediately
            (when blame-reveal--header-overlay
              (delete-overlay blame-reveal--header-overlay)
              (setq blame-reveal--header-overlay nil))

            ;; Clear previous temp overlays immediately
            (blame-reveal--clear-temp-overlays)

            ;; Update current block commit
            (setq blame-reveal--current-block-commit commit-hash)

            ;; Create new header only if layout is not 'none
            (unless (eq blame-reveal-display-layout 'none)
              (setq blame-reveal--header-overlay
                    (blame-reveal--create-header-overlay
                     block-start commit-hash color hide-header-fringe)))

            ;; Show temp fringe overlay for old commits or uncommitted changes
            (when (or is-old-commit
                      (and is-uncommitted blame-reveal-show-uncommitted-fringe))
              (setq blame-reveal--temp-overlay-timer
                    (run-with-idle-timer
                     blame-reveal-temp-overlay-delay nil
                     #'blame-reveal--temp-overlay-renderer
                     (current-buffer) commit-hash color))))

        ;; No current block or moved away
        (unless current-block
          (when blame-reveal--temp-overlay-timer
            (cancel-timer blame-reveal--temp-overlay-timer)
            (setq blame-reveal--temp-overlay-timer nil))
          (when blame-reveal--header-overlay
            (delete-overlay blame-reveal--header-overlay)
            (setq blame-reveal--header-overlay nil))
          (blame-reveal--clear-temp-overlays)
          (setq blame-reveal--current-block-commit nil)))

      ;; Only update sticky header if layout is not 'none
      (unless (eq blame-reveal-display-layout 'none)
        (blame-reveal--update-sticky-header)))))

(defun blame-reveal--scroll-handler-impl (buf)
  "Implementation of scroll handler for buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when blame-reveal-mode
        ;; If using lazy loading, ensure scrolled area is loaded
        (when blame-reveal--blame-data-range
          (let* ((range (blame-reveal--get-visible-line-range))
                 (start-line (car range))
                 (end-line (cdr range)))
            (blame-reveal--ensure-range-loaded start-line end-line)))

        ;; Load commit info for visible commits
        (blame-reveal--ensure-visible-commits-loaded)

        ;; Render visible region
        (blame-reveal--render-visible-region)
        (blame-reveal--update-sticky-header)))))

(defun blame-reveal--on-scroll ()
  "Handle scroll event with debouncing."
  (let ((current-start (window-start)))
    (unless (equal current-start blame-reveal--last-window-start)
      (setq blame-reveal--last-window-start current-start)
      (when blame-reveal--scroll-timer
        (cancel-timer blame-reveal--scroll-timer))
      (setq blame-reveal--scroll-timer
            (run-with-idle-timer
             0.1 nil
             #'blame-reveal--scroll-handler-impl
             (current-buffer))))))

(defun blame-reveal--scroll-handler (_win _start)
  "Handle window scroll events."
  (blame-reveal--on-scroll))

(defun blame-reveal--theme-change-handler ()
  "Handle theme change for all blame-reveal buffers.
Recalculates colors and refreshes all displays."
  (let ((current-buf (current-buffer))
        (current-point (point)))
    ;; Re-render fringe overlays with new theme colors in all buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when blame-reveal-mode
          (blame-reveal--recolor-and-render))))

    ;; Refresh header in current buffer if it exists
    (when (buffer-live-p current-buf)
      (with-current-buffer current-buf
        (when (and blame-reveal-mode
                   blame-reveal--current-block-commit)
          (save-excursion
            (goto-char current-point)
            ;; Force header refresh by clearing and re-triggering
            (let ((old-commit blame-reveal--current-block-commit))
              (setq blame-reveal--current-block-commit nil)
              ;; Clear existing header
              (when blame-reveal--header-overlay
                (delete-overlay blame-reveal--header-overlay)
                (setq blame-reveal--header-overlay nil))
              ;; Clear temp overlays
              (blame-reveal--clear-temp-overlays)
              ;; Re-trigger header update
              (blame-reveal--update-header))))))))

(defun blame-reveal--on-theme-change (&rest _)
  "Handle theme change event and refresh all blame displays."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer))
  (setq blame-reveal--theme-change-timer
        (run-with-timer 0.3 nil #'blame-reveal--theme-change-handler)))

(defun blame-reveal--setup-theme-advice ()
  "Setup advice to monitor theme changes."
  (if (boundp 'after-enable-theme-hook)
      ;; Use official hook for Emacs 29+
      (add-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    ;; Use advice for older Emacs versions
    (advice-add 'load-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'enable-theme :after #'blame-reveal--on-theme-change)
    (advice-add 'disable-theme :after #'blame-reveal--on-theme-change)))

(defun blame-reveal--remove-theme-advice ()
  "Remove theme change advice."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer)
    (setq blame-reveal--theme-change-timer nil))

  (if (boundp 'after-enable-theme-hook)
      (remove-hook 'after-enable-theme-hook #'blame-reveal--on-theme-change)
    (advice-remove 'load-theme #'blame-reveal--on-theme-change)
    (advice-remove 'enable-theme #'blame-reveal--on-theme-change)
    (advice-remove 'disable-theme #'blame-reveal--on-theme-change)))


;;;; Mode Line Functions

(defun blame-reveal--mode-line-revision ()
  "Return mode line string showing current revision."
  (when blame-reveal--revision-display
    (format " @%s"
            (propertize blame-reveal--revision-display
                        'face 'font-lock-constant-face))))

(defun blame-reveal--setup-mode-line ()
  "Add revision info to mode line."
  (unless (member '(:eval (blame-reveal--mode-line-revision)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  '((:eval (blame-reveal--mode-line-revision)))))))

(defun blame-reveal--cleanup-mode-line ()
  "Remove revision info from mode line."
  (setq global-mode-string
        (delete '(:eval (blame-reveal--mode-line-revision)) global-mode-string)))


;;;; Magit Integration

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


;;;; Interactive Commands

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
  "Show full commit details including description in a separate buffer."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block))
            (info (gethash commit-hash blame-reveal--commit-info)))
      (let* ((short-hash (nth 0 info))
             (author (nth 1 info))
             (date (nth 2 info))
             (summary (nth 3 info))
             (description (nth 5 info))
             (desc-trimmed (if description (string-trim description) ""))
             (buffer-name "*Commit Details*")
             (buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)

            (insert (propertize (format "%s\n" summary)
                                'face '(:height 1.1 :weight bold)))

            (insert (propertize short-hash 'face '(:foreground "#6c9ef8" :weight bold)))
            (insert (propertize " • " 'face '(:foreground "#666")))
            (insert (propertize author 'face '(:foreground "#a0a0a0")))
            (insert (propertize " • " 'face '(:foreground "#666")))
            (insert (propertize date 'face '(:foreground "#a0a0a0")))
            (insert "\n")

            (insert (propertize (make-string 60 ?─) 'face '(:foreground "#444")))
            (insert "\n\n")

            (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                (let ((desc-lines (split-string desc-trimmed "\n")))
                  (dolist (line desc-lines)
                    (insert (propertize (format "%s\n" line)
                                        'face '(:foreground "#d0d0d0")))))
              (insert (propertize "(no description)"
                                  'face '(:foreground "#666" :slant italic))))

            (insert "\n")
            (goto-char (point-min)))
          (special-mode)
          (local-set-key (kbd "q") 'quit-window))
        (pop-to-buffer buffer))
    (message "No commit info at current line")))

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


;;;; Minor Mode Definition

;;;###autoload
(define-minor-mode blame-reveal-mode
  "Toggle git blame fringe display."
  :lighter " BlameReveal"
  :group 'blame-reveal
  (if blame-reveal-mode
      (progn
        (let ((file (buffer-file-name)))
          (if (not (and file (vc-git-registered file)))
              (progn
                (message "Cannot enable blame-reveal-mode: not a git-tracked file")
                (setq blame-reveal-mode nil))

            (setq blame-reveal--emulation-alist
                  `((blame-reveal-mode . ,blame-reveal-mode-map)))
            (add-to-list 'emulation-mode-map-alists
                         'blame-reveal--emulation-alist)

            ;; Initialize state
            (setq blame-reveal--blame-data-range nil)
            (setq blame-reveal--all-commits-loaded nil)

            (blame-reveal--load-blame-data)
            (add-hook 'after-save-hook #'blame-reveal--full-update nil t)
            (add-hook 'window-scroll-functions #'blame-reveal--scroll-handler nil t)
            (add-hook 'post-command-hook #'blame-reveal--update-header nil t)
            (add-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region nil t)

            (blame-reveal--setup-mode-line)
            (setq blame-reveal--blame-stack nil)
            (setq blame-reveal--current-revision nil)
            (setq blame-reveal--revision-display nil)

            (blame-reveal--setup-theme-advice))))

    ;; Cleanup
    (setq emulation-mode-map-alists
          (delq 'blame-reveal--emulation-alist
                emulation-mode-map-alists))

    (when blame-reveal--temp-overlay-timer
      (cancel-timer blame-reveal--temp-overlay-timer)
      (setq blame-reveal--temp-overlay-timer nil))

    (blame-reveal--clear-overlays)
    (remove-hook 'after-save-hook #'blame-reveal--full-update t)
    (remove-hook 'window-scroll-functions #'blame-reveal--scroll-handler t)
    (remove-hook 'post-command-hook #'blame-reveal--update-header t)
    (remove-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region t)
    (when blame-reveal--scroll-timer
      (cancel-timer blame-reveal--scroll-timer)
      (setq blame-reveal--scroll-timer nil))

    (setq blame-reveal--loading nil)
    (setq blame-reveal--blame-data-range nil)
    (setq blame-reveal--all-commits-loaded nil)

    (unless (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (and (not (eq buf (current-buffer)))
                              blame-reveal-mode)))
                     (buffer-list))

      (blame-reveal--cleanup-mode-line)
      (setq blame-reveal--blame-stack nil)
      (setq blame-reveal--current-revision nil)
      (setq blame-reveal--revision-display nil)

      (blame-reveal--remove-theme-advice))))

(provide 'blame-reveal)
;;; blame-reveal.el ends here
