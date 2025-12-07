;;; blame-reveal.el --- Git blame visualization in fringe -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lucius Chen
;; Author: Lucius Chen
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience
;; URL: https://github.com/lucius-chen/blame-reveal

;;; Commentary:
;; Display git blame information in the fringe with adaptive color gradients.
;; Color intensity based on commit recency within an intelligent time window.
;; Show commit info above each block with sticky headers.
;; Performance optimized: lazy loading, smart caching, and viewport rendering.
;;
;; Key Features:
;; - Smart time-based commit selection with auto-calculation
;; - Gradient quality control (strict/auto/relaxed)
;; - Lazy loading for large files (configurable threshold)
;; - Incremental commit info loading with caching
;; - Recursive blame navigation with historical context
;; - Move/copy detection with progressive disclosure
;; - Sticky headers for long commits
;; - Theme-aware color schemes with HSL customization
;; - Customizable display layouts (line/compact/full/none)
;;
;; Quick Start:
;;   M-x blame-reveal-mode
;;
;; Common Customizations:
;;   (setq blame-reveal-recent-days-limit 'auto)      ; Smart time window
;;   (setq blame-reveal-gradient-quality 'auto)       ; Balanced quality
;;   (setq blame-reveal-display-layout 'compact)      ; Header format
;;   (setq blame-reveal-color-scheme '(:hue 210 ...)) ; Color theme
;;   (setq blame-reveal-move-copy-mode 'prompt)       ; Ask at boundaries
;;
;; See full documentation: https://github.com/lucius-chen/blame-reveal

;;; Code:

(require 'vc-git)
(require 'cl-lib)

;; Load all modules
(require 'blame-reveal-core)
(require 'blame-reveal-state)
(require 'blame-reveal-git)
(require 'blame-reveal-overlay)
(require 'blame-reveal-color)
(require 'blame-reveal-header)
(require 'blame-reveal-ui)
(require 'blame-reveal-commands)

;;; Fringe Bitmap Definitions

(define-fringe-bitmap 'blame-reveal-full
  [#b11111111 #b11111111 #b11111111 #b11111111
              #b11111111 #b11111111 #b11111111 #b11111111
              #b11111111 #b11111111 #b11111111 #b11111111
              #b11111111 #b11111111 #b11111111 #b11111111]
  20 8 'center)

(define-fringe-bitmap 'blame-reveal-loading-bright
  [#b11111111 #b11111111 #b11111111 #b01111110
              #b01111110 #b00111100 #b00011000 #b00000000]
  8 8 'center)

(define-fringe-bitmap 'blame-reveal-loading-dim
  [#b11111111 #b11111111 #b11111111 #b01111110
              #b01111110 #b00111100 #b00011000 #b00000000]
  8 8 'center)

;;; Keymaps

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
    (define-key map (kbd "m") #'blame-reveal-menu)
    map)
  "Keymap for blame-reveal-mode.")

(defvar blame-reveal--emulation-alist nil
  "Emulation mode map alist for blame-reveal.")

;;; Timer Variables

(defvar blame-reveal--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar blame-reveal--scroll-timer nil
  "Timer to debounce scroll updates.")

(defvar blame-reveal--scroll-render-delay 0.3
  "Internal delay before rendering overlays after scrolling stops.
This is an implementation detail and should not be customized by users.")

(defvar blame-reveal-loading-animation-speed 0.08
  "Loading animation update interval in seconds.")

(defvar blame-reveal-loading-animation-frames-per-step 4
  "Number of frames before advancing to next step.")

(defvar blame-reveal--global-loading-animation-timer nil
  "Global timer for loading animation across all buffers.")

;;; Customization Group

(defgroup blame-reveal nil
  "Show git blame in fringe with colors.

Quick start:
  M-x blame-reveal-mode

Common customizations:
  - `blame-reveal-color-scheme': Change color scheme
  - `blame-reveal-recent-days-limit': How many days of commits to highlight
  - `blame-reveal-header-format-function': Customize header display
  - `blame-reveal-use-magit': Use magit for commit details
  - `blame-reveal-lazy-load-threshold': Lazy loading threshold

See all options: M-x customize-group RET blame-reveal"
  :group 'vc
  :prefix "blame-reveal-")

;;; Header Customization

(defcustom blame-reveal-header-format-function
  #'blame-reveal-format-header-default
  "Function to format commit information for block-style header display.

The function receives three arguments:
  COMMIT-HASH - Full commit hash string (40 chars)
  COMMIT-INFO - Tuple: (short-hash author date summary timestamp description)
  COLOR       - Hex color string for this commit (e.g., \"#6699cc\")

Returns a =blame-reveal-commit-display' struct with:
  :lines - List of strings (multiple lines allowed for block style)
  :faces - List of face specs (one per line, can reference COLOR)
  :color - Color string for fringe indicators

This format is ONLY used for block-style headers.
Inline and margin styles have their own format functions.

Example - Minimal single-line format:
  (setq blame-reveal-header-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,hash ,_author ,_date ,msg ,_ ,_) info))
        (make-blame-reveal-commit-display
         :lines (list (format \"%s: %s\" hash msg))
         :faces (list =(:foreground ,color :weight bold))
         :color color))))

Example - Multi-line format with emphasis:
  (setq blame-reveal-header-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,hash ,author ,date ,msg ,_ ,_) info))
        (make-blame-reveal-commit-display
         :lines (list
                 (format \"■ %s\" msg)
                 (format \"  %s • %s • %s\" author date hash))
         :faces (list
                 =(:foreground ,color :weight bold :height 1.1)
                 =(:foreground ,color :height 0.9))
         :color color))))"
  :type 'function
  :group 'blame-reveal)

(defcustom blame-reveal-inline-format-function
  #'blame-reveal-format-inline-default
  "Function to format commit information for inline-style header.
If nil, uses default compact format.

The function receives the same three arguments as block format.

Returns a =blame-reveal-commit-display' struct with:
  :lines - List containing EXACTLY ONE string (enforced at runtime)
  :faces - List containing exactly one face spec
  :color - Color string for fringe indicators

IMPORTANT: Inline headers appear after the first line of code.
Keep the format compact and single-line.

Example - Compact format with hash and message:
  (setq blame-reveal-inline-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,hash ,author ,_date ,msg ,_ ,_) info))
        (make-blame-reveal-commit-display
         :lines (list (format \"[%s] %s - %s\"
                       (substring hash 0 5)
                       (blame-reveal--abbreviate-author author)
                       (substring msg 0 (min 40 (length msg)))))
         :faces (list =(:foreground ,color :height 0.95))
         :color color))))

Example - Author and time only:
  (setq blame-reveal-inline-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,_hash ,author ,date ,_msg ,_ ,_) info))
        (make-blame-reveal-commit-display
         :lines (list (format \"%s · %s\"
                       (blame-reveal--abbreviate-author author)
                       (blame-reveal--shorten-time date)))
         :faces (list =(:foreground ,color))
         :color color))))"
  :type 'function
  :group 'blame-reveal)

(defcustom blame-reveal-margin-format-function
  #'blame-reveal-format-margin-default
  "Function to format commit information for margin-style header.
If nil, uses default compact format (Author · Date).

The function receives the same three arguments as block format.

Returns a =blame-reveal-commit-display' struct with:
  :lines - List containing EXACTLY ONE string (enforced at runtime)
  :faces - List containing exactly one face spec
  :color - Color string for fringe indicators

IMPORTANT: Margin headers appear in the window margin.
Both left and right margins use the same format.
Keep format very compact (typically 15-25 characters).

Example - Hash and time only:
  (setq blame-reveal-margin-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,hash ,_author ,date ,_ ,_ ,_) info))
        (make-blame-reveal-commit-display
         :lines (list (format \"%s %s\"
                       (substring hash 0 4)
                       (blame-reveal--shorten-time date)))
         :faces (list =(:foreground ,color :height 0.9))
         :color color))))

Example - Author name only:
  (setq blame-reveal-margin-format-function
    (lambda (commit-hash info color)
      (pcase-let ((=(,_hash ,author ,_date ,_ ,_ ,_) info))
        (let ((short-name (blame-reveal--abbreviate-author author)))
          (make-blame-reveal-commit-display
           :lines (list (substring short-name 0 (min 12 (length short-name))))
           :faces (list =(:foreground ,color :weight bold :height 0.9))
           :color color)))))"
  :type 'function
  :group 'blame-reveal)

;;; Display Customization

(defcustom blame-reveal-fringe-side 'left-fringe
  "Which fringe to display blame indicators.
This is the core visual element showing commit age with colors."
  :type '(choice (const :tag "Left fringe" left-fringe)
                 (const :tag "Right fringe" right-fringe))
  :group 'blame-reveal)

(defcustom blame-reveal-header-style 'block
  "How to display commit information header.

Styles:
- `block': Show header as a separate block above the code
- `inline': Show header inline after the first line of code
- `margin': Show compact header in window margin

The header provides detailed commit info (author, date, message)
while the fringe shows visual age indicators."
  :type '(choice (const :tag "Block above code" block)
                 (const :tag "Inline after first line" inline)
                 (const :tag "In margin" margin))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and (boundp 'blame-reveal-mode) blame-reveal-mode)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when blame-reveal-mode
                 ;; Restore margins when switching away from margin style
                 (when (and (not (eq value 'margin))
                            (blame-reveal--is-margin-mode-p))
                   (blame-reveal--restore-window-margins))
                 ;; Setup margins when switching to margin style
                 (when (and (eq value 'margin)
                            (not (blame-reveal--is-margin-mode-p)))
                   (blame-reveal--ensure-window-margins))
                 ;; Refresh display
                 (when blame-reveal--header-overlay
                   (delete-overlay blame-reveal--header-overlay)
                   (setq blame-reveal--header-overlay nil))
                 (blame-reveal--update-header-impl))))))
  :group 'blame-reveal)

(defcustom blame-reveal-margin-side 'left
  "Which margin to use when =blame-reveal-header-style' is =margin'.

Left margin:
  - More visible but takes space from code area
  - Uses compact format by default (Author · Date)
  - Recommended width: 15-25 characters

Right margin:
  - Less intrusive, doesn't affect code indentation
  - Uses compressed inline format by default (full commit info)
  - Recommended width: 25-40 characters

Only takes effect when header style is set to margin."
  :type '(choice (const :tag "Left margin (compact format)" left)
                 (const :tag "Right margin (full format)" right))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and (boundp 'blame-reveal-mode) blame-reveal-mode
                    (eq blame-reveal-header-style 'margin))
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when blame-reveal-mode
                 ;; Restore old margin and setup new one
                 (blame-reveal--restore-window-margins)
                 (blame-reveal--ensure-window-margins)
                 ;; Refresh display
                 (when blame-reveal--header-overlay
                   (delete-overlay blame-reveal--header-overlay)
                   (setq blame-reveal--header-overlay nil))
                 (blame-reveal--update-header-impl))))))
  :group 'blame-reveal)

;;; Commit Selection Customization

(defcustom blame-reveal-recent-days-limit 'auto
  "Time limit for highlighting recent commits.

Using days (instead of commit count) ensures consistent behavior
across projects with different commit frequencies.

Values:
  'auto  - Automatically calculate based on commit density and
           gradient quality. Adapts to both normal and recursive
           blame modes. (Recommended)

  number - Fixed days (e.g., 30, 90, 180, 365)
           Shows commits within this many days from reference point.
           In recursive blame, reference point is the revision date.

  nil    - No limit, highlight all commits with relative coloring
           (like IntelliJ IDEA)

Examples:
  'auto - Smart calculation, good for most cases
  30    - Last month (for very active files)
  90    - Last quarter
  180   - Last half year
  365   - Last year
  nil   - All commits (relative age coloring)

The actual number of commits within this period varies by file activity:
- Active files: many commits in the time window
- Quiet files: few commits in the time window

This is intentional and provides consistent time-based context
regardless of commit frequency."
  :type '(choice (const :tag "Auto (smart calculation)" auto)
                 (integer :tag "Fixed days")
                 (const :tag "No limit (all commits)" nil))
  :group 'blame-reveal)

(defcustom blame-reveal-gradient-quality 'auto
  "Control trade-off between time coverage and color distinction.

This setting affects how 'auto mode calculates the days limit.
It controls the target number of commits and minimum color difference.

'strict  - Fewer commits (5-10), excellent distinction (3-5% color steps)
           Best for visual clarity, shorter time window.
           Recommended for files with many commits.

'auto    - Balanced (10-20 commits), good distinction (2-3% color steps)
           Good balance between clarity and historical context.
           Recommended for most cases.

'relaxed - More commits (15-30), acceptable distinction (1.5-2% color steps)
           More historical context, colors may be subtle.
           Recommended for files with few commits.

Only used when `blame-reveal-recent-days-limit' is 'auto.
For fixed days limit, this setting is ignored.

Color distinction is measured by the lightness/saturation step between
consecutive commits in the gradient. Smaller steps mean colors are
harder to distinguish visually."
  :type '(choice (const :tag "Strict (best distinction)" strict)
                 (const :tag "Auto (balanced)" auto)
                 (const :tag "Relaxed (more history)" relaxed))
  :group 'blame-reveal)

;;; Uncommitted Changes Customization

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

;;; Color Customization

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

;;; Performance Customization

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

(defcustom blame-reveal-lazy-load-threshold 3000
  "File size threshold (in lines) for lazy loading.
Files larger than this will use lazy loading (only blame visible region).
Smaller files will load completely for better recursive blame experience.

Recommended values:
- 3000-5000: Good balance for most cases
- 1000-2000: If you have slow git repository
- 10000+: If you have fast SSD and want full loading for larger files"
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-async-blame 'auto
  "Whether to use async loading for git blame data.

When enabled, git blame commands run in background processes,
keeping Emacs responsive during loading and scrolling.

Values:
  'auto - Use async for large files (> `blame-reveal-lazy-load-threshold' lines),
          sync for small files (recommended)
  t     - Always use async loading
  nil   - Always use synchronous loading (may block UI for large files)

Async loading provides better UX for large files but requires Emacs 25.1+.
For small files, sync loading is actually faster due to less overhead."
  :type '(choice (const :tag "Auto (async for large files)" auto)
                 (const :tag "Always async" t)
                 (const :tag "Always sync" nil))
  :group 'blame-reveal)

;;; Integration Customization

(defcustom blame-reveal-use-magit 'auto
  "Whether to use magit for showing commit details.
- 'auto: Use magit if available, otherwise use built-in
- t: Always use magit (error if not available)
- nil: Always use built-in git commands"
  :type '(choice (const :tag "Auto (use magit if available)" auto)
                 (const :tag "Always use magit" t)
                 (const :tag "Always use built-in" nil))
  :group 'blame-reveal)

;;; Interface Functions

(defun blame-reveal--force-update-header ()
  "Force immediate header update without idle-timer delay.
Used when settings change via transient menu."
  (when blame-reveal--blame-data
    ;; Cancel any pending idle timer
    (when blame-reveal--header-update-timer
      (cancel-timer blame-reveal--header-update-timer)
      (setq blame-reveal--header-update-timer nil))
    ;; Update immediately (bypass idle delay)
    (blame-reveal--update-header-impl)))

;;; Text-Scale Support

(defun blame-reveal--get-line-height ()
  "Get current line height in pixels."
  (let ((line-height (line-pixel-height)))
    (if (> line-height 0)
        line-height
      (ceiling (frame-char-height)))))

(defun blame-reveal--update-fringe-bitmap ()
  "Update fringe bitmap to match current line height."
  (let* ((line-height (blame-reveal--get-line-height))
         (rows (max 8 (min 64 line-height)))
         (bitmap-rows (make-list rows #b11111111)))
    (define-fringe-bitmap 'blame-reveal-full
      (vconcat bitmap-rows)
      nil 8 'center)))

(defun blame-reveal--on-text-scale-change ()
  "Handle text-scale changes: update bitmap and re-render."
  (when blame-reveal-mode
    (blame-reveal--update-fringe-bitmap)
    (blame-reveal--recolor-and-render)))

;;; Mode Line Functions

(defun blame-reveal--mode-line-revision ()
  "Return mode line string showing current revision and M/C status."
  (when (or blame-reveal--revision-display
            blame-reveal--detect-moves)
    (let* ((rev-part (when blame-reveal--revision-display
                       (propertize (format " @%s" blame-reveal--revision-display)
                                   'face 'font-lock-constant-face)))
           (mc-part (when blame-reveal--detect-moves
                      (propertize (if rev-part " [M/C]" " [M/C enabled]")
                                  'face 'warning))))
      (concat rev-part mc-part))))

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

;;; Minor Mode Definition

(defun blame-reveal--can-enable-mode-p ()
  "Check if blame-reveal-mode can be enabled in current buffer.
Returns t if valid, otherwise prints message and returns nil."
  (let ((file (buffer-file-name)))
    (cond
     ((null file)
      (message "Blame-reveal: Buffer is not visiting a file")
      nil)
     ((not (vc-git-registered file))
      (message "Blame-reveal: File is not tracked by git")
      nil)
     (t t))))

(defun blame-reveal--protect-during-transient-setup (orig-fun &rest args)
  "Prevent header deletion during transient-setup.
Sets a buffer-local flag during transient-setup to prevent
blame-reveal--update-header from interfering."
  (let ((current-buf (current-buffer)))
    ;; Set protection flag before transient-setup
    (when (and (buffer-live-p current-buf)
               (buffer-local-value 'blame-reveal-mode current-buf))
      (with-current-buffer current-buf
        (setq blame-reveal--in-transient-setup t)))

    ;; Execute transient-setup
    (unwind-protect
        (apply orig-fun args)
      ;; Clear protection flag after transient-setup completes
      ;; Use a small delay to ensure transient is fully initialized
      (run-with-timer
       0.05 nil
       (lambda ()
         (when (buffer-live-p current-buf)
           (with-current-buffer current-buf
             (setq blame-reveal--in-transient-setup nil)
             ;; Optionally trigger a header update to refresh
             (when blame-reveal-mode
               (blame-reveal--update-header)))))))))

(defun blame-reveal--setup-buffer-resources ()
  "Initialize all resources, hooks, and state for the current buffer."
  ;; Keymap & Emulation
  (setq blame-reveal--emulation-alist
        `((blame-reveal-mode . ,blame-reveal-mode-map)))
  (add-to-list 'emulation-mode-map-alists 'blame-reveal--emulation-alist)
  ;; Subsystems Init
  (blame-reveal--init-overlay-registry)
  (blame-reveal--init-color-strategy)
  ;; State & Cache Reset
  (setq blame-reveal--auto-days-cache nil
        blame-reveal--blame-data-range nil
        blame-reveal--all-commits-loaded nil)
  ;; Initializing hash table
  (unless (hash-table-p blame-reveal--move-copy-metadata)
    (setq blame-reveal--move-copy-metadata (make-hash-table :test 'equal)))
  ;; UI Setup
  (blame-reveal--setup-mode-line)
  (blame-reveal--setup-theme-advice)
  ;; Text-scale support
  (add-hook 'text-scale-mode-hook #'blame-reveal--on-text-scale-change nil t)
  (blame-reveal--update-fringe-bitmap)
  ;; Hooks
  (add-hook 'after-save-hook #'blame-reveal--full-update nil t)
  (add-hook 'window-scroll-functions #'blame-reveal--scroll-handler nil t)
  (add-hook 'post-command-hook #'blame-reveal--update-header nil t)
  (add-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region nil t)
  ;; Transient Protection: prevent header deletion during transient-setup
  (advice-add 'transient-setup :around #'blame-reveal--protect-during-transient-setup)
  ;; Trigger Loading
  (blame-reveal--load-blame-data))

(defun blame-reveal--cleanup-buffer-resources ()
  "Completely tear down all blame-reveal resources for the current buffer.
Handles Hooks, Timers, Overlays, and State."
  ;; Remove Hooks (First, stop reacting to events)
  (remove-hook 'after-save-hook #'blame-reveal--full-update t)
  (remove-hook 'window-scroll-functions #'blame-reveal--scroll-handler t)
  (remove-hook 'post-command-hook #'blame-reveal--update-header t)
  (remove-hook 'window-configuration-change-hook #'blame-reveal--render-visible-region t)

  ;; Cancel All Timers (Using the helper from state or defining locally)
  (blame-reveal--cleanup-operation-ui-artifacts)

  ;; Cancel State Machine (Stops async processes)
  (blame-reveal--state-cancel "mode disabled")
  ;; Clean UI / Overlays
  (blame-reveal--restore-window-margins)
  (blame-reveal--clear-all-overlays) ; The unified registry clearer
  ;; Legacy/Specific UI cleanup
  (when (bound-and-true-p blame-reveal--header-overlay)
    (delete-overlay blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil))
  (blame-reveal--clear-sticky-header)
  ;; Reset Cache & Variables
  (setq blame-reveal--auto-days-cache nil
        blame-reveal--blame-data-range nil
        blame-reveal--all-commits-loaded nil
        blame-reveal--detect-moves nil
        blame-reveal--current-line-cache nil
        blame-reveal--move-copy-metadata nil
        blame-reveal--blame-stack nil
        blame-reveal--current-revision nil
        blame-reveal--revision-display nil)
  ;; Advice Cleanup
  (blame-reveal--remove-theme-advice)
  (advice-remove 'transient-setup #'blame-reveal--protect-during-transient-setup)
  (remove-hook 'text-scale-mode-hook #'blame-reveal--on-text-scale-change t))

;;;###autoload
(define-minor-mode blame-reveal-mode
  "Toggle git blame fringe display."
  :lighter " BlameReveal"
  :group 'blame-reveal
  (if blame-reveal-mode
      ;; --- Enable Phase ---
      (if (blame-reveal--can-enable-mode-p)
          (progn
            (blame-reveal--setup-buffer-resources)
            (run-hooks 'blame-reveal-mode-on-hook))

        ;; If the check fails, forcibly disable the mode
        (setq blame-reveal-mode nil))

    ;; --- Disable Phase ---
    (blame-reveal--cleanup-buffer-resources)

    ;; Global cleanup (if no other buffers are using the mode)
    (unless (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (and (not (eq buf (current-buffer)))
                              blame-reveal-mode)))
                     (buffer-list))
      (blame-reveal--cleanup-mode-line))

    (run-hooks 'blame-reveal-mode-off-hook)))

;;; Global Mode

;;;###autoload
(define-minor-mode blame-reveal-global-mode
  "Toggle automatic blame-reveal for all git-tracked files.

When enabled, blame-reveal-mode will be automatically activated
for any git-tracked file you open. This is convenient when working
on a project and you want blame information always available.

When disabled, you need to manually enable blame-reveal-mode for
each file."
  :global t
  :group 'blame-reveal
  (if blame-reveal-global-mode
      (progn
        (add-hook 'find-file-hook #'blame-reveal--auto-enable)
        ;; Enable for already opened buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (blame-reveal--auto-enable)))
        (message "Blame-reveal global mode enabled"))
    (remove-hook 'find-file-hook #'blame-reveal--auto-enable)
    (message "Blame-reveal global mode disabled")))

(provide 'blame-reveal)
;;; blame-reveal.el ends here
