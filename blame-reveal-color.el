;;; blame-reveal-color.el --- Color strategy system -*- lexical-binding: t; -*-

;;; Commentary:
;; Color calculation, gradient strategies, and theme adaptation.
;; Provides pluggable color strategy system with quality assessment.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'blame-reveal-core)

;;; Color Strategy Protocol

(cl-defstruct (blame-reveal-color-strategy
               (:constructor nil))
  name description)

(cl-defgeneric blame-reveal-color-calculate (strategy commit-hash context))
(cl-defgeneric blame-reveal-color-quality (strategy num-commits context))
(cl-defgeneric blame-reveal-color-old (strategy context))
(cl-defgeneric blame-reveal-color-uncommitted (strategy context))

;;; Utility Functions

(defun blame-reveal-color--is-dark-theme-p ()
  "Check if the current Emacs theme has a dark background."
  (let* ((bg (face-background 'default))
         (rgb (and bg (color-name-to-rgb bg))))
    (when rgb
      (let ((r (nth 0 rgb))
            (g (nth 1 rgb))
            (b (nth 2 rgb)))
        (< (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 0.5)))))

(defun blame-reveal-color--ease-out-cubic (x)
  (let ((x1 (- 1.0 x)))
    (- 1.0 (* x1 x1 x1))))

(defun blame-reveal-color--hsl-to-hex (h s l)
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1)))))
         (m (- l (/ c 2.0)))
         (rgb (cond ((< h 60) (list c x 0)) ((< h 120) (list x c 0))
                    ((< h 180) (list 0 c x)) ((< h 240) (list 0 x c))
                    ((< h 300) (list x 0 c)) (t (list c 0 x))))
         (r (round (* 255 (+ (nth 0 rgb) m))))
         (g (round (* 255 (+ (nth 1 rgb) m))))
         (b (round (* 255 (+ (nth 2 rgb) m)))))
    (format "#%02x%02x%02x" r g b)))

;;; Gradient Strategy

(cl-defstruct (blame-reveal-gradient-strategy
               (:include blame-reveal-color-strategy)
               (:constructor blame-reveal-gradient-strategy-create))
  (hue 210) (dark-newest 0.70) (dark-oldest 0.35)
  (light-newest 0.45) (light-oldest 0.75)
  (saturation-min 0.25) (saturation-max 0.60))

(cl-defmethod blame-reveal-color-calculate
  ((strategy blame-reveal-gradient-strategy) _commit-hash context)
  (let* ((rank (plist-get context :rank))
         (total-recent (plist-get context :total-recent))
         (is-dark (plist-get context :is-dark)))
    (when (and rank total-recent)
      (let* ((age-ratio (if (= total-recent 1) 0.0
                          (/ (float rank) (- total-recent 1))))
             (eased-ratio (blame-reveal-color--ease-out-cubic age-ratio))
             (hue (blame-reveal-gradient-strategy-hue strategy))
             (sat-min (blame-reveal-gradient-strategy-saturation-min strategy))
             (sat-max (blame-reveal-gradient-strategy-saturation-max strategy))
             (saturation (+ sat-min (* (- sat-max sat-min) (- 1.0 eased-ratio))))
             (lightness (if is-dark
                            (let ((newest (blame-reveal-gradient-strategy-dark-newest strategy))
                                  (oldest (blame-reveal-gradient-strategy-dark-oldest strategy)))
                              (+ oldest (* (- newest oldest) (- 1.0 eased-ratio))))
                          (let ((newest (blame-reveal-gradient-strategy-light-newest strategy))
                                (oldest (blame-reveal-gradient-strategy-light-oldest strategy)))
                            (- oldest (* (- oldest newest) (- 1.0 eased-ratio)))))))
        (blame-reveal-color--hsl-to-hex hue saturation lightness)))))

(cl-defmethod blame-reveal-color-quality
  ((strategy blame-reveal-gradient-strategy) num-commits context)
  (when (and (> num-commits 0) strategy)
    (let* ((is-dark (plist-get context :is-dark))
           (lightness-range
            (if is-dark
                (- (blame-reveal-gradient-strategy-dark-newest strategy)
                   (blame-reveal-gradient-strategy-dark-oldest strategy))
              (- (blame-reveal-gradient-strategy-light-oldest strategy)
                 (blame-reveal-gradient-strategy-light-newest strategy))))
           (saturation-range
            (- (blame-reveal-gradient-strategy-saturation-max strategy)
               (blame-reveal-gradient-strategy-saturation-min strategy)))
           (lightness-step (/ lightness-range (float (max 1 (1- num-commits)))))
           (saturation-step (/ saturation-range (float (max 1 (1- num-commits)))))
           (combined-step (+ (* 0.7 lightness-step) (* 0.3 saturation-step))))
      (cond ((>= combined-step 0.05) 1.0) ((>= combined-step 0.03) 0.8)
            ((>= combined-step 0.02) 0.5) (t (* combined-step 20))))))

(cl-defmethod blame-reveal-color-old
  ((strategy blame-reveal-gradient-strategy) context)
  (let* ((is-dark (plist-get context :is-dark))
         (hue (blame-reveal-gradient-strategy-hue strategy))
         (saturation (blame-reveal-gradient-strategy-saturation-min strategy))
         (lightness (if is-dark
                        (* (blame-reveal-gradient-strategy-dark-oldest strategy) 0.7)
                      (let ((oldest (blame-reveal-gradient-strategy-light-oldest strategy)))
                        (+ oldest (* (- 1.0 oldest) 0.5))))))
    (blame-reveal-color--hsl-to-hex hue saturation lightness)))

(cl-defmethod blame-reveal-color-uncommitted
  ((_strategy blame-reveal-gradient-strategy) context)
  (if (plist-get context :is-dark) "#d9a066" "#e6b380"))

;;; Strategy Initialization

(defun blame-reveal--init-color-strategy ()
  (setq blame-reveal--color-strategy
        (blame-reveal-gradient-strategy-create
         :hue (plist-get blame-reveal-color-scheme :hue)
         :dark-newest (plist-get blame-reveal-color-scheme :dark-newest)
         :dark-oldest (plist-get blame-reveal-color-scheme :dark-oldest)
         :light-newest (plist-get blame-reveal-color-scheme :light-newest)
         :light-oldest (plist-get blame-reveal-color-scheme :light-oldest)
         :saturation-min (plist-get blame-reveal-color-scheme :saturation-min)
         :saturation-max (plist-get blame-reveal-color-scheme :saturation-max))))

;;; Color Access Functions

(defun blame-reveal--get-commit-color (commit-hash)
  "Get the calculated color for COMMIT-HASH. Uses cache and handles all types."
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))

  (let* ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
         (is-recent (blame-reveal--is-recent-commit-p commit-hash))
         (cached-color (gethash commit-hash blame-reveal--color-map)))
    (cond
     (is-uncommitted ;; 1. Uncommitted
      (blame-reveal--get-uncommitted-color))
     (cached-color) ;; 2. Cache Hit
     (t ;; 3. Calculation (Cache Miss)
      (let* ((color
              (cond
               ;; Recent Commit (calculate gradient)
               (is-recent
                (let* ((info (gethash commit-hash blame-reveal--commit-info))
                       (timestamp (and info (nth 4 info)))
                       (rank (cl-position commit-hash blame-reveal--recent-commits :test 'equal))
                       (context (list :timestamp timestamp :rank rank
                                      :total-recent (length blame-reveal--recent-commits)
                                      :is-dark (blame-reveal-color--is-dark-theme-p))))
                  (blame-reveal-color-calculate blame-reveal--color-strategy commit-hash context)))
               ;; Old Commit (get old commit color)
               (t
                (blame-reveal--get-old-commit-color)))))
        ;; Cache and return color, or default if calculation fails
        (when color (puthash commit-hash color blame-reveal--color-map))
        (or color (if (blame-reveal-color--is-dark-theme-p) "#6699cc" "#7799bb")))))))

(defun blame-reveal--get-old-commit-color ()
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (or blame-reveal-old-commit-color
      (blame-reveal-color-old blame-reveal--color-strategy
                              (list :is-dark (blame-reveal-color--is-dark-theme-p)))))

(defun blame-reveal--get-uncommitted-color ()
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (or blame-reveal-uncommitted-color
      (blame-reveal-color-uncommitted blame-reveal--color-strategy
                                      (list :is-dark (blame-reveal-color--is-dark-theme-p)))))

(defun blame-reveal--calculate-gradient-quality (num-commits)
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (blame-reveal-color-quality blame-reveal--color-strategy num-commits
                              (list :is-dark (blame-reveal-color--is-dark-theme-p))))

(defun blame-reveal--get-commit-display-info (commit-hash)
  "Get display information for COMMIT-HASH, including color and status flags.
Returns a list: (COLOR IS-UNCOMMITTED IS-OLD-COMMIT HIDE-FRINGE)."
  (blame-reveal--ensure-commit-info commit-hash)
  (let* ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
         ;; Determine if the commit is 'old' (not uncommitted AND not recent).
         (is-old-commit (and (not is-uncommitted)
                             (not (blame-reveal--is-recent-commit-p commit-hash))))
         ;; Fetch the color. The 'blame-reveal--get-commit-color' function
         ;; now handles all commit types (uncommitted, old, recent) and caching.
         (color (blame-reveal--get-commit-color commit-hash))
         ;; Determine if the fringe for uncommitted changes should be hidden.
         (hide-fringe (and is-uncommitted
                           (not blame-reveal-show-uncommitted-fringe))))
    (list color is-uncommitted is-old-commit hide-fringe)))

;;; Commit Selection

(defun blame-reveal--is-recent-commit-p (commit-hash)
  "Check if COMMIT-HASH is in the recent commits list."
  (member commit-hash blame-reveal--recent-commits))

(defun blame-reveal--get-reference-time ()
  "Get reference time for age calculation."
  (if (and (boundp 'blame-reveal--current-revision)
           blame-reveal--current-revision
           (not (eq blame-reveal--current-revision 'uncommitted)))
      (or (cl-loop for info being the hash-values of blame-reveal--commit-info
                   when (nth 4 info)
                   maximize (nth 4 info))
          (float-time))
    (float-time)))

(defun blame-reveal--update-recent-commits ()
  "Update list of recent commits based on days limit."
  (when blame-reveal--commit-info
    (let* ((commit-timestamps nil)
           (reference-time (blame-reveal--get-reference-time))
           (days-limit (pcase blame-reveal-recent-days-limit
                         ('auto (or (blame-reveal--auto-calculate-days-limit) 90))
                         ('nil nil)
                         (_ blame-reveal-recent-days-limit))))
      (maphash (lambda (commit info)
                 (when-let ((timestamp (nth 4 info)))
                   (push (cons commit timestamp) commit-timestamps)))
               blame-reveal--commit-info)
      (setq commit-timestamps
            (sort commit-timestamps
                  (lambda (a b) (> (cdr a) (cdr b)))))
      (when days-limit
        (let ((age-limit-seconds (* days-limit 86400)))
          (setq commit-timestamps
                (cl-remove-if
                 (lambda (commit-ts)
                   (> (- reference-time (cdr commit-ts))
                      age-limit-seconds))
                 commit-timestamps))))
      (setq blame-reveal--recent-commits
            (mapcar #'car commit-timestamps)))))

;;; Gradient Quality Assessment

(defun blame-reveal--get-quality-thresholds ()
  "Get gradient quality thresholds based on user setting."
  (pcase blame-reveal-gradient-quality
    ('strict  '(:min-quality 0.8 :max-commits 10))
    ('auto    '(:min-quality 0.5 :max-commits 20))
    ('relaxed '(:min-quality 0.3 :max-commits 30))
    (_ '(:min-quality 0.5 :max-commits 20))))

(defun blame-reveal--should-recalculate-p ()
  "Check if auto-calculation should be triggered."
  (if (not blame-reveal--auto-days-cache)
      t
    (let* ((cached-count (car blame-reveal--auto-days-cache))
           (current-count (hash-table-count blame-reveal--commit-info))
           (count-diff (abs (- current-count cached-count)))
           (count-ratio (if (> cached-count 0)
                            (/ (float count-diff) cached-count)
                          1.0)))
      (or (> count-diff 10)
          (> count-ratio 0.2)))))

(defun blame-reveal--collect-timestamps ()
  "Collect all commit timestamps from commit-info.
Returns sorted list (newest first)."
  (let ((timestamps nil))
    (maphash (lambda (_commit info)
               (when-let ((ts (nth 4 info)))
                 (push ts timestamps)))
             blame-reveal--commit-info)
    (sort timestamps #'>)))

(defun blame-reveal--calculate-sample-span (timestamps)
  "Calculate time span of sample commits.
Returns (SAMPLE-SIZE . SPAN-DAYS)."
  (let* ((total (length timestamps))
         (sample-size (cond
                       ((>= total 15) 15)
                       ((>= total 10) 10)
                       ((>= total 5) 5)
                       (t total)))
         (sample (seq-take timestamps sample-size))
         (span-seconds (when (> (length sample) 1)
                         (- (car sample) (car (last sample)))))
         (span-days (when span-seconds
                      (/ span-seconds 86400.0))))
    (cons sample-size span-days)))

(defun blame-reveal--estimate-base-days (sample-size span-days)
  "Estimate base days limit from sample.
Returns estimated days."
  (when span-days
    (let* ((extension-factor
            (cond
             ((>= sample-size 15) 1.5)
             ((>= sample-size 10) 1.8)
             ((>= sample-size 5) 2.0)
             (t 2.5)))
           (base-days (* span-days extension-factor))
           (min-days-for-gradient
            (cond
             ((< span-days 7) 7)
             ((< span-days 30) 14)
             (t 30))))
      (max base-days min-days-for-gradient))))

(defun blame-reveal--count-commits-in-range (timestamps days reference-time)
  "Count commits within DAYS from REFERENCE-TIME.
Returns number of commits. Optimized using sorted list properties."
  (let* ((age-limit-seconds (* days 86400))
         (oldest-ts-in-range (- reference-time age-limit-seconds))
         ;; We need to find the position of the *first* timestamp
         ;; that is OLDER than the limit, because timestamps is sorted
         ;; newest (large number) to oldest (small number).
         ;; All commits before this position are within the age limit.
         (first-older-pos
          (cl-position-if (lambda (ts) (< ts oldest-ts-in-range))
                          timestamps)))
    (if first-older-pos
        ;; If an older commit is found, the count is its position index.
        first-older-pos
      ;; If no commit is older than the limit, all commits are in range.
      (length timestamps))))

(defun blame-reveal--adjust-for-minimum-commits (days timestamps reference-time total-commits)
  "Ensure at least minimum commits in range.
Returns adjusted days."
  (let ((min-commits 5)
        (max-days 730)
        (commits-in-range (blame-reveal--count-commits-in-range
                           timestamps days reference-time)))
    (while (and (< commits-in-range min-commits)
                (< days max-days)
                (< commits-in-range total-commits))
      (setq days (* days 1.5))
      (setq commits-in-range (blame-reveal--count-commits-in-range
                              timestamps days reference-time)))
    days))

(cl-defun blame-reveal--adjust-for-quality (days timestamps reference-time)
  "Adjust the days limit to meet color gradient quality requirements.
This process balances the need for a good visual gradient (high quality)
against the desire to show more recent history (more commits).

Returns a cons cell: (ADJUSTED-DAYS . FINAL-COMMITS)."
  (let* ((thresholds (blame-reveal--get-quality-thresholds))
         (min-quality (plist-get thresholds :min-quality))
         (max-commits (plist-get thresholds :max-commits))
         (min-commits 5) ; Hardcoded minimum number of commits for a reasonable gradient
         ;; Helper function to calculate current range and quality
         (update-metrics (lambda (current-days)
                           (let* ((count (blame-reveal--count-commits-in-range
                                          timestamps current-days reference-time))
                                  (quality (blame-reveal--calculate-gradient-quality count)))
                             (list count quality))))

         ;; Initial metrics
         (metrics (funcall update-metrics days))
         (commits-in-range (nth 0 metrics))
         (gradient-quality (nth 1 metrics))
         (iterations 0))
    ;; --- PHASE 1: Reduce days to improve quality (if quality is too low) ---
    (while (and gradient-quality
                (< gradient-quality min-quality)
                (> commits-in-range min-commits)
                (< iterations 10))
      (setq days (* days 0.75))
      (setq metrics (funcall update-metrics days)
            commits-in-range (nth 0 metrics)
            gradient-quality (nth 1 metrics)
            iterations (1+ iterations)))
    ;; --- PHASE 2: Increase days to capture more history (if quality is high) ---
    (setq iterations 0)
    (while (and gradient-quality
                (> gradient-quality 0.9) ; High quality threshold for expansion
                (< commits-in-range max-commits)
                (< iterations 10))
      (let* ((new-days (* days 1.3))
             (new-metrics (funcall update-metrics new-days))
             (new-count (nth 0 new-metrics))
             (new-quality (nth 1 new-metrics)))
        ;; Only commit to the expansion if the quality remains acceptable
        (when (and new-quality (>= new-quality min-quality))
          (setq days new-days)
          (setq commits-in-range new-count)))
      ;; Recalculate current quality for the next loop iteration based on possibly updated 'days'
      (setq gradient-quality (blame-reveal--calculate-gradient-quality commits-in-range))
      (setq iterations (1+ iterations)))
    (cons days commits-in-range)))

(defun blame-reveal--validate-and-finalize (days sample-size span-days)
  "Validate and finalize days limit.
Returns final days (integer)."
  (when (and days
             (> days (* span-days 3)))
    (setq days (max (cond
                     ((< span-days 7) 7)
                     ((< span-days 30) 14)
                     (t 30))
                    (* span-days 2))))
  (max 7 (min 730 (ceiling days))))

(defun blame-reveal--auto-calculate-days-limit ()
  "Automatically calculate days limit for optimal color gradient.

Algorithm:
1. Collect and sort timestamps
2. Calculate sample span (newest 5-15 commits)
3. Estimate base days with extension factor
4. Ensure minimum commits (5+)
5. Adjust for quality requirements
6. Validate and cache result

Returns: integer days limit or nil."
  (when (and blame-reveal--commit-info
             (> (hash-table-count blame-reveal--commit-info) 3))
    ;; Check cache
    (if (and blame-reveal--auto-days-cache
             (not (blame-reveal--should-recalculate-p)))
        (cdr blame-reveal--auto-days-cache)
      ;; Recalculate
      (let* ((timestamps (blame-reveal--collect-timestamps))
             (total-commits (length timestamps)))
        (when (>= total-commits 3)
          (pcase-let* ((reference-time (blame-reveal--get-reference-time))
                       (`(,sample-size . ,span-days)
                        (blame-reveal--calculate-sample-span timestamps))
                       (base-days (blame-reveal--estimate-base-days
                                   sample-size span-days))
                       (adjusted-days (blame-reveal--adjust-for-minimum-commits
                                       base-days timestamps reference-time
                                       total-commits))
                       (`(,final-days . ,final-commits)
                        (blame-reveal--adjust-for-quality
                         adjusted-days timestamps reference-time))
                       (validated-days (blame-reveal--validate-and-finalize
                                        final-days sample-size span-days)))
            ;; Cache result
            (setq blame-reveal--auto-days-cache
                  (cons (hash-table-count blame-reveal--commit-info)
                        validated-days))
            validated-days))))))

(provide 'blame-reveal-color)
;;; blame-reveal-color.el ends here
