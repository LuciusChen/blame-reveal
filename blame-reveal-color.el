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
  (let* ((bg (or (face-background 'default) "white"))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      (< (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb)) (* 0.114 (nth 2 rgb))) 0.5))))

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
  (unless blame-reveal--color-strategy (blame-reveal--init-color-strategy))
  (if (blame-reveal--is-uncommitted-p commit-hash)
      (blame-reveal--get-uncommitted-color)
    (or (gethash commit-hash blame-reveal--color-map)
        (let* ((info (gethash commit-hash blame-reveal--commit-info))
               (timestamp (and info (nth 4 info)))
               (rank (cl-position commit-hash blame-reveal--recent-commits :test 'equal))
               (context (list :timestamp timestamp :rank rank
                              :total-recent (length blame-reveal--recent-commits)
                              :is-dark (blame-reveal-color--is-dark-theme-p)
                              :min-timestamp (car blame-reveal--timestamps)
                              :max-timestamp (cdr blame-reveal--timestamps)))
               (color (if rank
                          (blame-reveal-color-calculate blame-reveal--color-strategy commit-hash context)
                        (blame-reveal--get-old-commit-color))))
          (when color (puthash commit-hash color blame-reveal--color-map))
          (or color (if (blame-reveal-color--is-dark-theme-p) "#6699cc" "#7799bb"))))))

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
  "Get display info for COMMIT-HASH.
Returns (COLOR IS-UNCOMMITTED IS-OLD-COMMIT HIDE-FRINGE)."
  (blame-reveal--ensure-commit-info commit-hash)
  (let* ((is-uncommitted (blame-reveal--is-uncommitted-p commit-hash))
         (is-old-commit (and (not is-uncommitted)
                             (not (blame-reveal--should-render-commit commit-hash))))
         (color (cond
                 (is-uncommitted (blame-reveal--get-uncommitted-color))
                 (is-old-commit (blame-reveal--get-old-commit-color))
                 (t (blame-reveal--get-commit-color commit-hash))))
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

(defun blame-reveal--auto-calculate-days-limit ()
  "Automatically calculate days limit for optimal color gradient."
  (when (and blame-reveal--commit-info
             (> (hash-table-count blame-reveal--commit-info) 3))
    (if (and blame-reveal--auto-days-cache
             (not (blame-reveal--should-recalculate-p)))
        (cdr blame-reveal--auto-days-cache)
      (let* ((timestamps nil)
             (reference-time (blame-reveal--get-reference-time))
             (current-count (hash-table-count blame-reveal--commit-info)))
        (maphash (lambda (_commit info)
                   (when-let ((ts (nth 4 info)))
                     (push ts timestamps)))
                 blame-reveal--commit-info)
        (when (>= (length timestamps) 3)
          (setq timestamps (sort timestamps #'>))
          (let* ((total-commits (length timestamps))
                 (sample-size (cond
                               ((>= total-commits 15) 15)
                               ((>= total-commits 10) 10)
                               ((>= total-commits 5) 5)
                               (t total-commits)))
                 (sample (seq-take timestamps sample-size))
                 (span-seconds (when (> (length sample) 1)
                                 (- (car sample) (car (last sample)))))
                 (span-days (when span-seconds
                              (/ span-seconds 86400.0))))
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
                       (t 30)))
                     (adjusted-days (max base-days min-days-for-gradient))
                     (thresholds (blame-reveal--get-quality-thresholds))
                     (min-quality (plist-get thresholds :min-quality))
                     (max-commits (plist-get thresholds :max-commits))
                     (min-commits 5)
                     (target-commits (min max-commits total-commits))
                     (age-limit-seconds (* adjusted-days 86400))
                     (commits-in-range
                      (length (cl-remove-if
                               (lambda (ts)
                                 (> (- reference-time ts) age-limit-seconds))
                               timestamps))))
                (while (and (< commits-in-range min-commits)
                            (< adjusted-days 730)
                            (< commits-in-range total-commits))
                  (setq adjusted-days (* adjusted-days 1.5))
                  (setq age-limit-seconds (* adjusted-days 86400))
                  (setq commits-in-range
                        (length (cl-remove-if
                                 (lambda (ts)
                                   (> (- reference-time ts) age-limit-seconds))
                                 timestamps))))
                (let ((gradient-quality (blame-reveal--calculate-gradient-quality
                                         commits-in-range))
                      (iterations 0))
                  (while (and gradient-quality
                              (< gradient-quality min-quality)
                              (> commits-in-range min-commits)
                              (> adjusted-days min-days-for-gradient)
                              (< iterations 10))
                    (setq adjusted-days (* adjusted-days 0.75))
                    (setq age-limit-seconds (* adjusted-days 86400))
                    (setq commits-in-range
                          (length (cl-remove-if
                                   (lambda (ts)
                                     (> (- reference-time ts) age-limit-seconds))
                                   timestamps)))
                    (setq gradient-quality
                          (blame-reveal--calculate-gradient-quality commits-in-range))
                    (setq iterations (1+ iterations)))
                  (setq iterations 0)
                  (while (and gradient-quality
                              (> gradient-quality 0.9)
                              (< commits-in-range target-commits)
                              (< adjusted-days 730)
                              (< commits-in-range total-commits)
                              (< iterations 10))
                    (setq adjusted-days (* adjusted-days 1.3))
                    (setq age-limit-seconds (* adjusted-days 86400))
                    (let ((new-count (length (cl-remove-if
                                              (lambda (ts)
                                                (> (- reference-time ts) age-limit-seconds))
                                              timestamps))))
                      (when (>= (blame-reveal--calculate-gradient-quality new-count)
                                min-quality)
                        (setq commits-in-range new-count))
                      (setq iterations (1+ iterations)))))
                (when (and (<= commits-in-range total-commits)
                           (> adjusted-days (* span-days 3)))
                  (setq adjusted-days (max min-days-for-gradient
                                           (* span-days 2))))
                (let ((final-days (max 7 (min 730 (ceiling adjusted-days)))))
                  (setq blame-reveal--auto-days-cache
                        (cons current-count final-days))
                  final-days)))))))))

(provide 'blame-reveal-color)
;;; blame-reveal-color.el ends here
