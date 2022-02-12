(require 'dash)
(require 'cl-lib)

;; period constructor
(cl-defstruct (period (:constructor period-create)
                      (:copier nil))
  day month year node)

;; interleave two lists
(defun both (a b) (list a b))
(defun interleave-h (a b c)
  (if (equal a nil) c
    (interleave-h (cdr a) (cdr b) (append c (both (car a) (car b))))))
(defun interleave (a b)
  (interleave-h a b '()))

;; remove '(nil nil nil _)
(defun remove-nil (x)
  (-filter '(lambda (y)
              (not (equal
                    (list (car y) (cadr y) (caddr y))
                    '(nil nil nil)))) x))

;; compare periods
(defun before (a b)
  (let ((dyear (- (period-year a) (period-year b)))
        (dmonth (- (period-month a) (period-month b)))
        (dday (- (period-day a) (period-day b))))
    (if (< dyear 0) t
      (if (> dyear 0) nil
        (if (< dmonth 0) t
          (if (> dmonth 0) nil (< dday 0)))))))

(defun after (a b) (not (before a b)))

;; difference between dates
(defun date-diff (a b)
  (let ((year-diff (- (period-year a) (period-year b)))
        (month-diff (- (period-month a) (period-month b)))
        (day-diff (- (period-day a) (period-day b))))
    (+ (* 365 year-diff) (* 31 month-diff) day-diff)))

;; e.g.
;; (let ((a (period-create :year 2021 :month 11 :day 1))
;;       (b (period-create :year 2021 :month 11 :day 1)))
;;   (date-diff a b))

;; proportion that period is of days starting at start
(defun date-proportion (start period days)
  (let ((dif (date-diff period start)))
    (/ (float dif) days)))

;; e.g.
;; (let ((a (period-create :year 2021 :month 11 :day 1))
;;       (b (period-create :year 2020 :month 10 :day 13))
;;       (c (period-create :year 2021 :month 1 :day 3)))
;;   (date-proportion b c (date-diff a b)))

;;str-to-num how I need it
(defun try-str-to-num (s)
  (let ((n (string-to-number s)))
    (if (and (equal n 0) (not (equal s "0"))) s n)))

(defun get-periods (file)
  (sort (mapcar
 '(lambda (a) (apply 'period-create
                     (interleave '(:year :month :day :node)
                                 (mapcar 'try-str-to-num a))))
 (remove-nil (with-temp-buffer
  (org-mode)
  (insert-file-contents file)
  (goto-char (point-min))
  (org-map-entries '(lambda ()
                      (list (org-entry-get nil "YEAR")
                      (org-entry-get nil "MONTH")
                      (org-entry-get nil "DAY")
                      (org-entry-get nil "ITEM")
                      )))))) 'before))

(setq file (concat (file-name-as-directory org-directory) "Notes/Research/jobs.org"))

(get-periods file)
