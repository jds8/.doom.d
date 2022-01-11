(require 'dash)
(require 'cl-lib)

;; period constructor
(cl-defstruct (period (:constructor period-create)
                      (:copier nil))
  day month year)

;; interleave two lists
(defun both (a b) (list a b))
(defun interleave-h (a b c)
  (if (equal a nil) c
    (interleave-h (cdr a) (cdr b) (append c (both (car a) (car b))))))
(defun interleave (a b)
  (interleave-h a b '()))

;; remove '(nil nil nil)
(defun remove-nil (x)
   (-filter '(lambda (y) (not (equal y '(nil nil nil)))) x)
  )

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

(defun get-periods (file)
  (sort (mapcar
 '(lambda (a) (apply 'period-create
                     (interleave '(:year :month :day)
                                 (mapcar 'string-to-number a))))
 (remove-nil (with-temp-buffer
  (org-mode)
  (insert-file-contents file)
  (goto-char (point-min))
  (org-map-entries '(lambda ()
                      (list (org-entry-get nil "YEAR")
                      (org-entry-get nil "MONTH")
                      (org-entry-get nil "DAY"))))))) 'before))
