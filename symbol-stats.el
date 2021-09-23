;; reference: https://taeric.github.io/CodeAsData.html

(require 'cl-lib)

;; append and remove symbols for differential: 'd 'z => 'dz and 'dz => 'z
(defun symbol-stats/symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))
(defun symbol-stats/symbol-end (symbol)
  (intern (substring (symbol-name symbol) 1)))

;; equality
(defun symbol-stats/are-equal (t1 t2) (equal t1 t2))

;; divide
(defun symbol-stats/add (t1 t2)
  (cond (t (list '+ t1 t2))
))
(defun symbol-stats/is-add (t1) (equal (car t1) '+))
(defun symbol-stats/addend (t1) (cadr t1))
(defun symbol-stats/addother (t1) (caddr t1))

;; subtract
(defun symbol-stats/subtract (t1 t2)
  (cond (t (list '- t1 t2))
))
(defun symbol-stats/is-subtract (t1) (equal (car t1) '-))
(defun symbol-stats/subend (t1) (cadr t1))
(defun symbol-stats/subother (t1) (caddr t1))
(defun symbol-stats/negate (t1)
  (cond ((equal (car t1) '-) (cdr t1))
        (t (list '- t1))
))

;; multiply
(defun symbol-stats/multiply (t1 t2)
  (cond (t (list '* t1 t2))
  ))
(defun symbol-stats/is-mul (t1) (equal (car t1) '*))
(defun symbol-stats/multiplier (t1) (cadr t1))
(defun symbol-stats/multiplicand (t1) (caddr t1))

;; divide
(defun symbol-stats/divide (t1 t2)
  (cond (t (list '/ t1 t2))
))
(defun symbol-stats/is-div (t1) (equal (car t1) '/))
(defun symbol-stats/num (t1) (cadr t1))
(defun symbol-stats/denom (t1) (caddr t1))

;; create distributions
(defun symbol-stats/make-dist (p left right) (list (symbol-stats/symbol-append p 'dist) left right))
(defun symbol-stats/is-dist (t1) (equal (symbol-stats/symbol-end (car t1)) 'dist))
(defun symbol-stats/vars (t1) (cdr t1))

;; create integrals
(defun symbol-stats/make-int (t1 var) (list 'int t (symbol-stats/symbol-append 'd var)))
(defun symbol-stats/is-int (t1) (equal (car t1) 'int))
(defun symbol-stats/integrand (t1) (cadr t1))
(defun symbol-stats/differential (t1) (symbol-stats/symbol-end (car (last t1))))

(defun symbol-stats/log (t1)
  (cond ((symbol-stats/is-mul t1) (+ (symbol-stats/log (symbol-stats/num t1)) (symbol-stats/log (symbol-stats/denom t1))))
        ((symbol-stats/is-div t1) (- (symbol-stats/log (symbol-stats/num t1)) (symbol-stats/log (symbol-stats/denom t1))))
        (t (list 'log t1))
        ))

(and t (let ((x 3) (y 3)) (+ x y)))
(match-data (list '* 1 2)
       ((cons x y) x))
(cons '* (list x y))

(defun symbol-stats/make-expectation (t1 pd)
  (let ((p (symbol-stats/symbol-append pd 'dist)))
        (cond ((symbol-stats/is-add t1) (symbol-stats/add (symbol-stats/make-expectation (symbol-stats/addend t1) p) (symbol-stats/make-expectation (symbol-stats/addother t1) p)))
                ((symbol-stats/is-subtract t1) (symbol-stats/subtract (symbol-stats/make-expectation (symbol-stats/subend t1) p) (symbol-stats/make-expectation (symbol-stats/subother t1) p)))
                ((and (symbol-stats/is-mul t1) (let ((t2 (symbol-stats/multiplier t1)) (t3 (symbol-stats/multiplicand t1))) (and (symbol-stats/is-dist t2) (symbol-stats/are-equal t3 (symbol-stats/log t2)))))
                 (let ((t2 (symbol-stats/multiplier t1)))
                   (symbol-stats/negate (symbol-stats/entropy
                        (car (symbol-stats/vars t2)) (cadr (symbol-stats/vars t2))
                        ))))
                ((and (symbol-stats/is-mul t1) (let ((t2 (symbol-stats/multiplier t1)) (t3 (symbol-stats/multiplicand t1))) (and (symbol-stats/is-dist t3) (symbol-stats/are-equal t2 (symbol-stats/log t3)))))
                 (let ((t3 (symbol-stats/multiplier t1)))
                        (symbol-stats/negate (symbol-stats/entropy
                        (car (symbol-stats/vars t3)) (cadr (symbol-stats/vars t3))
                        ))))
                (t (list 'E t1 p))
        )))
(symbol-stats/make-expectation (symbol-stats/multiply data (symbol-stats/log data)) 'q)

;; create entropy
(defun symbol-stats/entropy (v1 v2) (list 'H v1 v2))

;; operations on terms
(defun symbol-stats/marginalize (p var) (make-int (make-dist (car p) (list (cadr p) var) (list (caddr p))) var))
(defun symbol-stats/mult-one (t1 p)
  (cond ((is-int t1) (make-int (mult-one (integrand t1) p) (differential t1)))
        (t t1)
  ))

;; constructor
;; user must specify all random variables in the joint distribution, joint-vars,
;; and the starting term, term
(cl-defstruct (full-term (:constructor full-term-create)
                         (:copier nil))
  joint-vars term)

(setq ft (full-term-create :joint-vars (list 'x 'z) :term (list 'log (symbol-stats/make-dist 'p (list 'x) nil))))
(full-term-term ft)

;; examples
(setq data (symbol-stats/make-dist 'p (list 'x 'h) 'y))
(symbol-stats/are-equal data (symbol-stats/denom (symbol-stats/divide data data)))
(setq I (marginalize data 'z))
(symbol-stats/is-dist (integrand I))

(integrand I)
(differential I)
(mult-one I (make-dist 'q 'z nil))

(provide 'symbol-stats)
