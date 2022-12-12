;; reference: https://taeric.github.io/CodeAsData.html

(require 'cl-lib)

;; utils
(defun symbol-stats/any (v lst) (seq-reduce #'(lambda (truth-val p) (or (eq p v) truth-val)) lst nil))
(defun symbol-stats/contains-elements (lst elements)
  (cond ((eq elements nil) t)
        ((and (not (listp elements)) (-contains? lst elements)) t)
        ((and (not (listp elements)) (not (-contains? lst elements))) nil)
        (t (and (-contains? lst (car elements)) (contains-elements lst (cdr elements))))))
(defun symbol-stats/single-set-diff-helper (lst element nonelements)
  (cond ((eq lst nil) nonelements)
        ((eq (car lst) element) (append nonelements (cdr lst)))
        (t (symbol-stats/set-diff-helper (cdr lst) element (cons (car lst) nonelements)))
  ))
(defun symbol-stats/set-diff-helper (lst elements nonelements)
  (cond ((eq lst nil) nonelements)
        ((symbol-stats/contains-elements elements (car lst)) (symbol-stats/set-diff-helper (cdr lst) elements nonelements))
        (t (symbol-stats/set-diff-helper (cdr lst) elements (cons (car lst) nonelements)))
  ))
(defun symbol-stats/set-diff (lst elements) (symbol-stats/set-diff-helper lst elements '()))

;; append and remove symbols for differential: 'd 'z => 'dz and 'dz => 'z
(defun symbol-stats/symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))
(defun symbol-stats/symbol-end (symbol)
  (intern (substring (symbol-name symbol) 1)))

;; equality
(defun symbol-stats/are-equal (t1 t2) (equal t1 t2))
(defun symbol-stats/full-term-are-equal (ft1 ft2) (symbol-stats/are-equal (full-term-term ft1)) (full-term-term ft2))

;; divide
(defun symbol-stats/add (t1 t2)
  (cond (t (list '+ t1 t2))
))
(defun symbol-stats/is-add (t1) (equal (car t1) '+))
(defun symbol-stats/full-term-is-add (ft) (symbol-stats/is-add (full-term-term ft)))
(defun symbol-stats/addend (t1) (cadr t1))
(defun symbol-stats/addother (t1) (caddr t1))

;; subtract
(defun symbol-stats/subtract (t1 t2)
  (cond (t (list '- t1 t2))
))
(defun symbol-stats/is-subtract (t1) (equal (car t1) '-))
(defun symbol-stats/full-term-is-subtract (ft) (symbol-stats/is-subtract (full-term-term ft)))
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
(defun symbol-stats/full-term-is-mul (ft) (symbol-stats/is-mul (full-term-term ft)))
(defun symbol-stats/multiplier (t1) (cadr t1))
(defun symbol-stats/multiplicand (t1) (caddr t1))

;; divide
(defun symbol-stats/divide (t1 t2)
  (cond (t (list '/ t1 t2))
))
(defun symbol-stats/is-div (t1) (equal (car t1) '/))
(defun symbol-stats/full-term-is-div (ft) (symbol-stats/is-div (full-term-term ft)))
(defun symbol-stats/num (t1) (cadr t1))
(defun symbol-stats/denom (t1) (caddr t1))

;; logarithm
(defun symbol-stats/log (t1)
  (cond ((symbol-stats/is-mul t1) (+ (symbol-stats/log (symbol-stats/num t1)) (symbol-stats/log (symbol-stats/denom t1))))
        ((symbol-stats/is-div t1) (- (symbol-stats/log (symbol-stats/num t1)) (symbol-stats/log (symbol-stats/denom t1))))
        (t (list 'log t1))
        ))
(defun symbol-stats/is-log (t1) (eq (car t1) 'log))
(defun symbol-stats/full-term-is-log (ft) (symbol-stats/is-log (full-term-term ft)))

;; create distributions
(defun symbol-stats/is-dist (t1) (equal (symbol-stats/symbol-end (car t1)) 'dist))
(defun symbol-stats/full-term-is-dist (ft) (symbol-stats/is-dist (full-term-term ft)))
(defun symbol-stats/vars (t1) (cdr t1))
(defun symbol-stats/get-dist (p) (if (equal (symbol-stats/symbol-end p) 'dist) p (symbol-stats/symbol-append p 'dist)))
(defun symbol-stats/make-dist (p left right) (list (symbol-stats/get-dist p) left right))
(defun symbol-stats/conditions-on (pd vars) (symbol-stats/contains-elements (caddr pd) vars))

;; Bayes' Rule
(defun symbol-stats/bayes-rule-from-posterior (posterior prior-rhs)
  (let ((posterior-vars (cadr posterior)))
    (let ((likelihood-lhs (symbol-stats/set-diff (caddr posterior) prior-rhs)))
      (let ((likelihood (symbol-stats/make-dist 'p likelihood-lhs (append posterior-vars prior-rhs)))
            (prior (symbol-stats/make-dist 'p posterior-vars prior-rhs))
            (evidence (symbol-stats/make-dist 'p likelihood-lhs prior-rhs)))
        (symbol-stats/multiply (symbol-stats/divide likelihood evidence) prior)))))
(defun symbol-stats/bayes-rule-to-posterior-helper (likelihood prior evidence)
  (let ((lhs (cadr prior))
        (rhs (append (cadr likelihood) (caddr prior))))
   (symbol-stats/make-dist (car prior) lhs rhs)))
;; assume that multiplication cannot occur in a denominator
(defun symbol-stats/get-bayes-terms (term1 term2 term3)
  (let ((term1-lhs (cadr term1))
        (term1-rhs (caddr term1))
        (term2-lhs (cadr term2))
        (term2-rhs (caddr term2))
        (term3-lhs (cadr term3))
        (term3-rhs (caddr term3)))
    (cond ((equal term1-lhs term2-lhs)
           (cond ((> (length term1-rhs) (length term2-rhs))
                 (let ((prior term3)
                       (evidence term2)
                       (likelihood term1))
                   (list prior evidence likelihood)))
                 (t (let ((prior term3)
                       (evidence term1)
                       (likelihood term2))
                   (list prior evidence likelihood)))
                 ))
          ((equal term1-lhs term3-lhs)
           (cond ((> (length term1-rhs) (length term3-rhs))
                 (let ((prior term2)
                       (evidence term3)
                       (likelihood term1))
                   (list prior evidence likelihood)))
                (t (let ((prior term2)
                       (evidence term1)
                       (likelihood term3))
                   (list prior evidence likelihood)))
                 ))
          (t (cond ((> (length term2-rhs) (length term3-rhs))
                    (let ((prior term1)
                          (evidence term3)
                          (likelihood term2))
                      (list prior evidence likelihood)))
                   (t (let ((prior term1)
                       (evidence term2)
                       (likelihood term3))
                   (list prior evidence likelihood))))))))
(defun symbol-stats/bayes-rule-to-posterior (terms)
  (cond ((symbol-stats/is-mul terms)
         (cond ((symbol-stats/is-div (cadr terms))
               (let ((term1 (caddr terms))
                     (term2 (cadr (cadr terms)))
                     (term3 (caddr (cadr terms))))
                     (let ((bayes-terms (symbol-stats/get-bayes-terms term1 term2 term3)))
                       (let ((prior (car bayes-terms))
                             (evidence (cadr bayes-terms))
                             (likelihood (caddr bayes-terms)))
                         (symbol-stats/bayes-rule-to-posterior-helper likelihood prior evidence)))))))))

;; create entropy
(defun symbol-stats/entropy (v1 v2) (list 'H v1 v2))
(defun symbol-stats/is-entropy (t1) (eq (car t1) 'H))
(defun symbol-stats/full-term-is-entropy (ft) (symbol-stats/is-entropy (full-term-term ft)))

;; create integrals
(defun symbol-stats/make-int (t1 var) (list '∫ t1 (symbol-stats/symbol-append 'd var)))
(defun symbol-stats/is-int (t1) (equal (car t1) '∫))
(defun symbol-stats/full-term-is-int (ft) (symbol-stats/is-int (full-term-term ft)))
(defun symbol-stats/integrand (t1) (cadr t1))
(defun symbol-stats/differential (t1) (symbol-stats/symbol-end (car (last t1))))

;; expectation
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
(defun symbol-stats/is-expectation (t1) (eq (car t1) 'E))
(defun symbol-stats/full-term-is-expectation (ft) (symbol-stats/is-expectation (full-term-term ft)))

;; marginalization
(defun symbol-stats/marginalize (p var) (symbol-stats/make-int (symbol-stats/make-dist (car p) (cons var (cadr p)) (caddr p)) var))
(defun symbol-stats/can-marginalize (p var) (and (not (symbol-stats/any var (cadr p))) (not (symbol-stats/any var (caddr p))) ))
(defun symbol-stats/can-marginalize-any (p vars) (mapcar #'(lambda (v) (symbol-stats/can-marginalize p v)) vars))
(defun symbol-stats/try-marginalize-one (can-marg-vars vars p) (if (eq can-marg-vars nil) p
                                                                 (if (car can-marg-vars)
                                                                     (symbol-stats/marginalize p (car vars))
                                                                   (symbol-stats/try-marginalize-one (cdr can-marg-vars) (cdr vars) p))))
(defun symbol-stats/try-marginalize (p vars) (let ((can-marg-vars (symbol-stats/can-marginalize-any p vars))) (symbol-stats/try-marginalize-one can-marg-vars vars p)))

;; TODO expectation of score function is 0

;; TODO if ft is (+ t1 t2) then want to return (+ (marginalize t1) t2) if (not (eq (marginalize t1) t1)) and (+ t1 (marginalize t2)) otherwise
(defun symbol-stats/marginalize-full-term (ft)
  (let ((p (full-term-term ft)) (vars (full-term-joint-vars ft)))
      (cond ((symbol-stats/is-dist p) (symbol-stats/full-term-create vars p))
            ((symbol-stats/is-log p) (symbol-stats/full-term-create vars (symbol-stats/log (symbol-stats/try-marginalize (cadr p) vars))))
            ((symbol-stats/full-term-is-add ft)
             (let ((t1-marginalized (symbol-stats/marginalize-full-term (symbol-stats/full-term-cadr ft))))
               (if (not (eq t1-marginalized (cadr p)))
                   (symbol-stats/add t1-marginalized (symbol-stats/full-term-caddr))
                   (let ((t2-marginalized (symbol-stats/marginalize-full-term (symbol-stats/full-term-caddr ft))))
                     (symbol-stats/add t1-marginalized t2-marginalized))
                 )))
            (t ft)
             )))

;; multiplcation by one
(defun symbol-stats/mult-one (t1 p)
  (cond ((symbol-stats/is-int t1) (symbol-stats/make-int (symbol-stats/multiply (symbol-stats/divide (symbol-stats/integrand t1) p) p) (symbol-stats/differential t1)))
        (t t1)
  ))

;; constructor
;; user must specify all random variables in the joint distribution, joint-vars,
;; and the starting term, term
(cl-defstruct (full-term (:constructor full-term-create)
                         (:copier nil))
  joint-vars term)
;; make full terms out of cadr and caddr of ft
(defun symbol-stats/full-term-cadr (ft) (full-term-create :joint-vars (full-term-joint-vars ft) :term (cadr (full-term-term ft))))
(defun symbol-stats/full-term-caddr (ft) (full-term-create :joint-vars (full-term-joint-vars ft) :term (caddr (full-term-term ft))))
(defun symbol-stats/full-term-create (vars p) (full-term-create :joint-vars vars :term p))

;; examples
(setq ft (full-term-create :joint-vars (list 'x 'z) :term (list 'log (symbol-stats/make-dist 'p (list 'x) nil))))
(cadr (full-term-term ft))
(symbol-stats/marginalize-full-term ft)

(setq ftt (full-term-create :joint-vars (list 'x 'z) :term (symbol-stats/add (symbol-stats/make-dist 'p (list 'x) nil) (symbol-stats/make-dist 'p (list 'y) nil))))
(symbol-stats/full-term-cadr ftt)
(symbol-stats/full-term-caddr ftt)

(setq data (symbol-stats/make-dist 'p (list 'x 'h) (list 'y 'z)))
(symbol-stats/are-equal data (symbol-stats/denom (symbol-stats/divide data data)))
(setq I (symbol-stats/marginalize data 'w))
(symbol-stats/is-dist (symbol-stats/integrand I))
(symbol-stats/make-expectation (symbol-stats/multiply data (symbol-stats/log data)) 'q)

(symbol-stats/integrand I)
(symbol-stats/differential I)
(symbol-stats/mult-one I (symbol-stats/make-dist 'q (list 'w) (list 'x 'h)))

(setq pd (symbol-stats/make-dist 'p (list 'x) (list 'z)))
(symbol-stats/try-marginalize pd (list 'y 'z))

(symbol-stats/bayes-rule-from-posterior data (list 'y))

(setq prior (symbol-stats/make-dist 'p (list 'z) (list 'y)))
(setq likelihood (symbol-stats/make-dist 'p (list 'x) (list 'z 'y)))
(setq evidence (symbol-stats/make-dist 'p (list 'x) (list 'y)))
(symbol-stats/bayes-rule-to-posterior-helper likelihood prior evidence)

(symbol-stats/get-bayes-terms evidence likelihood prior)
(symbol-stats/get-bayes-terms evidence prior likelihood)
(symbol-stats/get-bayes-terms likelihood evidence prior)
(symbol-stats/get-bayes-terms likelihood prior evidence)
(symbol-stats/get-bayes-terms prior evidence likelihood)
(symbol-stats/get-bayes-terms prior likelihood evidence)

(symbol-stats/bayes-rule-to-posterior (symbol-stats/multiply (symbol-stats/divide likelihood evidence) prior))
(provide 'symbol-stats)
