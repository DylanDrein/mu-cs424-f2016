;;; function call: (func args...)
;;; special form lambda: (λ (params...) expr)
;;; special form define: (define global expr)

(define a17 (lambda (x) (+ x 17)))

(define hypot
  (λ (x y)
    (sqrt (+ (* x x)
	     (* y y)))))

(define compose (λ (f g) (λ (x) (f (g x)))))

;;; special form if: (if GUARD THEN ELSE)


;;; The Factorial Function, n! = 1*2*3*...*n
;;;  1! = 1
;;;  n! = n * (n-1)!          [when n>1]

;; (define fact
;;   (λ (n)
;;     (if (= n 1)
;;         1
;;         (* n (fact (- n 1))))))

;; Note that 0! = 1

(define fact
  (λ (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

;;; want: (+ 1 (let t=(sqrt 17) in (* t (exp t))))

;;; do as: (+ 1 ((λ (t) (* t (exp t))) (sqrt 17)))

;;; Sugared: (+ 1 (let ((t (sqrt 17))) (* t (exp t))))

;;; Keyword let, transformed to calling λ expression.
;;;    (let ((var1 val1) (var2 val2) ...) expr)
;;;        =>    ((λ (var1 var2 ...) expr) val1 val2 ...)
