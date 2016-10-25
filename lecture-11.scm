;;; Curried functions:

;; f is not curried, call as (f x y)
;; f is curried, call as ((f x) y)

(define curry_ (λ (f) (λ (x) (λ (y) (f x y)))))
(define uncurry_ (λ (f) (λ (x y) ((f x) y))))

(define fib
  (λ (n)
    (if (<= n 1)
	1
	(+ (fib (- n 1))
	   (fib (- n 2))))))

(define pre-fib
  (λ (f)
    (λ (n) (if (<= n 1) 1 (+ (f (- n 1)) (f (- n 2)))))))

;;; Note: fib is the (unique) fixed-point of pre-fib.  (Over the domain of fib.)

;;; Intuition: sometimes can find fixedpoint of a numeric function g:R->R by iteration.
;;;   g(... g(g(g(x0))) ...)    often converges, and if so converges to a fixedpoint of g.

(define iterate (λ (f n) (λ (x) (if (zero? n) x ((iterate f (- n 1)) (f x))))))

(define pre-fib_
  (λ (pre-fib0)
    (λ (n)
      (if (<= n 1)
	  1
	  (+ ((pre-fib0 pre-fib0) (- n 1))
	     ((pre-fib0 pre-fib0) (- n 2)))))))

(define fib_ (pre-fib_ pre-fib_))

;;; Lets Do Lambda λ-Calculus!!!

;;; in text file ...
