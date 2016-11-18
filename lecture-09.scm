;;; Continuation-Passing Style = CPS

;;; Write a regular code to CPS transformer

;;; CPS = not allowed to make non-tail calls

;;; In CPS:
;;;  - every function takes an extra argument which is what to do with the result.
;;;  - instead of returning value, call continuation on value to be returned
;;;  - as a consequence of the above, all intermediate results are named
;;;  - as a consequence of the above, order of subcomputations is made explicit

(define fact (λ (n) (if (= n 0) 1 (* n (fact (- n 1))))))

(define cfact (λ (c n)
		(c= (λ (nz)
		      (if nz
			  (c 1)
			  (c- (λ (nm1)
				(cfact (λ (fnm1)
					 (c* c n fnm1))
				       nm1))
			      n 1)))
		    n 0)))

(define cps-primop-1 (λ (f) (λ (c a) (c (f a)))))
(define cps-primop-2 (λ (f) (λ (c a b) (c (f a b)))))
(define c= (cps-primop-2 =))
(define c- (cps-primop-2 -))
(define c* (cps-primop-2 *))
(define c+ (cps-primop-2 +))
(define csqrt (cps-primop-1 sqrt))

(define hypot (λ (x y) (sqrt (+ (* x x) (* y y)))))

(define chypot1 (λ (c x y)
		  (c* (λ (x2) (c* (λ (y2) (c+ (λ (x2py2) (csqrt c x2py2))
					      x2 y2))
				  y y))
		      x x)))

(define chypot2 (λ (c x y)
		  (c* (λ (y2) (c* (λ (x2) (c+ (λ (x2py2) (csqrt c x2py2))
					      x2 y2))
				  x x))
		      y y)))

;;; define [e]c = CPS convert e to call continuation c
;;;    [2]c -> (c 2)
;;;    [v]c -> (c v)
;;;    [(λ (v) e)]c -> (λ (cc v) [e]cc)
;;;    [(f e)]c -> [e](λ (er) [f](λ (fr) (fr c ef) ))
;;;    [(if g tp ep)]c -> [g](λ (gr) (if gr [tp]c [ep]c))

;;; convert scheme expression to CPS, allowed constructs: numbers, variables, λ expr, if, calls
(define to-cps
  (λ (e cont)
    (cond ((number? e) `(,cont ,e))
	  ((symbol? e) `(,cont ,e))
	  ((equal? (car e) 'λ) xxx)
	  ((equal? (car e) 'if)
	   (let ((guard (cadr e)) (then-part (caddr e)) (else-part (cadddr e)))
	     (to-cps guard
		     `(λ (gr) (if gr
				  ,(to-cps then-part cont)
				  ,(to-cps else-part cont))))))
	  (else
	   ;; must be call
	   xxx))))
	
