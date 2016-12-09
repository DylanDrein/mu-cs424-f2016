#lang racket

;;; Dylan Drein 13344741

;;; CPS Conversion (cont)

;;; define [e]c = CPS convert e to call continuation c
;;;    [2]c -> (c 2)
;;;    [v]c -> (c v)
;;;    [(λ (v...) body)]c -> (λ (cc v...) [body]cc)
;;;    [(f e)]c -> [e](λ (er) [f](λ (fr) (fr c ef) ))
;;;    [(if g tp ep)]c -> [g](λ (gr) (if gr [tp]c [ep]c))

;;; convert scheme expression to CPS, allowed constructs: numbers, variables, λ expr, if, calls
(define to-cps
  (λ (e cont)
    (cond ((number? e) `(,cont ,e))
	  ((symbol? e) `(,cont ,e))
	  ((equal? (car e) 'λ)		; e = (λ (VAR...) BODY)
	   (let ((vars (cadr e))
		 (body (caddr e))
		 (cv (gensym 'c)))
	     `(λ (,cv ,@vars) ,(to-cps body cv))))
	  ((equal? (car e) 'if)
	   (let ((guard (cadr e))
		 (then-part (caddr e))
		 (else-part (cadddr e))
		 (gv (gensym 'guard)))
	     (to-cps guard
		     `(λ (,gv) (if ,gv
				  ,(to-cps then-part cont)
				  ,(to-cps else-part cont))))))
	  ((= (length e) 1)	; (fe), call with 0 args
	   (let ((fe (car e))
		 (fv (gensym 'f)))
	     (to-cps fe `(λ (,fv) (,fv ,cont)))))
	  ((= (length e) 2)	; (fe arg), call with 1 arg
	   (let ((fe (car e))
		 (ae (cadr e))
		 (fv (gensym 'f))
		 (av (gensym 'arg)))
	     (to-cps ae `(λ (,av)
			   ,(to-cps fe `(λ (,fv) (,fv ,cont ,av)))))))
	  ((= (length e) 3)	; (fe arg1 arg2), call with 1 arg
	   (let ((fe (car e))
		 (ae1 (cadr e))
		 (ae2 (caddr e))
		 (fv (gensym 'f))
		 (av1 (gensym 'arg))
		 (av2 (gensym 'arg)))
	     (to-cps ae1
		     `(λ (,av1)
			,(to-cps ae2
				 `(λ (,av2)
				    ,(to-cps fe
					     `(λ (,fv) (,fv ,cont ,av1 ,av2)))))))))
	  (else
	   (let ((fe (car e)) (arges (cdr e))) ; (fe a1 a2 ... an)
	     (error "forgot to handle >2 args" e))))))
	  

(define cps-primop-1 (λ (f) (λ (c a) (c (f a)))))
(define cps-primop-2 (λ (f) (λ (c a b) (c (f a b)))))
(define c= (cps-primop-2 =))
(define c- (cps-primop-2 -))
(define c* (cps-primop-2 *))
(define c+ (cps-primop-2 +))
(define csqrt (cps-primop-1 sqrt))

(define hypot-e '(λ (x y) (csqrt (c+ (c* x x) (c* y y)))))

;;; output would be prettier if we applied this transformation in appropriate places:
;;; ((λ (v) body) u)    ->   [v->u]body

;;; Curried functions:

;; f is not curried, call as (f x y)
;; f is curried, call as ((f x) y)

(define curry_ (λ (f) (λ (x) (λ (y) (f x y)))))
(define uncurry_ (λ (f) (λ (x y) ((f x) y))))

;;; Let's try to define fact without explicit recursion

(define pre-fact
  (λ (f)
    (λ (n)
      (if (= n 0) 1 (* n (f (- n 1)))))))

(define factorial (λ (n) (if (= n 0) 1 (* n (factorial (- n 1))))))


(define fib
  (λ (n)
    (if (<= n 1)
        1
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define pre-fib
  (λ (f)
    (λ (n)
      (if (<= n 1)
          1
          (+ (f (- n 1))
             (f (- n 2)))))))

;;; Note: fib is the (unique) fixed point of pre fib. (Over the domain of fib.)
;;; Intuition: sometimes can fine fized point of a numeric function g:R ->R by iteration
;    g(... g(g(g(xo))) ...) often converges to a fixed point of g.

(define iterate (λ (f n)
                  (λ (x)
                    (if (zero? n) x
                        (( iterate f (- n 1)) (f x))))))



