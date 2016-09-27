;;; Scheme datatype: LIST

;;; cons, car, cdr
;;; bonus: c[ad]+r

;;; Algebraic rules of lists, i.e., AXIOMS of list operations:
;;;  (car (cons X _)) = X
;;;  (cdr (cons _ Y)) = Y

(define cons_ (λ (x xs) (λ (z) (if z x xs))))
(define car_ (λ (xs) (xs #t)))
(define cdr_ (λ (xs) (xs #f)))

(define cons_1 (λ (x xs) (λ (z) (z x xs))))
(define car_1 (λ (lis) (lis (λ (x xs) x))))
(define cdr_1 (λ (lis) (lis (λ (x xs) xs))))

;;; (append_ () ys) = ys
;;; (append_ (cons x xs) ys) = (cons x (append_ xs ys))
(define append_
  (λ (xs ys)
    (if (null? xs)
	ys
	(cons (car xs) (append_ (cdr xs) ys)))))

;;; (reverse_ EMPTY-LIST) = EMPTY-LIST

(define reverse_
  (λ (xs)
    (if (null? xs)
	xs
	(append (reverse_ (cdr xs))
		(list (car xs))))))

(define nth
  (λ (xs i)
    (if (zero? i)
	(car xs)
	(nth (cdr xs) (- i 1)))))

(define map_
  (λ (f xs)
    (if (null? xs)
	xs
	(cons (f (car xs)) (map_ f (cdr xs))))))

(define filter_
  (λ (p? xs)
    (if (null? xs)
	xs
	(if (p? (car xs))
	    (cons (car xs) (filter_ p? (cdr xs)))
	    (filter_ p? (cdr xs))))))

;;; GET RID OF HORRIBLE REPEATED CODE!!!

(define filter_1
  (λ (p? xs)
    (if (null? xs)
	xs
	(let ((x (car xs))
	      (filtered-cdr (filter_1 p? (cdr xs))))
	  (if (p? x)
	      (cons x filtered-cdr)
	      filtered-cdr)))))

(define filter_2
  (λ (p? xs)
    (if (null? xs)
	xs
	(append (if (p? (car xs))
		    (list (car xs))
		    (list))
		(filter_2 p? (cdr xs))))))
