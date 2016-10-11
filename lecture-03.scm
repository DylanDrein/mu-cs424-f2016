#lang racket/base

;;; Scheme datatype: LIST

;;; cons, car, cdr
;;; bonus: c[ad]+r	--> cadr gets the car of the cdr, i.e. the second element in the list

;;; Algebraic rules of lists, i.e., AXIOMS of list operations:
;;;  (car (cons X _)) = X
;;;  (cdr (cons _ Y)) = Y

;;; Creates a pair of objects to be passed into car_ and cdr_ (Not executable itself)
(define cons_ (λ (x xs) (λ (z) (if z x xs))))
	;; x is the first element
	;; xs is everything else in pairs

;;; Returns the first element of the pair
(define car_ (λ (xs) (xs #t)))

;;; Returns the second element of the pair
(define cdr_ (λ (xs) (xs #f)))

(define cons_1 (λ (x xs) (λ (z) (z x xs))))

(define car_1 (λ (lis) (lis (λ (x xs) x))))

(define cdr_1 (λ (lis) (lis (λ (x xs) xs))))

;;; (append_ () ys) = ys
;;; (append_ (cons x xs) ys) = (cons x (append_ xs ys))
(define append_
  (λ (xs ys)
    (if (null? xs)			; if xs is empty just return ys
        ys
        (cons (car xs) (append_ (cdr xs) ys)))))
		;; else take car of xs and append_ the rest of xs and ys

;;; (reverse_ EMPTY-LIST) = EMPTY-LIST

(define reverse_
  (λ (xs)
    (if (null? xs)			; if xs is empty just return it
        xs
        (append (reverse_ (cdr xs))
                (list (car xs))))))
				;; else keep taking the first element and appending it to the end

(define nth			; returns the element at a position in the list
  (λ (xs i)
    (if (zero? i)
        (car xs)
        (nth (cdr xs) (- i 1)))))
;;;  note: no defensive programming -> index assumed to be valid

(define map_			; applies the function f to each element in the list xs
  (λ (f xs)
    (if (null? xs)
        xs
        (cons (f (car xs)) (map_ f (cdr xs))))))

;;; filter removes elemenets from a list that do not pass some check
;;; e.g.: (filter_ (λ (x) (>= x 0)) (list 1 -2 3 -4 5 -6 0 -0 17 -9))
;;; removes all negative numbers from the list
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
