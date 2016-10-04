(define ddx
  (λ (e)
    (cond ((number? e) 0)		; d/dx const = 0
	  ((equal? e 'x) 1)		; d/dx x = 1
	  (else
	   (let ((op (car e)))
	     (let ((d-func (lookup op diff-table)))
	       (apply d-func
		      (append (cdr e)
			      (map ddx (cdr e))))))))))

(define diff-table
  (list (list '+ (λ (u v du dv)		; (u+v)' = u' + v'
		   (ssl+ du dv)))
	(list '* (λ (u v du dv)		; (u*v)' = u*v' + u'*v
		   (ssl+ (ssl* u dv) (ssl* du v))))
	(list '- (λ (u du)		; (-u)' = -u'
		   (ssl- du)))
	(list 'sin (λ (u du)		; (sin u)' = (cos u) * u'
		     (ssl* (ssl-cos u) du)))
	(list 'cos (λ (u du)		; (cos u)' = - (sin u) * u'
		     (ssl- (ssl* (ssl-sin u) du))))
	))

(define ssl-cos (λ (e) (list 'cos e)))

(define ssl-sin (λ (e) (list 'sin e)))

(define ddx-nth (λ (e n) (repeat ddx n e)))

(define repeat (λ (f n x)
		 (if (zero? n) x (repeat f (- n 1) (f x)))))

(define ddx-unit-test-table
  '(( (sin (* x x))
      2
      (+ (* (cos (* x x)) 2) (* (- (* (sin (* x x)) (* x 2))) (+ x x))))
    ( (sin (* x x))
      1
      (* (cos (* x x)) (+ x x)))))

(define unit-tests-at
  (λ (x)
    (map (λ (unit-test)
	   (let ((u (car unit-test))
		 (n (cadr unit-test))
		 (du1 (caddr unit-test)))
	     (let ((du2 (ddx-nth u n)))
	       (about= (ssl-eval du1 x)
		       (ssl-eval du2 x)))))
	 ddx-unit-test-table)))

(define about= (λ (x y) (< (abs (- x y)) 1e-6)))

(define lookup
  (λ (key table)
    (cond ((null? table)
	   (error "unknown SLL operator:" key))
	  ((equal? key (caar table))
	   (cadr (car table)))
	  (else (lookup key (cdr table))))))

(define ssl+
  (λ (u v)
    (cond ((equal? u 0) v)
	  ((equal? v 0) u)
	  ((and (number? u) (number? v)) (+ u v))
	  (else (list '+ u v)))))

(define ssl*
  (λ (u v)
    (cond ((equal? u 1) v)
	  ((equal? v 1) u)
	  ((equal? u 0) 0)
	  ((equal? v 0) 0)
	  ((and (number? u) (number? v)) (* u v))
	  (else (list '* u v)))))

(define ssl-
  (λ (u)
    (cond ((number? u) (- u))
	  (else (list '- u)))))

;;; evaluate an SSLx expression at some value of x
(define ssl-eval
  (λ (e x)
    (cond ((number? e) e)
	  ((equal? e 'x) x)
	  (else (let ((f (lookup (car e) ssl-op-table)))
		  (apply f (map (λ (e1) (ssl-eval e1 x)) (cdr e))))))))

(define ssl-op-table
  (list (list '+ (λ (v1 v2) (+ v1 v2)))
	(list '* (λ (v1 v2) (* v1 v2)))
	(list '- (λ (v) (- v)))
	(list 'sin sin)
	(list 'cos cos)))
