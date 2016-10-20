#lang scheme

;;; Plan:
;;;  1. introduce datatype "symbolic expressions" or sexpr
;;;  2. blow your mind
;;;  3. ?
;;;  4. profit

;;; <SEXPR> ::= <NUMBER>
;;;             | <LIST OF SEXPR>
;;;             | #T | #F
;;;             | <SYMBOL>

;;; ==================== special form
;;; (quote THING)
;;; Shorthand:
;;; 'THING => (quote THING)    - transformed at "read time"
;;; E.g.:
;;;		(quote (1 2)) => '(1 2)

;;; ==================== special form
;;; (or E1 E2 ... En) => evaluates l-to-r, returns 1st non-false value

;;; SLL = silly little language
;;; SLLe = silly little language expression
;;; SLLbinOp = silly little language binary operation
;;; SLLunOp = silly little language unary operation

;;; <SLLe> ::= <NUMBER>
;;;         | (<SLLbinOp> <SLLe> <SLLe>)
;;;         | (<SLLunOp> <SLLe>)
;;; <SLLbinOp> ::= + | *				; addition, OR, multiplication
;;; <SLLunOp> ::= -						; negation (not subtraction!)

;;; Examples of <SLLe>'s
;;;   7, (+ 2 3), (- (* 2 (- 6)))
;;; Negative Examples:
;;;   +, sin, (+ 2 3 4), (+ 2), (- 2 3), (sqrt 4)

(define sll-eval0
  (λ (e)
    (if (number? e)								; If it's a number, return that number
	e
	(if (sll-binop? (car e))					; If it's a binary op, get the expression
	    (if (equal? (car e) '+)
		(+ (sll-eval (cadr e))
		   (sll-eval (caddr e)))
		(if (equal? (car e) '*)
		    (* (sll-eval (cadr e))
		       (sll-eval (caddr e)))
		    (error "unknown binary op" (car e))))
	    (if (equal? (car e) '-)
		(- (sll-eval (cadr e)))
		(error "unknown unary op" (car e)))))))

;;; ================ Special Form
;;; (cond (t1 r1) (t2 r2) (t3 r3) (else r4))
;;;   => (if t1 r1 (if t2 r2 (if t3 r3 r4)))

(define sll-eval1
  (λ (e)
    (cond ((number? e) e)
	  ((sll-binop? (car e))
	   (cond ((equal? (car e) '+)
		  (+ (sll-eval (cadr e))
		     (sll-eval (caddr e))))
		 ((equal? (car e) '*)
		  (* (sll-eval (cadr e))
		     (sll-eval (caddr e))))
		 (else (error "unknown binary op" (car e)))))
	  ((sll-unop? (car e))
	   (cond ((equal? (car e) '-)
		  (- (sll-eval (cadr e))))
		 (else (error "unknown unary op" (car e))))))))

;;; Dead:

(define sll-binop?
  (λ (s)
    (or (equal? s '+) (equal? s '*))))

;;; Dead:

(define sll-unop?
  (λ (s)
    (or (equal? s '-))))

(define sll-eval				; Final form, doesn't use sll-binop? or sll-unop?
  (λ (e)
    (cond ((number? e) e)
	  ((equal? (car e) '+)
	   (+ (sll-eval (cadr e))
	      (sll-eval (caddr e))))
	  ((equal? (car e) '*)
	   (* (sll-eval (cadr e))
	      (sll-eval (caddr e))))
	  ((equal? (car e) '-)
	   (- (sll-eval (cadr e))))
	  (else (error "unknown SLL operator:" (car e))))))

;;; Define SLLX, same as SLL except
;;;    SLLXe ::= x
;;; the symbol x is a valid SLLX expression

;;; Examples:
;;;   x, (+ 3 x), (* (+ x 9) (+ x (- 9))), ...
;;; Negative Examples:
;;;  (* (+ x 9) (- x 9)), ...

;;; Take symbolic derivative of SSLX expression wrt x, yielding SSLX expression
(define ddx0
  (λ (e)
    (cond ((number? e) 0)		; d/dx const = 0
	  ((equal? e 'x) 1)		; d/dx x = 1
	  ((equal? (car e) '+)		; d/dx (u+v) = d/dx u + d/dx v
	   (list '+
		 (ddx0 (cadr e))
		 (ddx0 (caddr e))))
	  ((equal? (car e) '*)		; d/dx (u*v) = u*v' + u'*v
	   (list '+
		 (list '*
		       (cadr e)
		       (ddx0 (caddr e)))
		 (list '*
		       (ddx0 (cadr e))
		       (caddr e))))
	  ((equal? (car e) '-)
	   (list '- (ddx0 (cadr e))))
	  (else (error "unknown SLL operator:" (car e))))))

;;; be a little careful to produce non-ugly output when easy

(define ddx
  (λ (e)
    (cond ((number? e) 0)		; d/dx const = 0
	  ((equal? e 'x) 1)		; d/dx x = 1
	  ((equal? (car e) '+)		; d/dx (u+v) = d/dx u + d/dx v
	   (ssl+ (ddx (cadr e)) (ddx (caddr e))))
	  ((equal? (car e) '*)		; d/dx (u*v) = u*v' + u'*v
	   (ssl+ (ssl* (cadr e) (ddx (caddr e)))
		 (ssl* (ddx (cadr e)) (caddr e))))
	  ((equal? (car e) '-)
	   (ssl- (ddx (cadr e))))
	  (else (error "unknown SLL operator:" (car e))))))

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
