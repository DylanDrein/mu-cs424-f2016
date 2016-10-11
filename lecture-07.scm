;;; Add macros to Scheme interpreter.

;;; Option One:

(define eval_ADD-MACROS-ONE-UGLY
  (λ (e env)
    (cond ((symbol? e) (lookup-var e env))
	  ((not (list? e)) e) ; non-list non-symbol is self-evaluatory
	  ;; special forms go here:
	  ((equal? (car e) 'λ)		; (λ VARS BODY)
	   (let ((vars (cadr e)) (body (caddr e)))
	     (list '%closure vars body env)))
	  ((equal? (car e) 'if)
	   (eval_ (if (eval_ (cadr e) env) (caddr e) (cadddr e)) env))
	  ((equal? (car e) 'quote) (cadr e))
	  ;; Add More Macros Here:
	  ((equal? (car e) 'let) xxx)
	  ((equal? (car e) 'cond) xxx)
	  ((equal? (car e) 'and) xxx)
	  ((equal? (car e) 'or) xxx)
	  ;; otherwise must be regular function call
	  (else (let ((eeoe (map (λ (e0) (eval_ e0 env)) e)))
		  (apply_ (car eeoe) (cdr eeoe)))))))

;;; Ick: not modular.

;;; Option Two:

(define eval_ (λ (e env) (eval-core (de-sugar e) env)))

(define eval-core
  (λ (e env)
    (cond ((symbol? e) (lookup-var e env))
	  ((not (list? e)) e) ; non-list non-symbol is self-evaluatory
	  ;; special forms go here:
	  ((equal? (car e) 'λ)		; (λ VARS BODY)
	   (let ((vars (cadr e)) (body (caddr e)))
	     (list '%closure vars body env)))
	  ((equal? (car e) 'quote) (cadr e))
	  ;; No macros here, assume already expanded away.
	  ;; otherwise must be regular function call
	  (else (let ((eeoe (map (λ (e0) (eval-core e0 env)) e)))
		  (apply_ (car eeoe) (cdr eeoe)))))))

(define de-sugar
  (λ (e)
    (cond ((not (pair? e)) e)
	  (else (let ((f (car e)))
		  (cond ((equal? f 'λ)
			 (list 'λ (cadr e) (de-sugar (caddr e))))
			;; macros:
			((assoc f macro-alist)
			 ;; Instead of this:
			 ;;  ((cadr (assoc f macro-alist)) e)
			 ;; can write this:
			 => (λ (x) (de-sugar ((cadr x) e))))
			;; regular function call:
			(else (map de-sugar e))))))))

(define macro-alist
  `((and ,(λ (e)
	    ;; (and A B) -> (if A B #f)
	    ;; (and A B C ...) -> (if A (and B C ...) #f)
	    ;; (and A) -> A
	    ;; (and) -> #t
	    (let ((forms (cdr e)))
	      (cond ((null? forms) '#t)
		    ((null? (cdr forms)) (car forms))
		    (else `(if ,(car forms) (and ,@(cdr forms)) #f))))))
    (or ,error)
    (let ,error)
    (cond ,error)
    (if ,(λ (e) (let ((guard (cadr e))
		      (then-part (caddr e))
		      (else-part (cadddr e)))
		  ;; Sugar to write this more conveniently:
		  ;; (list (list '%if
		  ;; 	       guard
		  ;; 	       (list 'λ '() then-part)
		  ;; 	       (list 'λ '() else-part)))
		  ;; use (quasiquote e), aka: `e
		  ;; (quasiquote ((%if (unquote guard)
		  ;;                   (λ () (unquote then-part))
		  ;;                   (λ () (unquote else-part)))))
		  ;; which itself can be abbreviated:
		  `((%if ,guard (λ () ,then-part) (λ () ,else-part))))))
    ))

;;; Copied from prev lecture:

(define apply_
  (λ (f args)
    (cond ((procedure? f) (apply f args))
	  ((equal? (car f) '%closure)
	   (let ((vars (cadr f))
		 (body (caddr f))
		 (env (cadddr f)))
	     (eval_ body (append (map list vars args) env))))
	  (else (error "error: call to non-procedure" f)))))

(define lookup-var
  (λ (s env)
    (cadr (or (assoc s env)
	      (assoc s global-variable-alist)
	      (error "error: unbound variable" s)))))

(define global-variable-alist
  (list (list 'pi pi)
	(list 'e (exp 1))
	(list '+ +) (list '* *) (list '- -)
	(list 'sin sin)
	(list '%if (λ (g a b) (if g a b)))
	(list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'list list)
	))
