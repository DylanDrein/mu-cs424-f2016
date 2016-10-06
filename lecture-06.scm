;;; A Meta-Circular Interpreter in Scheme

(define eval_
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
	  ;; otherwise must be regular function call
	  (else (let ((eeoe (map (λ (e0) (eval_ e0 env)) e)))
		  (apply_ (car eeoe) (cdr eeoe)))))))

;;; Could get away w/o (if g t e) special form by expanding syntactic sugar:
;;; (define %if (λ (g t e) (if g t e)))
;;; (if G T E)   ==>   ((%if G (λ () T) (λ () E)))

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
	(list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'list list)
	))
