;;; A Meta-Circular Interpreter in Scheme

(define eval_
  (λ (e env)
    (cond ((number? e) e)
	  ((boolean? e) e)
	  ((symbol? e) (lookup-var e env))
	  ;; special forms go here:
	  ((equal? (car e) 'λ) e)	; (λ VARS BODY)
	  ((equal? (car e) 'quote) (cadr e))
	  ;; otherwise must be regular function call
	  (else (let ((eeoe (map (λ (e0) (eval_ e0 env)) e)))
		  (apply_ (car eeoe) (cdr eeoe)))))))

(define apply_
  (λ (f args)
    (cond ((procedure? f) (apply f args))
	  (else (let ((vars (cadr f))
		      (body (caddr f)))
		  (eval_ body (map list vars args)))))))

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
