;;; Parenthesis are only used for function calls (not for grouping terms)

;;; Format:
;;; function call: (func args...)

;;; Examples:
;;;		(sin 2)
;;;		(sin (2)) --> Gives an ERROR as 2 is not a function!
;;;		(+ 1 3)


;;; special form lambda: (λ (params...) expr)
;;; special form define: (define global expr)

;;; Examples:

;;; Define a function that takes a number as a parameter and adds 17 to it:
(define a17 (lambda (x) (+ x 17)))		; Use: (a17 3) --> Outputs 20

;;; Define a function to get the hypotenuse:
(define hypot
  (λ (x y)
    (sqrt (+ (* x x)
			 (* y y)))))	; Also note that this is the preferred indentation

;;; Takes two functions as parameters to be ran in sequence on a given input x:
(define compose (λ (f g) (λ (x) (f (g x)))))


;;; special form if: (if GUARD THEN ELSE)

;;; Examples:
;;;		(if (< 1 2) 17 93) --> Read as: If 1 is greater than 2, return 17, else return 93
;;;		Note: If the expression cannot be evaluated to false then the true value gets returned bu default
;;;		E.g.: (if 0 17 93) --> 0 can't be evaluated to false so 17 is returned


;;; The Factorial Function, n! = 1*2*3*...*n
;;;  1! = 1
;;;  n! = n * (n-1)!          [when n>1]

;; (define fact
;;   (λ (n)
;;     (if (= n 1)
;;         1
;;         (* n (fact (- n 1))))))

;; Note that 0! = 1 so will fail for zero even though it's a valid input

(define fact
  (λ (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))	; This can get crazy big answers super quick!!

;;; want: (+ 1 (let t=(sqrt 17) in (* t (exp t))))

;;; do as: (+ 1 ((λ (t) (* t (exp t))) (sqrt 17)))

;;; Sugared: (+ 1 (let ((t (sqrt 17))) (* t (exp t))))

;;; Keyword let, transformed to calling λ expression.
;;;    (let ((var1 val1) (var2 val2) ...) expr)
;;;        =>    ((λ (var1 var2 ...) expr) val1 val2 ...)

;;; Examples:
;;;		(+ 1 (let ((t (sqrt 17))) (* t (exp t))))
;;;		=> let t = sqrt(17), then evaluate: t * (e^t) + 1

;;;		(let ((x 1) (y (+ x 1))) (+ x (* 100 y)))
;;;		=> Error: values are computed before being assigned so cannot use x to evaluate y this way
;;;		(let ((x 1)) (let ((y (+ x 1))) (+ x (* 100 y))))
;;;		=> Two sepearate 'let' statements are needed to fix this


;;; Other notes:

;;; Lists:
;;;		Are loosely typed so can combine contents:
;;;			(list 2 17 (sqrt 3) (zero? 2) sin)
;;;			=> (2 17 1.7320508075688772 #f #<procedure:sin>)
;;;		Empty list:
;;;			null
;;;			(list)
;;;		Construct keyword cons:
;;;			(cons 3)
;;;			=> (3)
;;;			(cons 2 (cons 3 null))
;;;			=> (2 3)
;;;			(cons 1 (cons 2 (cons 3 null)))
;;;			=> (1 2 3)

;;;		car --> returns the first element of a list
;;;			(car (list 4 5 6 7))
;;;			=> 4
;;;		cdr --> returns a list excluding the first element
;;;			(cdr (list 4 5 6 7))
;			=> (5 6 7)