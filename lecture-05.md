# Lecture 5 notes

First we define our basic derivative function. This contains an anonymous/lambda function which takes e as a parameter.
Then we check if the expression e is a number, if so it's derivative is 0
then we check if it's x to the power 1 which is differentiated to 1
else we define a variable op which is the start of our expression, which should be an operator as our expressions are written in polish notation
we then take op and search for it in our function diff-table declared further down. This returns the function differentiated.
we then apply the function to the rest of the input (as in not the operator we differentiated). We also supply the rest of the input differentiated as some functions derivatives require these values. (e.g. (d/dx) sin(u) = cos(u)* (d/dx)u')
````
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
````

Now we define our table of function derivatives.
This boils down to a list of functions, each element being simply a list with the operation first and the value after the operation second.
````
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
````
Now we define the trigonometric operations which return a list of format (operation value)
````
(define ssl-cos (λ (e) (list 'cos e)))

(define ssl-sin (λ (e) (list 'sin e)))
````

Now we allow differentiating multiple times using the repeat function below. This function just calls repeat (essentially just renames it)
````
(define ddx-nth (λ (e n) (repeat ddx n e)))
````

The repeat function recursively calls itself passing in the differentiating function, n decreased by 1 and the expression after it's differentiated until n==0 (as then it's been differentiated n times)
```
(define repeat (λ (f n x)
		 (if (zero? n) x (repeat f (- n 1) (f x)))))
```

Here we define some unit tests. These unit tests compare the input and the output to our ddx function. These format of unit testing however does not work if we simplified our expression further. For example if we output (* 3 x) but this table said (+ x (+ x x)) then even though both expressions mean 3x we would fail the unit test.
````
(define ddx-unit-test-table
  '(( (sin (* x x))
      2
      (+ (* (cos (* x x)) 2) (* (- (* (sin (* x x)) (* x 2))) (+ x x))))
    ( (sin (* x x))
      1
      (* (cos (* x x)) (+ x x)))))
````

Instead we can try to evaluate our derivative at a point as say for the example above (+ 2 (+ 2 2)) => 6 and (* 3 2) => 6
This is a function that takes in an x value, with another function inside that takes in a variable called unit-test.
Then we make some variable definitions. u = start of unit-test (the first expression, undifferentiated), n = number of times to differentiate, du1 = the second expression (differentiated).
Then we set du2 to the nth derivative of u.
Then we use about= to check the two derivatives du1, du2 are approximately equal at point x
````
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
````

The about functions checks if two values are roughly equal (as in the absolute difference is less than 0.000001 which should account for floating point errors)
````
(define about= (λ (x y) (< (abs (- x y)) 1e-6)))
````

Here we search a table for a key(operation). If the table is empty we give an error message.
Otherwise we check if the operation is the first element and return it if it is,
else we run lookup on the rest of the list.
````
(define lookup
  (λ (key table)
    (cond ((null? table)
	   (error "unknown SLL operator:" key))
	  ((equal? key (caar table))
	   (cadr (car table)))
	  (else (lookup key (cdr table))))))
````


ssl+ and ssl* just define addition and multiplication in our silly little language.
````
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
````
ssl-eval just evaluates our expressions.
If the expression is just a number it is returned immediately. if the expression is just x then we just return that.
Otherwise we define
````
;;; evaluate an SSLx expression at some value of x
(define ssl-eval
  (λ (e x)
    (cond ((number? e) e)
	  ((equal? e 'x) x)
	  (else (let ((f (lookup (car e) ssl-op-table)))
		  (apply f (map (λ (e1) (ssl-eval e1 x)) (cdr e))))))))
````

The ssl-op-table just defines some mathematical operations on our silly little language (we just rename them with a ' before the function name)
````
(define ssl-op-table
  (list (list '+ (λ (v1 v2) (+ v1 v2)))
	(list '* (λ (v1 v2) (* v1 v2)))
	(list '- (λ (v) (- v)))
	(list 'sin sin)
	(list 'cos cos)))
````
