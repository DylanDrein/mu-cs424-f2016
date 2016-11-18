## Lecture 12

## CS424

**Assignment**
 - common bug

 (and #f (/ 1 0)) --> #f

 whereas:

  (and 3 (/ 1 0)) --> error (because 3 isnt false)

  and:

  (or #f 7) --> will return 7 not #t

## last lecture

curry_ and uncurry_

We dont want explicit recursion

Trying to define factorial:

```
  ;; fibonacci defined recursively
  (define fib
    (lambda (n)
        if (<= n 1)
            1
            (+
                (fib (- n 1))
                (fib (- n 2)))))

;; try not to return recursively --> pre-fib
  (define pre-fib
      (lambda (f)
        (lambda (n)
            if (<= n 1)
                1
                (+
                    (f (- n 1))
                    (f (- n 2))))))
```

Note: fib is the (unique) fixed-point of pre-fib. (over the domain of fib)


intuition: sometimes can find the fixedpoint of a numeric function g:R->R by iteration
        g(...g(g(g(x0)))...) often converges to fixed point of g.

error is undefined everywhere in some sense

error is a terrible approx of fibinocci

```
(define iterate
    (lambda (f n)
        (lambda (x)
            if (zero? n)
                x
                (iterate f (- n 1))
                    (f x))))

(((iterate error per-fib 5) error) 4) ;; will be defined up to 5

;; new prefib
(define pre-fib_
    (lambda (f)
            (lambda (n)
                (if (<= n 1)
                    1
                    (+
                        ((pre-fib0 pre-fib0) (- n 1))
                        ((pre-fib0 pre-fib0) (- n 2))))))))))
```

this is tricky but theres a way to write a fixed point finding routine - however its easier to use lambda calculus

## Lambda Calculus

scheme is not quite poor, we havent excercised the non-pure parts

**Alonso Church proposed lambda Calculus**

**lambda calculus is pure** (use lower case lambda)

equivalent computational power to the turing machine

lambda calculus "Term", E:

E ::= V [variable] || lambda V . E [lambda expression] || E E [application]

V ::= x | y | z | a | b | ....

Notation: two terms next to one another is an application of A term to B term
i.e
(A B) application of A to B

- parens used for grouping
- lambda expressions extend as far to the right as possible
- you cant have three terms in an application
- application is "left associative" : x y z = (x y) z
- all lambda expressions have one variable, multivariate is shorthand (sugar) for curied
- lambda x y z . e = lambda x . lambda y . lambda z . e = lambda x . (lambda y . (lambda z . e))

Q: Whye write lambdav.e instead of v->e ?

A: Curches typewriter didnt have unicode

lambda calculus has reduction

- one reduction called beta-reduction (small greek beta)
- capital is a meta variable i.e V
- (lambda v . e1) e2 -> [e2/v]e1 (all instances of v are replaced by e2)
- rules to avoid variable capture, where variables have to be renamed if necessary

