#lang racket

;;; Dylan Drein 13344741

;;; Lambda Calculus
;;; function from a variable to a value
;;; input to output

λ Calculus "Term", E

e ::= v            [where v is a variable]
      | λ v . e    [Lambda expression]
      | E E        [Application]

V ::= x | y | z | a | b

Notation:
- parentheses used for grouping, not used for function call
- λ expressions extend as far to the right as possible ;*
(λ x . x x) y ; application of first expression to the second (the y)
(λ x . x x) y = λ x . (x x y) ;*

- application is left associative:
e1 e2 e3 = (e1 e2) e3

- all λ expressions have one variable.
  multivariable is shorthand (sugar) for curried
λ x y z . e = λ x . λ y . λ z . e = λ x . (λ y .(λ z . e))

In differential calculus, have reduction:
(d/dx)(u*v) --> (u)*(d/dx)(v) + (d/dx)(u)*(v)

In λ calc, one reduction, called beta-reduction
  (λ v . e1) e2 --> [e2/v]e1   (where [e2/v] means replace instances of v by e2)

where variables have to be renamed if necessary