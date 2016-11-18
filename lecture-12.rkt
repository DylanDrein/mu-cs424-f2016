;;; Dylan Drein | 13344741
;;; Lecture 12: λ-Calculus

Originally invented as a competitor to the Turing machine, a model of computation
=> Turing Equivalent

Second reason for interest: Also a model of functions.

There was a different way of defining functions before the def we have now:
- Something you can write down/ get your hands on, excludes some funcs that the contemporary set def would allow for
- Something more tangible and concrete (constructivism) vs. the hard, set mathematical definition

Computer programming languages only recognise functions which you can write and define.
Set theoretic definition is focused on the I/O behaviour: Correspondence between the input and outputs. The middle is a sort of black box. (Extention)
Computer recognition of functions is not the same: Intention of a function is important (memory load, runtime etc.

Traditional Calculus allows for reduction

λ-Calc allows for one reduction, Beta reduction:
(λ v . e1) e2 --> [e2/v]e1 ; e2/v means replace instances of v by e2
where variables have to be renamed if necessary

(λ y . ( x y z y)) u
--> [u/y](x y z y) ; beta reduction
---> x u z u

Define sub operator:
[e/v] v = e ; e replaces v in an expression 'v' v is the variable of interest
[e/v] x = x ; x is not the variable of interest, beta reduction not applied
[e/v] (e1 e2) = [e/v]e1 [e/v]e2 ; beta reduction on an application, apply beta reduction individually to the components of that application
[[wrong]] [e/v] (λ x . e1) = (λ x . [e/v]e1) ; B-reduction on lambda expression
     ;;Why it's wrong:
     [g/x](λ x . x y) = λ x . g y ;under false definition above
     ;Should replace free occurrences of the variable of interest

     FV(e) = set of free vars in expression e
     FV(v) = {v}
     FV(e1 e2) = SetUnion(FV(e1) FV(e2))
     FV(λv . e) = FV(e) \ {v}

[[correct]] [e/v](λ v . e1) = (λ v . e1)       ;stop when shadowed
[[wrong]]   [e/v](λ v . e1) = (λ x . [e/v]e1)  ;not shadowed because {v} != {x}
                                               ; rule here is not allowed if x is not an element of FV(e)


            (λ x . x v) x --> [x/x](x v) = [x/x]x [x/x]v = x v

            (λ x . x v) v --> [v/x](x v) = [v/x]x [v/x]v = v v ;right?
            (λ x . (λ y . x)) y --> [y/x](λ y . x) = λ y . [y/x]x = λ y . y ;; local y inside body is not the same as the 'free' outside y **wrong**
                                                                            ;; vay r "captured"
            ; Substituting an exp, must be aware of collisions between local and free variables
            ;Not allowed to do this type of sub. Have to rename local variable out of the way: Alpha-renaming

            alpha-rename variable:
            - Rename occurrences of formal parameter of λ exp.

            eg:
            (λ v . e) --> (λ x . [x/ve]e)  ;where x is not an element of FV(e)

            ;;^ Reductions of λ Calculus

EXAMPLES:
(λ x . x) (λ y . y)
- > (λ y . y)

(λ f . λ g . λ x . f(g x)) (λ y . y)z
= ((λ f . (λ g . (λ x . f(g x)))) (λ y . y))z
-> (λ g . (λ x .(λ y . y) (g x)))z
--> (λ g . (λ x .    (g x)   )z
--->       (λ x .     z x   ) ;; no more subs we can do: we're done.
                              ;; Had a choice to do inner or outer first. If you do both and get different results then system is not CONFLUENT
                              ;; λ-Calculus is CONFLUENT.

Two terms are equal means two terms are equal up to alpha-renaming.
      λ x . a x a = λ x . a y a

PNO Arithmetic(sp?):

    - Zero = λ f x . x = λ f . (λ x . x)
      One = λ f x . f x
      TWO = λ f x . f (f x))
      THREE = λ f x . f (f (f x))   ;; representation of a number n: doing 'something' n-times
      ...

      Succ = λ n . (λ f x . f (n f x))  ; tail recursive on n
      Succ = λ n . (λ f x . n f (f x))  ; alt def, not tail recursive on n
                                        ; equivalent?

      Plus = λ n m . λ f x . m f (n f x)
      Plus = λ n m . λ f x . n f (m f x) ;equivalent?
      Plus λ n m . n Succ m ; apply successor m times to n

      Plus Two Three = (λ n m . λ f x . m f (n f x)) (λ f x . f ( f x)) (λ f x . f (f (f x)))


