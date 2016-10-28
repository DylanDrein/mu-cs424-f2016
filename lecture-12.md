#Lambda Calculus 

Lambda claculus was the first turing competitor, it is a model of both calulation and functions.
Adopted a new move in mathematics called Construction 
Constructivism asserts that it is necessary to find (or "construct") a mathematical object to prove
that it exists. When one assumes that an object does not exist and derives a contradiction from that
assumption, one still has not found the object and therefore not proved its existence, according to 
constructivism. This viewpoint involves a verificational interpretation of the existence quantifier,
which is at odds with its classical interpretation.


#### Reductions
Beta reduction stays the same, just drops the type.

(λv:t . e<sub>1</sub>) e<sub>2</sub> ↝ [v ↦ e<sub>2</sub>] e<sub>1</sub>

This implies *The Erasure Theorem*: that any well-typed term in the simply typed lambda calculus will reduce to the same thing if you drop the types and reduce it in the untyped lambda calculus.

We also need to add reductions for the newly introduced basis objects. Like

plus 2 3 ↝ 5<br>
isZero 2 ↝ false<br>
...
Beta-reduction captures the idea of function application. Beta-reduction is defined in terms of substitution: the beta-reduction of  ((λV.E) E′)  is E[V := E′].

For example, assuming some encoding of 2, 7, ×, we have the following β-reduction: ((λn.n×2) 7) → 7×2.

#### Arithmetic in lambda calculus
There are several possible ways to define the natural numbers in lambda calculus, but by far the most common are the Church numerals, which can be defined as follows:

0 := λf.λx.x
1 := λf.λx.f x
2 := λf.λx.f (f x)
3 := λf.λx.f (f (f x))
and so on. Or using the alternative syntax presented above in Notation:

0 := λfx.x
1 := λfx.f x
2 := λfx.f (f x)
3 := λfx.f (f (f x))
A Church numeral is a higher-order function—it takes a single-argument function f, and returns another single-argument function. The Church numeral n is a function that takes a function f as argument and returns the n-th composition of f, i.e. the function f composed with itself n times. This is denoted f(n) and is in fact the n-th power of f (considered as an operator); f(0) is defined to be the identity function. Such repeated compositions (of a single function f) obey the laws of exponents, which is why these numerals can be used for arithmetic. (In Church's original lambda calculus, the formal parameter of a lambda expression was required to occur at least once in the function body, which made the above definition of 0 impossible.)

We can define a successor function, which takes a number n and returns n + 1 by adding another application of f,where '(mf)x' means the function 'f' is applied 'm' times on 'x':

SUCC := λn.λf.λx.f (n f x)
Because the m-th composition of f composed with the n-th composition of f gives the m+n-th composition of f, addition can be defined as follows:

PLUS := λm.λn.λf.λx.m f (n f x)
PLUS can be thought of as a function taking two natural numbers as arguments and returning a natural number; it can be verified that

PLUS 2 3
and

5
are β-equivalent lambda expressions. Since adding m to a number n can be accomplished by adding 1 m times, an equivalent definition is:

PLUS := λm.λn.m SUCC n [21]
Similarly, multiplication can be defined as

MULT := λm.λn.λf.m (n f)[16]
Alternatively

MULT := λm.λn.m (PLUS n) 0
since multiplying m and n is the same as repeating the add n function m times and then applying it to zero. Exponentiation has a rather simple rendering in Church numerals, namely

POW := λb.λe.e b
The predecessor function defined by PRED n = n − 1 for a positive integer n and PRED 0 = 0 is considerably more difficult. The formula

PRED := λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
can be validated by showing inductively that if T denotes (λg.λh.h (g f)), then T(n)(λu.x) = (λh.h(f(n−1)(x))) for n > 0. Two other definitions of PRED are given below, one using conditionals and the other using pairs. With the predecessor function, subtraction is straightforward. Defining

SUB := λm.λn.n PRED m,
SUB m n yields m − n when m > n and 0 otherwise.