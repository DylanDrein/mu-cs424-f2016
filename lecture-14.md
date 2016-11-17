# Simply Typed Lambda Calculus

λ calculus = untyped<br>
λ<sup>→</sup> calculus = simply Typed

We want to add types to λ Calculus.

To achieve this we will need to:
 * define types via a grammar
 * augment the λ calculus grammar so that it accepts types
 * have some way of inferring the type of an expression

### Defining types

Our λ calculus grammar looks like this currently:
>  E ::= V | E E | λ V . E

The simplest starting point we have is to create a grammar for the primitives we are familiar with.

> τ :: = τ<sub>1</sub> | τ<sub>2</sub> | τ<sub>3</sub> | ...

Where τ<sub>1</sub> might be int, τ<sub>3</sub> could be boolean.
**Note:** There is a finite number of these primitives  

We also need to be able to talk about function types,
for instance a function that takes an int and returns an int

> τ :: = τ<sub>1</sub> | τ<sub>2</sub> | τ<sub>3</sub> | ... | τ → τ

We now need to define a basis for an int, char and bool.

We can add the basis to our grammar quite easily.
>  E ::= V | E E | λ V . E | B

Where B is:
> B ::= 1 | 2 | 3 | ... | 3349786 | 3349787 | ... |
      'A' | 'B' | 'C' | ... |
      true | false

This is our 1st order basis. But how do we manipulate these? We can't add or check equality. They are frozen in expressions

We can add basis functions
> B ::= 1 | 2 | 3 | ... | 3349786 | 3349787 | ... |
>   'A' | 'B' | 'C' | ... |
>   true | false |<br />
>   not | and | or |
>   (+) | (*) | (-) | zero?

not, and, etc are conditions, and (+), zero?, etc are operators

### Changing the grammar

We can now write expressions such as
> 3 7 false

or 

> true true

These do not make any sense, but are still valid.

#### Type Environment

We will define a type environment as a mapping from terms to types.
This partial mapping dict will be denoted by a capital gamma Γ with each entry (E → τ) will represented as E: τ

> Γ = {
    1: int, 2: int, ...,<br />
    'A': char, 'B': char, ...<br />
    true: bool, false: bool,<br />
    not: bool → bool, and: bool → bool → bool,<br />
    (+): int → int → int, zero?: int → bool}

**Note:** As we can only have 1 arg, functions like and or (+) need to be curried

Now we can ask ourselves, what does correctly typed mean? Can we infer the type of an expression from subexpressions?

#### Typing Rules

We will borrow notation from the [Curry-Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence "Wiki link to Curry-Howard Isomorphism")

We write `Γ, v:τ` for the type environment Γ augmented with a mapping from v to τ.
We use `Γ ⊢ E:τ` for the logical statement that, in the type environment Γ it can be shown that the term E has type τ.
As in predicate calculus, we write antecedents or premises above a horizontal line and consequences or conclusions below.

Example
```
V:τ ϵ Γ
--------
Γ ⊢ V:τ
```
This type environment concludes that the type of V is τ

How do we deal with E E or λ V . E?

##### E E

```
Γ ⊢ E1:τ2 → τ1,  E2:τ2
-----------------------
Γ ⊢ E1 E2:τ1
```

##### λ V . E

```

-----------------------
Γ ⊢ λ V . E:τ1 → τ2
```

V needs to be of type τ<sub>1</sub>.
In order to do this we need to modify our lambda expression.

>  E ::= V | E E | λ V:τ . E | B

**Note:** We only need to define type when we introduce a new variable.

Let's try:

```
Γ ⊢ E:τ2
--------------------
Γ ⊢ λ V . E:τ1 → τ2
```

but we don't know about the type of V, so we have to update our type environment to include it

```
{V:τ1} ∪ Γ ⊢ E:τ2
-----------------------
Γ ⊢ λ V . E:τ1 → τ2
```

Now we can do type inference!

#### Inferring types

An expression in λ calculus

> λ x . λ y . ((+) ((((\*) x) x) (((\*) y) y)))

In λ<sup>→</sup> calculus we need to type everything, we can't infer the variable type

```
?
-------------------------------------------------------------------
λ x:int . λ y:int . ((+) ((((*) x) x) (((*) y) y))):int → int → int
```

We need to show (with some syntax sugar to make it more readible)

```
{x:int} ∪ Γ ⊢ λ y:int . (+) ((*) x x) ((*) y y):int → int
-----------------------------------------------------------
λ x:int . λ y:int . (+) ((*) x x) ((*) y y):int → int → int
```

In order to infer the final type. We can keep going

```
{y:int} ∪ {x:int} ∪ Γ ⊢ (+) ((*) x x) ((*) y y):int
----------------------------------------------------------
{x:int} ∪ Γ ⊢ λ y:int . (+) ((*) x x) ((*) y y):int → int
```

We have two things to show now:
```
{y:int} ∪ {x:int} ∪ Γ ⊢ (+) ((*) x x):int → int, {y:int} ∪ {x:int} ∪ Γ ⊢ (*) y y:int
----------------------------------------------------------------------------------------
{y:int} ∪ {x:int} ∪ Γ ⊢ (+) ((*) x x) ((*) y y):int
```

```
{y:int} ∪ {x:int} ∪ Γ ⊢ (+):int → int → int
-------------------------------------------------
{y:int} ∪ {x:int} ∪ Γ ⊢ (+) ((*) x x):int → int
```
Since (+):int → int → int ϵ Γ we are done in this branch

On the other side:
```
{y:int} ∪ {x:int} ∪ Γ ⊢ (*) x:int → int, {y:int} ∪ {x:int} ∪ Γ ⊢ x:int
--------------------------------------------------------------------------
{y:int} ∪ {x:int} ∪ Γ ⊢ ((*) x x):int → int
```
The second part is in the type environment, so it is done

```
{y:int} ∪ {x:int} ∪ Γ ⊢ (*):int → int, {y:int} ∪ {x:int} ∪ Γ ⊢ x:int
------------------------------------------------------------------------
{y:int} ∪ {x:int} ∪ Γ ⊢ (*) x:int → int
```
Both parts are true in the type environment, so we have shown `{y:int} ∪ {x:int} ∪ Γ ⊢ (*) x:int → int`

We have now shown all the branches are true in the type environment, so we have shown
```
λ x:int . λ y:int . ((+) ((((*) x) x) (((*) y) y))):int → int → int
```

#### Conclusions

In λ calculus we could write code that does not terminate,
however in λ<sup>→</sup> calculus we cannot.

This means we can't write non-terminating programs.
It also it is not Turing complete.

The above proof looks like predicate logic.