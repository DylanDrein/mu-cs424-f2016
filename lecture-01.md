# Lecture One

Course materials: http://github.com/barak/mu-cs424-f2016

## Introduction

### Course Coverage

Languages
* Scheme (a pure subset)
* haskell
* prolog (so cool, mind bending)
* lambda calculus

### History/Mindset of Programming Languages

Originally it was believed that hardware was the difficult part of computer programming and that software would be straightforward.  
One of the first computers was Charles Babbages [Difference Engine](http://www.computerhistory.org/babbage/engines/) made in the 1800's without electricity, using cogs and widgets. It didn't work as it used numbers in base 10 not 2 and had 20 digit precision
[Ada Lovelace](https://en.wikipedia.org/wiki/Ada_Lovelace) realized programming is fun

### First Electronic computer
[Konrad Zuse](https://en.wikipedia.org/wiki/Konrad_Zuse), Germany who worked alone on his Z1 floating point computer that didn't catch on.  
People mainly used machine code for a while, which involved looking up instructions in a table and then hitting the button for the command  
#### Assembly Language
````
calculate (a+b*c)-d*e
start:
    load d  
    mul e  
    store t  
    load t  
    mul c  
    add a  
    sub t  
    a: .word 1  
    b: .word 1  
    c: .word 1  
    d: .word 1  
    t: .word 1  
````

Bell Labs wrote a formula translation language to translate equations into assembly code  

## Fortran
* allows you to write formulas in direct style
* features: is, subroutines, functions, arrays, i/o

### Fortran 11
* Object Oriented
* A ball

At the same time:  
[Grace Hopper](https://en.wikipedia.org/wiki/Grace_Hopper) wanted to make it easier for regular people to write programs which lead to Common Business-Oriented Language  

## COBOL
````
add a to b
add c to d yielding x
````
featured: subroutines, richer allocation, precision, 12 digit decimals, richer i/o, databases  

## LISP
* different model of computation
````
(+ 2 3)
````
gives 5
* more delightful

# Language
* idea of model computation
* Turing Machine - program goes through stages
* Church - Lambda Calculus , higher order functions, more mathematical  

Turing Machine can compute everything Churches Lambda Calculus (equivalent models of computation)

## Algol
* similar to Fortan, less clunky
* block structure, begin, end

### Algo 60
* closer to C based languages (Java, C++, etc.)
* immediate execution, not lazy

### Algo 68
* Lazy Evaluation
* based on the idea that a method may ignore some parameters depending on the value of others
* prevents needing to write multiple functions, callbacks
* evaluation when needed instead of when statement written
* call by need vs usual "eager evaluation"

70's  
Logicians like to write axioms, similar to databases  
Want to lookup database entries for truths  
can add more relational axioms in database to improve proof system power  

## Prolog
* Prolog = Logic Programming
* resolution theorem proving
* Formal systems -> produce theorems -> ask questions.
* Japanese threw a load of money into Prolog.
* [5th Generation Computing Project](https://en.wikipedia.org/wiki/Fifth_generation_computer)
* 'it's this really cool language'

80's  
Prolog tries multiple different proof strategies at the same time, good for parallel execution  

## [oaklisp](https://github.com/barak/oaklisp)

## [oak](https://en.wikipedia.org/wiki/Oak_(programming_language)

# Tower of Power
* lazy evaluation
* first-class functions
* monads
* purity
* algebraic data types
* logic variables, unification, and resolution theorem proving (ala Prolog)


## Purity
* no i/o
* no sideffects

## [Algebraic Data types](https://en.wikipedia.org/wiki/Algebraic_data_type)
* 'much cooler way to do it'

## Lazy evaluation
* modularity
* numeric computation, allows you choose when to stop iterative processes as it returns a stream. Going further down the stream results in greater accuracy/longer compute time
* [Monads](https://en.wikipedia.org/wiki/Monad_(functional_programming)

Throughout the course Barak will bring us up the Tower of Power
