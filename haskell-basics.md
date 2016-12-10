CS424 Haskell Basics
====================

# Contents

* [Haskell Basics](#haskell-basics)
    * [The Glorious Glasgow Haskell Compiler](#the-glorious-glasgow-haskell-compiler)
    * [Running Haskell](#running-haskell)
    * [Hello, World! in Haskell](#hello-world-in-haskell)
    * [Literate Programming](#literate-programming)
* [Functions, Lists, Pattern Matching and Recursion](#functions-lists-pattern-matching-and-recursion)
    * [Functions](#functions)
    * [Lists](#lists)
    * [Pattern Matching](#pattern-matching)
* [Types and Typeclasses](#types-and-typeclasses)
    * [Types](#types)

# Haskell Basics

## The Glorious Glasgow Haskell Compiler
The Glasgow Haskell Compiler (ghc) is a compiler for Haskell, similar to javac
for Java and g++ for C++.  ghc behaves similarly to gcc and g++,  if you know
how to compile C/C++ then you probably know how to compile Haskell.

## Running Haskell
Haskell can either be compiled (like C and Java) or interpreted (like Python).  
This gives the us the benefits of efficient programs after compilation and the
speed of development that comes with interpreted languages.

### Interpreted Haskell
Running Haskell interpreted will run the code directly from the source code.  It
will not be optimised very much before it runs but it also doesn't generate
executable files.  When Haskell is run through Sublime Text it is interpreted.

```console
$ runhaskell HelloWorld.hs
```

### Compiling Haskell
Compiling Haskel with ghc creates an executable file that can be run again and
again.  When Haskell is compiled it optimises the code before creating an
executable.  Haskell code goes through a huge amount of optimisation, ghc can
compile with different levels of optimisation using the `-O1`,`-O2`,`-O3`,`-O4`
options when compiling.

Compiling files is pretty simple.  To compile `HelloWorld.hs` we usually use.
```bash
$ ghc -O2 -o compiled/HelloWorld HelloWorld.hs
```
**Note:** I like to make a separate folder for compiled code, usually calling it
`bin`.  This is not necessary at all, but it keeps the folder neat.

To run the program, we use:
```
$ ./compiled/HelloWorld
```

### Compiler options

```console
-O2 => Compile with optimisation
-o compiled/HelloWorld => Create the executable in the compiled folder with the
name 'HelloWorld'
-threaded => Enable multithreading
```

## Hello, World! in Haskell
```haskell
main = print "Hello, world!"
```

## Literate Programming

There are two file extensions associated with Haskell source code:
- `.hs` - Normal Haskell
- `.lhs` - Literate Haskell

Haskell supports Literate Programming

What is literate programming? To quote Dr. Knuth:

>"The main idea is to regard a program as a communication to human beings rather than as a set of instructions to a computer."

In a literate Haskell program, there are two ways to distinguish between code
and non-code portions. You can: either prepend all code with a > , (bird style)
or surround lines of code with \begin{code} and \end{code} pairs (latex style).
For those who know, use and love latex, the latter is the suggested way to go.

[Literate Haskell - Haskell Wiki](https://www.haskell.org/haskellwiki/Literate_programming)
# Functions, Lists, Pattern Matching and Recursion

## Functions
Functions are defined at the top level.  Haskell uses spaces to separate
function parameters rather than brackets.

```haskell
addOne x = x + 1
```

This function will return a number (x + 1) given any input x.

The basic function syntax is defined in
[**Learn You a Haskell for Great Good! - Baby's first functions**](http://learnyouahaskell.com/starting-out#babys-first-functions).

### Prefix and Infix functions
Haskell uses a mixture of prefix and infix notation in its syntax. All
functions use prefix notation unless the function name is comprised of only
special characters -
`!` `#` `$` `%` `&` `*` `+` `.` `/` `<` `=` `>` `?` `@` `^` `|` `-` `~` `:`

#### Using prefix functions as infix
Surrounding a function in back ticks `\`` will allow it to be used as an infix
operator. For example, for all {x,y}:

```haskell
mod x y == x `mod` y

div x y == x `div` y

add :: Num a => a -> a -> a
add x y = x + y

add x y == x `add` y

```

#### Using infix functions as prefix
Similarity, Surrounding an infix function in brackets `()` will allow it to be
used as a prefix function. For example, for all {x,y}

```haskell
x + y == (+) x y

x / y == (/) x y

-- | List Concatenation
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
```

### Haskell Lexical Structure
A full lexical structure can be found in the Haskell2010 Report -
https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4

## Lists
In Haskell, lists are a homogeneous data structure. It stores several elements
of the same type. Lists are denoted with square brackets `[]` with comma
separated values.

```haskell
xs = [1,2,3]
```

**Note:** Many Haskell programmers use the variable names `xs` and `ys` to
denote lists. As with all variable names, they can be called anything*. The
name `xs` can be read as 'the plural of x'.

\*almost anything, variables must begin with a lower case letter.

### List definition in [Haskell Source Code][Source]
```haskell
data [] a = [] | a : [a]
```

[Source]: https://downloads.haskell.org/~ghc/7.0.4/docs/html/libraries/ghc-prim-0.2.0.0/src/GHC-Types.html#Char

## Pattern Matching
Haskell will treat multiple functions with the same name as different patterns
for a function to follow.  If we know a definite answer for a given input, we
can define a pattern for it.  For example if we try to divide by 0 we should get
an error.  The function below defines the inverse function, `λ x -> 1 / x`, for
any given number `x` the function should return `1 / x`, unless `x == 0`.

```haskell
inverse 0 = error "Divide by zero"
inverse x = 1 / x
```

Patterns matching can be done over multiple lines. In the following example we
define `zipWith`. `zipWith` is a function that takes two lists and joins them
with a function.

```haskell
zipWith (+) [1,2,3] [2,4,6] = (1 + 2) : (2 + 4) : (3 + 6) : []
                            =  3 : 6 : 9 : []
                            = [3, 6, 9]
```

### `zipWith` definition
```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) =  f x y : zipWith f xs ys
```

### Splat! - More Syntactic Sugar
When dealing with lists as function arguments, the list can be split into its
head and tail using the splat operator `(:)` on the left hand side of the
assignemnt. In the function `f (x:xs)` below, x becomes the first element of
the list and xs becomes a list containg the remaining elements.

```haskell
f :: [a] -> [a]
f (x:xs) = ...
```

For example, the following functions both convert a string in any case to title
case. i.e. this first letter of the word is capitalised, the rest are lowercase.
The first uses traditional `head`/`tail` syntax, the second uses the splat.

```haskell
titleCase :: [Char] -> [Char]
titleCase xs = toUpper (head xs) : map toLower (tail xs)

titleCase :: [Char] -> [Char]
titleCase (x:xs) = toUpper x : map toLower xs
```

*Note:* The x:xs pattern is used a lot, especially with recursive functions. But
patterns that have : in them only match against lists of length 1 or more.

This syntax can be used to bind any number of leading elements, but they can
only pattern match against lists with enough elements.

```haskell
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- source: Learn You a Haskell for Great Good
```

Unmatched patterns will cause run time exceptions. Take the `titleCase`
function above, it does not provide a pattern for an empty list/string. If
`titleCase` is used on an empty string it will produce an error:

```haskell
ghci> titleCase ""
"*** Exception: <interactive>:1:1-45: Non-exhaustive patterns in function titleCase
```

This can be avoided by providing general patterns for all functions. Compiling
with `-Wincomplete-patterns` or `-Wall` will point out any incomplete patterns.

# Types and Typeclasses

## Types

Haskell uses a strict type systems, i.e. it uses types like `Int`, `Float`,
`Double`, `Boolean`, `Char` etc.  but it is not like most languages.  Haskell
has the ability to infer the type of something based on what functions its used
in.  This allows for functions to become polymorphic automatically.

```haskell
let x = 1     -- x is a number, it could be Int, Float or Double
print (x / 2) -- x is now a Double or Float since division is a function that
              -- returns a floating point number.  More on this later.
```

Haskell uses the `::` operator to represent types.  The function f where
`f :: Int -> Int` can be read as "a function f of type Int -> Int".  `::` can
also be used to explicitly give a type to something, but this is somewhat rare.

```haskell
x = 1        -- x can be any number type.
y = 1 :: Int -- y is definitely an Int.
```

When writing Haskell code we like to give function types to go along with our
functions.  These are not necessary as Haskell will infer the type of a function
but it is always recommended to write one.  When defining a function
type definition we usually write the type above it.

```haskell
-- | Checks if a given String or list is a Palindrome (same forward as back).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
```

### Types in the Haskell Source code
[GHC.Types](https://downloads.haskell.org/~ghc/7.4.1/docs/html/libraries/ghc-prim-0.2.0.0/src/GHC-Types.html)

### Infering types in ghci

In ghci, Haskell's interactive prompt, we can get the inferred type by using the
`:t` ghci operation.  Note:  In ghci, we need to use the `let` keyword to define
functions as it is in an interactive prompt (a Monadic interface).

```haskell
Prelude> let isPalindrome xs = xs == reverse xs
Prelude> :t isPalindrome
isPalindrome :: Eq a => [a] -> Bool
```

### Arrow Notation

In the above example we use two different type of arrow notation, `=>` and `->`.
I tend to call them 'Fat Arrow' and 'Skinny Arrow' respectively. `->` is used
to separate the different parameters and return type.  In Haskell, every
function can only return one thing, no more, no less.  As such the last term in
the type definition is the return type.  All of the terms before the last `->`
are the function parameters.

```haskell
add :: Int -> Int -> Int   -- This is bad practice, usually functions should
add x y = x + y            -- be as generic as possible.
```

In the above example the function `add` has a type of `Int -> Int -> Int`,
This means `addInts` takes two `Int` values and returns an `Int` value.

The fat arrow `=>` is used to define type variables in functions.  This is where
we can substitute a **type variable** in place of a definite type in the
function definition.  Type variables can be constrained to one or more **Type
Classes**.  I tend to use `'a'` as a type variable to represent
'anything' but any lowercase word can be used.

```haskell
add :: Num a => a -> a -> a
add x y = x + y
```

In this case we use the `Num` typeclass, the `Num` typeclass contains any type
that can be used as a number.  (Anything you can add, subtract, multiply etc.)
By using the `Num` typeclass, this allows our function to work for any Number
type.  This includes `Int`, `Floats` etc. but also any data type that has an
instance of the `Num` typeclass such as `Vector`, `Matrix`, `Complex`

If we have more than one typeclass,  we list them in brackets, separated by
`,`.  For example:

```haskell
factorial :: (Enum a, Num a) => a -> a
factorial n =  product [1..n]
```

Here we use the `Enum` and `Num` type class because the `product` function is of
type `product :: Num a => [a] -> a` and `..` is a function that works with
Enumarator types.
