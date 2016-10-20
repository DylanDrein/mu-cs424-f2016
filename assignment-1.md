# Assignment 1

*Due date:* 08:30 Thurs 27-Oct-2016.

*Handing it in:* I will place turn-in instructions here; the hope is to have an automated testing system in place, which can be iterated. 

## Part 1A

Fill in all the `error` holes in the macro alist from the code from lecture seven.

## Part 1B (optional)

For bonus points, write a `quasiquote` macro.
This would have various levels of functionality.

(a) Just handling `unquote`,

(b) Being more efficient, so eg `(quasiquote (a b c))` would expand to `(quote (a b c))` rather than to `(list (quote a) (quote b) (quote c))`.

(c) Also handling `unquote-splicing`

(d) efficiently.

## Part 2

Define a Scheme function `mark-tail-calls` which marks tail- vs
non-tail calls in an s-expression in *TCS*, the subset of core Scheme
defined as follows.

```
TCS ::= SYMBOL | NUMBER | IF | FUNCTION-CALL

IF ::= (if TCS TCS TCS)

FUNCTION-CALL ::= (TCS TCS...)
```
where ... means repeated zero or more times.

It marks them by replacing each tail-position function call `(f a...)` with `(tail-call f a...)` and each non-tail function call `(f a...)` with `(non-tail-call f a...)`.

Examples:
```
(mark-tail-calls '(if (a (b c)) (d (e (f g) h) i) (j (k 7))))
=> (if (non-tail-call a (non-tail-call b c)) (tail-call d (non-tail-call e (non-tail-call f g) h) i) (tail-call j (non-tail-call k 7)))
```
