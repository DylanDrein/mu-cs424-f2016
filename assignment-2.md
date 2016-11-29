# Assignment 2

*Due Date:* 11:00 Thurs 1-Dec-2016.

Note: this text will be replicated on Moodle.
Hand the assignment in on moodle as a single file (preferably ascii or utf-8 text).

## Setup

Consider the simply typed lambda calculus, with added types t1, t2, ..., t9 and one basis object of type b:t8 and five basis functions of types f4:t4→t5, f5:t5→t6, ..., f8:t8→t9.

Recall that in the simply typed lambda calculus, a variable after the symbol λ must be given a type, as in:

   λ x:t1 . b
   
This entire term would have type t1→t8, i.e.,

   (λ x:t1 . b) : t1→t8

### Problem 1

Define a term, using as short a definition as you can manage, of type (t1→t3)→(t2→t3→t5)→t2→t1→t7

### Problem 2

Prove (using the Curry-Howard Correspondence) that it is not possible to define a term of type (t7→t1)→t1→t7 but that it is possible to define a term of type (t7→t1)→t1→t8.
