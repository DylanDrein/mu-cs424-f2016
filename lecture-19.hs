-- Haskell!!!

-- Scheme = reified pure λ Calculus
--   - simple as possible syntax
--   - simple as possible semantics
--     - eager order evaluation
--     - not pure (i.e., dirty): I/O, (set! v 3), ...

-- Haskell = reified typed λ Calculus
--   - strongly typed, polymorphic types
--   - pure (I/O and mutation via "monads")
--   - syntax as much like math as possible
--     - cancer of the semicolon, profusion of syntax
--   - algebraic data types (unify structs, unions, enums)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fib1 n =
  if n==0
  then 1
  else if n==1
       then 1
       else
         fib1 (n-1) + fib1 (n-2)

hypot x y = sqrt (x^2 + y^2)

-- (+++) x y = hypot x y
(+++) = hypot

head' (h:_) = h
tail' (_:t) = t

{-
*Main> (+) 2 3
5
*Main> 2+3
5

*Main> take 10 [1,3..]
[1,3,5,7,9,11,13,15,17,19]
*Main> [x^2 | x<-[0..5]]
[0,1,4,9,16,25]
*Main> [x+y | x<-[0..5], y<-[0..5]]
[0,1,2,3,4,5,1,2,3,4,5,6,2,3,4,5,6,7,3,4,5,6,7,8,4,5,6,7,8,9,5,6,7,8,9,10]
*Main> [10*x+y | x<-[0..5], y<-[0..5]]
[0,1,2,3,4,5,10,11,12,13,14,15,20,21,22,23,24,25,30,31,32,33,34,35,40,41,42,43,44,45,50,51,52,53,54,55]
*Main> [10*x+y | x<-[0..5], y<-[0..5], x>y]
[10,20,21,30,31,32,40,41,42,43,50,51,52,53,54]
*Main> [[x,y,z] | x<-[0..10], y<-[0..10], z<-[0..10], x^2+y^2 == z^2]
[[0,0,0],[0,1,1],[0,2,2],[0,3,3],[0,4,4],[0,5,5],[0,6,6],[0,7,7],[0,8,8],[0,9,9],[0,10,10],[1,0,1],[2,0,2],[3,0,3],[3,4,5],[4,0,4],[4,3,5],[5,0,5],[6,0,6],[6,8,10],[7,0,7],[8,0,8],[8,6,10],[9,0,9],[10,0,10]]
*Main> [[x,y,z] | x<-[1..10], y<-[1..10], z<-[1..10], x^2+y^2 == z^2]
[[3,4,5],[4,3,5],[6,8,10],[8,6,10]]

-}

repeat' x = x:repeat' x
repeat'' x =
  let xs = repeat'' x
  in x:xs

threePis = [pi,pi,pi]

fibs = 1:1:zipWith (+) fibs (tail fibs)

map' f [] = []
map' f (x:xs) = f x:map' f xs
