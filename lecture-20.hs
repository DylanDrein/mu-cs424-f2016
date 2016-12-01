-- Introduction to Model Theory

-- Technique for showing that a set of axioms A does not imply some
-- conjecture C.

-- 1. Find a world in which all the axioms A are true and C is false.
-- 2. Suppose that logic is self-consistent.
-- 3. QED

-- a1 : s1 -> d1
-- a2 : s2 -> d2
-- a3 : s3 -> d3
-- pp : d2 -> d3

-- conjecture: d2 (want to prove false)
-- Model:
--    d2=false
--    s2=false
--    s1,d1,s3,d3 = true

-- Algebraic Data Types

-- Color ... enum Red, Blue, Green
-- Point ... struct x, y
-- Shape ... union Square, Circle

data Color = Red | Blue | Green
            deriving (Show, Eq)
data Point = Point Double Double
            deriving (Show, Eq)
data Shape = Circle Double Double Double | Square Double Double Double Double 
           deriving (Show, Eq)

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Blue Blue = True
colorEq Green Green = True
colorEq _ _ = False

scaleShape :: Double -> Shape -> Shape
scaleShape s (Circle x y radius) = Circle x y (s*radius)
scaleShape s (Square x0 y0 x1 y1) =
  let xc = (x0+x1)/2
      yc = (y0+y1)/2
  in
   Square (xc+s*(x0-xc)) (yc+s*(y0-yc)) (xc+s*(x1-xc)) (yc+s*(y1-yc))

scaleShape' :: Double -> Shape -> Shape
scaleShape' s (Circle x y radius) = Circle x y (s*radius)
scaleShape' s (Square x0 y0 x1 y1) =
  Square (xc+s*(x0-xc)) (yc+s*(y0-yc)) (xc+s*(x1-xc)) (yc+s*(y1-yc))
  where
    xc = (x0+x1)/2
    yc = (y0+y1)/2

-- Define MyList of Int

data MyListInt = NilInts | MyListIntPair Int MyListInt
               deriving (Show, Eq)

myLIHead :: MyListInt -> Int
myLIHead (MyListIntPair i _) = i

myLIEmpty :: MyListInt -> Bool
myLIEmpty (MyListIntPair _ _) = False
myLIEmpty NilInts = True

myLIlength :: MyListInt -> Int
myLIlength (MyListIntPair _ mores) = 1 + myLIlength mores
myLIlength NilInts = 0

-- Define MyList of t

data List t = Nil | Pair t (List t)
               deriving (Show, Eq)

myHead :: List t -> t
myHead (Pair x _) = x

myEmpty :: List t -> Bool
myEmpty (Pair _ _) = False
myEmpty Nil = True

myLength :: List t -> Int
myLength (Pair _ xs) = 1 + myLength xs
myLength Nil = 0
