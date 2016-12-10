import Data.List hiding (find)

-- Partial application
addOne :: Num a => a -> a
addOne x = x + 1

addOne' :: Num a => a -> a
addOne' = (+1)

addOneToAll :: Num a => [a] -> [a]
addOneToAll xs = map (\x -> x + 1) xs

addOneToAll' :: Num b => [b] -> [b]
addOneToAll' = map (+1)

-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html

data Mebe a = Nada | Gotta a
  deriving (Eq, Show)

-- | Similar data structure often used for error handling.
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html
data Either a b = Left a | Right b
  deriving (Eq, Show)

isGotta :: Mebe a -> Bool
isGotta (Gotta _) = True
isGotta _ = False

fromGotta :: Mebe a -> a
fromGotta (Gotta x) = x
fromGotta Nada    = error "Not jus"

catMaybes :: [Mebe a] -> [a]
catMaybes = map fromGotta . filter isGotta


inverse :: (Eq a, Fractional a) => a -> Mebe a
inverse 0 = Nada
inverse x = Gotta (1 / x)

safeHead :: [a] -> Mebe a
safeHead [] = Nada
safeHead (x:xs) = Gotta x

find :: (a -> Bool) -> [a] -> Mebe a
find _ [] = Nada
find f (x:xs) = if f x
                  then Gotta x
                  else find f xs

instance Functor Mebe where
  fmap _ Nada    = Nada
  fmap f (Gotta x) = Gotta (f x)

-- Covered on lecture-22.hs
instance Applicative Mebe where
  pure = Gotta

  Gotta f  <*> m       = fmap f m
  Nada <*> _m      = Nada

  Gotta _m1 *> m2      = m2
  Nada  *> _m2     = Nada

instance Monad Mebe where
  return = Gotta

  (Gotta x) >>= f = f x
  Nada    >>= f = Nada

  (>>) = (*>)
