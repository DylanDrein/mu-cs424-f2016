-- Question from last week
--  What's the difference between `data` and `newtype`

--  Both newtype and the single-constructor data introduce a single value
--  constructor, but the value constructor introduced by newtype is strict and
--  the value constructor introduced by data is lazy.

--  newtpe is used to wrap existsing types
--  newtype is faster
--  newtype is strict
--  newtype can only have one constructor whereas `data` can have many

-- Example of `newtype`
--  This could prevent a potential crash of a $125 million Mars orbiter
newtype Feet = Feet Double
newtype Cm   = Cm   Double

-- Functors
--  uniform action over a parameterized type, generalizing the map function on
--  lists.

-- | The 'Functor' class is used for types that can be mapped over.
-- Instances of 'Functor' should satisfy the following laws:

-- > fmap id  ==  id
-- > fmap (f . g)  ==  fmap f . fmap g
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- | A list is a functor!
--   In the case of lists, fmap is map
instance Functor [] where
  fmap = map

-- | As mentioned in lecture-21, Maybe/Mebe is a functor
instance  Functor Maybe  where
  fmap _ Nothing       = Nothing
  fmap f (Just a)      = Just (f a)

-- | Finally a function is a functor, this means we can map functions onto
--   functions, aka function composition.
instance Functor ((->) r) where
  fmap = (.)

-- Side note: Function composition
--  Function composition is the act of pipelining the result of one function,
--  to the input of another, creating an entirely new function.

--  Mathematically, this is most often represented by the ⊚ operator, where
--  f ⊚ g (often read as f of g) is the composition of f with g.

--  The `.` operator in Haskell is used for composition
--  See the definition below:

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

infixr 9 .
f $ x = f x

-- Applicatives
--  An applicative is an intermediate data structure between Functor and Monad.
--  An Applicative provides operations to

--    lift an type to a pure expression (pure)
--    sequence computations and combine their results (<*>).

--  Similar to Functors, Applicative typeclasses must follow four laws

--  Identity
--    pure id <*> v = v

--  Composition
--    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

--   homomorphism
--    pure f <*> pure x = pure (f x)

--   interchange
--    u <*> pure y = pure ($ y) <*> u

--  See Applicative Programming with Effects, by Conor McBride and Ross Paterson.
--  http://www.soi.city.ac.uk/~ross/papers/Applicative.html
class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

-- | A list is an applicative
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing


-- An example in ghci

-- ghci λ: :load Download.hs
-- [1 of 1] Compiling Main             ( Download.hs, interpreted )
-- Ok, modules loaded: Main.
-- ghci λ: (link1, link2) = ("http://adit.io/index.html","http://www.haskell.org/haskellwiki/Typeclassopedia")
-- ghci λ: pure (+) <*> printPayload link1 <*> printPayload link2
-- GET http://adit.io/index.html
-- Downloaded http://adit.io/index.html    Size: 3509 bytes.
-- GET http://www.haskell.org/haskellwiki/Typeclassopedia
-- Downloaded http://www.haskell.org/haskellwiki/Typeclassopedia   Size: 260374 bytes.
-- 263883

-- Monads
--  A Monad is a concept lifted from category theory, From the perspective of a
--  Haskell programmer, however, it is best to think of a monad as an abstract
--  datatype of actions.

--  http://chrisdone.com/posts/monads-are-burritos

--  When working with monads, we think in terms of their basic functions which
--  allows us to:
--    Lift a value to a monadic representation (return)
--    Sequentially compose two actions, passing any value produced by the first
--      as an argument to the second (>>=)

-- Note: In recent versions of Haskell, Monads are now also Applicatives,
-- previously Monads and Applicatives were two separate data structures. As
-- such, The monad's `return` function will be the same as the Applicative's
-- `pure` function and it will 'inherit' it from the Applicative definition
-- unless manually defined.

class Applicative m => Monad m where
  -- | Sequentially compose two actions, passing any value produced
  -- by the first as an argument to the second.
  (>>=)       :: Monad m => m a -> (a -> m b) -> m b

  -- | Sequentially compose two actions, discarding any value produced
  -- by the first, like sequencing operators (such as the semicolon)
  -- in imperative languages.
  (>>)        :: forall a b. m a -> m b -> m b
  m >> k = m >>= \_ -> k

  -- | Inject a value into the monadic type.
  return      :: a -> m a
  return      = pure

-- | Lists are monads
instance Monad []  where
  xs >>= f             = [y | x <- xs, y <- f x]

-- | And of course, Maybe is a monad too
instance  Monad Maybe  where
  (Just x) >>= k      = k x
  Nothing  >>= _      = Nothing

  return = Just

-- Most importantly IO is a monad

