-- More Monads

mSqrt :: Double -> Maybe Double
mSqrt x = if (x<0)
          then Nothing
          else Just (sqrt x)

-- Want
--   f x = sqrt (sin (sqrt x))
-- but with Nothing if problem

f1, f2, f3, f4 :: Double -> Maybe Double

-- *Main> Just 4 >>= (\x -> Just (sin x))
-- Just (-0.7568024953079282)
-- *Main> Just 4 >>= Just . sin
-- Just (-0.7568024953079282)
-- *Main> Just 4 >>= return . sin
-- Just (-0.7568024953079282)


f1 x = mSqrt x >>= return . sin >>= mSqrt
f2 = (>>= mSqrt) . (>>= return . sin) . mSqrt

-- *Main Control.Monad> (/) 1 3
-- 0.3333333333333333
-- *Main Control.Monad> (1/) 3
-- 0.3333333333333333
-- *Main Control.Monad> :t (1/)
-- (1/) :: Fractional a => a -> a
-- *Main Control.Monad> (/3) 1

f3 x = mSqrt x
       >>= (\y -> return (sin y))
       >>= (\z -> mSqrt z)

f4 x = do
  y <- mSqrt x
  z <- return (sin y)
  mSqrt z

putXXXln :: IO ()
-- putXXXln = putChar 'x'
--            >>= (\_ -> putChar 'x')
--            >>= (\_ -> putChar 'x')
--            >>= (\_ -> putChar '\n')

-- putXXXln = pxx (pxx (px)) >>= (\_ -> putChar '\n')
--   where px = putChar 'x'
--         pxx = (>>= \_ -> px)

-- a >> b = a >>= (\_ -> a)

-- putXXXln = do3 (putChar 'x') >> putChar '\n'
--   where do3 a = a >> a >> a

putXXXln = do
  px
  px
  px
  putChar '\n'
  where px = putChar 'x'

putStringLn :: [Char] -> IO ()
putStringLn s = putString s >> putChar '\n'

putString :: [Char] -> IO ()
-- putString [] = return ()
-- putString (c:s) = putChar c >> putString s
putString s = seqM $ map putChar s

seqM :: Monad m => [m ()] -> m ()
seqM as = foldl (>>) (return ()) as

inOutThreeChar :: IO ()
inOutThreeChar =
  getChar >>= (\c -> seqM $ [putChar '\n'] ++ (take 3 $ repeat $ putChar c) ++ [putChar '\n'])

-- *Main Control.Monad> :t getChar 
-- getChar :: IO Char
-- *Main Control.Monad> :t repeat
-- repeat :: a -> [a]
-- *Main Control.Monad> take 100 (repeat 1)
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
-- *Main Control.Monad> :l lecture-23.hs
-- [1 of 1] Compiling Main             ( lecture-23.hs, interpreted )
-- Ok, modules loaded: Main.
-- *Main Control.Monad> inOutThreeChar 
-- v
-- vvv
