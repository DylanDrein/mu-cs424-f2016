-- | Example class to demonstrate multithreading in Haskell
--
-- This class depends on the following packages:
--  - async -> https://hackage.haskell.org/package/async
--  - http-conduit -> https://hackage.haskell.org/package/http-conduit
--
--  Use `cabal install async http-conduit` to install them.
module Main where

import Control.Concurrent.Async
import Network.HTTP.Conduit

import Data.Int (Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS

type URL = String

-- | Perform a GET request on `url` and return the size of the returned data in
-- bytes
payload :: URL -> IO Int64
payload = fmap BS.length . simpleHttp

printPayload :: (Int, URL) -> IO Int64
printPayload (index, url) = do
  putStrLn $ show index ++ ":\tGET " ++ url
  size <- payload url
  putStrLn $ show index ++ ":\tDownloaded " ++ url ++ "\tSize: " ++ show size ++ " bytes."
  return size

-- Single-threaded main
-- main :: IO ()
-- main = mapM_ printPayload $ zip [1..] links

-- Multi-threaded main
main :: IO ()
main = mapConcurrently_ printPayload $ zip [1..] links

-- | All of my Haskell related bookmarks
links :: [URL]
links = [ "http://adit.io/index.html"
        , "http://www.haskell.org/haskellwiki/Typeclassopedia"
        , "http://byorgey.wordpress.com/2014/01/17/diagrams-1-0/"
        , "http://bartoszmilewski.com/"
        , "https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/"
        , "https://izbicki.me/blog/functors-and-monads-for-analyzing-data.html"
        , "http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/"
        , "https://codetalk.io/posts/2015-11-28-briefly-on-the-purpose-of-functors-applicatives-and-monads.html"
        , "https://izbicki.me/blog/polymorphism-in-haskell-vs-c%2B%2B.html"
        , "http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/"
        , "https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html"
        , "http://gelisam.blogspot.co.at/2015/06/will-it-memoize.html?m=1"
        , "http://dev.stephendiehl.com/hask/#laziness"
        , "https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/"
        , "https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#maintenance"
        , "http://dev.stephendiehl.com/fun/index.html"
        , "http://www.bcl.hamilton.ie/~barak/papers/sound-efficient-ad2008.pdf"
        , "http://stackoverflow.com/questions/3208258/memoization-in-haskell/3209189#3209189"
        , "https://medium.com/@ayanonagon/the-y-combinator-no-not-that-one-7268d8d9c46#.p0gbxco34"
        , "http://alexey.radul.name/ideas/2013/introduction-to-automatic-differentiation/"
        , "https://gist.github.com/quchen/5280339"
        , "http://stackoverflow.com/questions/15216202/should-i-use-a-lexer-when-using-a-parser-combinator-library-like-parsec"
        , "http://www.cse.unsw.edu.au/~pls/thesis/munc-thesis.pdf"
        , "https://gist.github.com/staltz/868e7e9bc2a7b8c1f754"
        , "http://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell/19213247#19213247"
        , "https://github.com/nh2/call-haskell-from-anything" ]
