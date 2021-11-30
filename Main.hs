module Main where

import Control.Monad
import Control.Monad.ST
import Criterion.Main
import Data.STRef
import qualified Data.Vector as V


sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ \x -> do
    modifySTRef n (+ x)
  readSTRef n

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort l@(x : xs) = quickSort small ++ mid ++ quickSort large
  where
    small = [y | y <- l, y < x]
    mid = [y | y <- l, y == x]
    large = [y | y <- l, y > x]


fibST :: Integer -> Integer
fibST n =
    if n < 2
    then n
    else runST $ do
        x <- newSTRef 0
        y <- newSTRef 1
        fibST' n x y

    where fibST' 0 x _ = readSTRef x
          fibST' n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y $! x'+y'
              fibST' (n-1) x y

fib :: Integer -> Integer
fib n =
  if n < 2
  then n
  else fib' n 0 1

  where fib' 0 x _ = x
        fib' n x y = fib' (n-1) y (x+y)

-- Our benchmark harness.
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ],
  bgroup "fibST" [ bench "1"  $ whnf fibST 1
               , bench "5"  $ whnf fibST 5
               , bench "9"  $ whnf fibST 9
               , bench "11" $ whnf fibST 11
               ]
  ]
