module Main where

import Control.Monad
import Control.Monad.ST
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
quickSort (x : xs) = quickSort small ++ mid ++ quickSort large
  where
    small = [y | y <- xs, y < x]
    mid = [y | y <- xs, y == x] ++ [x]
    large = [y | y <- xs, y > x]

main :: IO ()
main = do
  print (sumST [1, 2, 3])
  print $ quickSort [3, 4, 0, 1, 8, 9, 10]
