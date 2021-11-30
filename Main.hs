module Main where

import Control.Monad.ST
import Data.STRef
import Control.Monad
import HaskellSay (haskellSay)

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_  xs $ \x -> do
    modifySTRef n (+x)
  readSTRef n


main :: IO ()
main = do
  haskellSay $ "Hello Haskell Nixers!" <> show (sumST [1, 2, 3])
