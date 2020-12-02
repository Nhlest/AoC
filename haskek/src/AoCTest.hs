module AoCTest where

import Util

-- | Test function in preparation for AoC, should sort an array of Integers
aocTest :: [Int] -> [Int]
aocTest [] = []
aocTest (x:xs)       = less_than ++ [x] ++ greater_than
  where less_than    = aocTest [y | y <- xs, y <= x]
        greater_than = aocTest [y | y <- xs, y > x]

runAoCTest input = do
  let arrToSort = map (\[ResultNumber n] -> read n) $ parseUniversal [PRNumber, PRWhitespace] input
  print $ aocTest arrToSort