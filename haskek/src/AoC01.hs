module AoC01 where

import Util

-- | Maybe finds two numbers from a list that give `target` in sum and multiplies them
aoc01 :: Int -> [Int] -> Maybe Int
aoc01 _ [] = Nothing
aoc01 target (x:xs) | null complements = aoc01 target xs
                    | otherwise        = Just $ x * head complements
  where complements = filter (==target-x) xs

-- | Maybe finds `count` numbers from a list that give `target` in sum and multiplies them
aoc01s :: Int -> Int -> [Int] -> Maybe Int
aoc01s _ _ [] = Nothing
aoc01s count target l@(x:xs) | count < 2  = Nothing
                             | count == 2 = aoc01 target l
                             | otherwise  = case aoc01s (pred count) (target-x) xs of
                                              Nothing -> aoc01s count target xs
                                              Just y  -> Just $ x * y

runAoC01 input = do
  let arrToSort = parseAsInts Nothing input
  print $ aoc01 2020 arrToSort

runAoC01s input = do
  let arrToSort = parseAsInts Nothing input
  print $ aoc01s 3 2020 arrToSort