module AoC03 where

import Util

import Data.Maybe

data CellType = Empty | Tree deriving Eq

getRepeated m (x,y) | y >= length m = Nothing
                    | otherwise = Just $ m !! y !! xx
                        where xx = x `mod` (length $ head m)
-- |
aoc03 :: (Int, Int) -> [[CellType]] -> Int
aoc03 (sx, sy) m = length $ filter (==Tree) $ map (fromMaybe Empty) $ takeWhile (isJust) [getRepeated m (t*sx, t*sy) | t <- [1..]]

aoc03s :: [[CellType]] -> Int
aoc03s m = product [aoc03 (sx, sy) m | (sx, sy) <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]

-- | a

splitBy delimiter = foldr f [[]] 
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

runAoC03 input = do
  let arrOfTokens = parseUniversal [PRChar] input
  let arrToCheck = map concat $ splitBy [ResultChar '\n'] $ arrOfTokens
  print $ length arrToCheck
  print $ aoc03 (3, 1) $ map (\r -> map (\(ResultChar c) -> if c == '.' then Empty else Tree) r) arrToCheck
  pure ()

runAoC03s input = do
  let arrOfTokens = parseUniversal [PRChar] input
  let arrToCheck = map concat $ splitBy [ResultChar '\n'] $ arrOfTokens
  print $ length arrToCheck
  print $ aoc03s $ map (\r -> map (\(ResultChar c) -> if c == '.' then Empty else Tree) r) arrToCheck
  pure ()