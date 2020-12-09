module AoC09 where

import Data.Either
import Data.Maybe
import Util

chunksOf :: Int -> [a] -> [[a]]
chunksOf a lis@(_:ls) = if length ls < a then [lis]
                    else take a lis:chunksOf a ls

aoc09 :: [Int] -> Int
aoc09 xmas = head $ mapMaybe ((\(x : xs) -> if null [1 | a <- xs, b <- xs, a + b == x] then Just x else Nothing) . reverse) (chunksOf 26 xmas)

aoc09s :: [Int] -> Int -> Int
aoc09s xmas target = head $ [minimum chunk + maximum chunk | n <- [2..length xmas -1], chunk <- chunksOf n xmas, sum chunk == target]

runAoC09 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc09 $ fromRight [] arrOfTokens

runAoC09s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc09s arr (aoc09 arr)

filterForToday = do
  number <* whitespace