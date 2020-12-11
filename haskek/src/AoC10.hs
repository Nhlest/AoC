module AoC10 where

import Data.Either
import Data.Maybe
import Util
import Data.List
import qualified Data.Map as M

aoc10 :: [Int] -> Int
aoc10 jolt = let (a,b,c) = foldl (\(a,b,c) d -> (d, b + (if d-a == 1 then 1 else 0), c + (if d-a == 3 then 1 else 0))) (0,0,1) $ sort jolt in b*c

aoc10s :: [Int] -> Int
aoc10s jolt = fromJust $ M.lookup (last srt) $ go [] srt M.empty
  where go [] (x:xs) cache = go [x] xs (M.insert x 1 cache)
        go _ [] cache = cache
        go (x1:x2:x3:xs) (y:ys) cache = if x1 - y > 3 then go (y:x1:x2:x3:xs) ys cache
                                 else let m = (if x1 - y <= -3 then fromJust $ M.lookup x1 cache else 0) *
                                              (if x2 - y <= -3 then fromJust $ M.lookup x2 cache else 0) *
                                              (if x3 - y <= -3 then fromJust $ M.lookup x3 cache else 0)
                                      in go (y:x1:x2:x3:xs) ys (M.insert y m cache)
        go (x1:x2:xs) (y:ys) cache = if x1 - y > 3 then go (y:x1:x2:xs) ys cache
                                 else let m = (if x1 - y <= -3 then fromJust $ M.lookup x1 cache else 0) *
                                              (if x2 - y <= -3 then fromJust $ M.lookup x2 cache else 0)
                                      in go (y:x1:x2:xs) ys (M.insert y m cache)
        go (x1:xs) (y:ys) cache = if x1 - y > 3 then go (y:x1:xs) ys cache
                                 else let m = (if x1 - y <= -3 then fromJust $ M.lookup x1 cache else 0)
                                      in go (y:x1:xs) ys (M.insert y m cache)
        srt = reverse $ sort jolt
        

runAoC10 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc10 $ fromRight [] arrOfTokens

runAoC10s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc10s arr

filterForToday = do
  number <* whitespace