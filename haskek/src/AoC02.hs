module AoC02 where

import Util

data Policy   = Policy Int Int Char
type Password = String
type Row      = (Policy, Password)

-- | Validates a set of passwords according to the first part ruleset
aoc02 :: [Row] -> Int
aoc02 [] = 0
aoc02 ((Policy lower higher chr, pw):ps) = isValid + aoc02 ps
 where isValid = let numofc = length $ filter (==chr) pw in
                     if numofc >= lower && numofc <= higher then 1 else 0

-- | Validates a set of passwords according to the second part ruleset
aoc02s :: [Row] -> Int
aoc02s [] = 0
aoc02s ((Policy lower higher chr, pw):ps) = isValid + aoc02s ps
 where isValid = if fs `xor` sn then 1 else 0
       fs = pw !! (lower  - 1) == chr
       sn = pw !! (higher - 1) == chr

runAoC02 input = do
  let arrOfTokens = parseUniversal [PRNumber, PRToken "-", PRNumber, PRWhitespace, PRChar, PRToken ":", PRWhitespace, PRWord, PRWhitespace] input
  let arrToCheck = map (\[ResultNumber l, ResultNumber h, ResultChar c, ResultWord w] -> (Policy (read l) (read h) c, w)) arrOfTokens
  print $ aoc02 arrToCheck
  pure ()

runAoC02s input = do
  let arrOfTokens = parseUniversal [PRNumber, PRToken "-", PRNumber, PRWhitespace, PRChar, PRToken ":", PRWhitespace, PRWord, PRWhitespace] input
  let arrToCheck = map (\[ResultNumber l, ResultNumber h, ResultChar c, ResultWord w] -> (Policy (read l) (read h) c, w)) arrOfTokens
  print $ aoc02s arrToCheck
  pure ()