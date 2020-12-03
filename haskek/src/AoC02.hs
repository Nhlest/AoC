module AoC02 where

import Util
import Data.Either

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

data PolicyToken = PolNum Int | PolChar Char | PolWord String

runAoC02 input = do
  let arrToCheck = parseUniversal [PRNumber PolNum, PRTokenSilent "-", PRNumber PolNum, PRWhitespace, PRChar PolChar, PRTokenSilent ":", PRWhitespace, PRWord PolWord, PRWhitespace] 
                    (\[PolNum l, PolNum h, PolChar c, PolWord w] -> (Policy l h c, w)) input
  print $ aoc02 $ fromRight [] arrToCheck
  pure ()

runAoC02s input = do
  let arrToCheck = parseUniversal [PRNumber PolNum, PRTokenSilent "-", PRNumber PolNum, PRWhitespace, PRChar PolChar, PRTokenSilent ":", PRWhitespace, PRWord PolWord, PRWhitespace] 
                    (\[PolNum l, PolNum h, PolChar c, PolWord w] -> (Policy l h c, w)) input
  print $ aoc02s $ fromRight [] arrToCheck
  pure ()