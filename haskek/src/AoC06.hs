module AoC06 where

import Util
import Data.Either

aoc06 :: [()] -> Int
aoc06 = const 2

aoc06s :: [()] -> Int
aoc06s = const 2

runAoC06 input = do
  let arrOfTokens = parseUniversal input $ do
        pure ()
  print $ aoc06 $ fromRight [] arrOfTokens

runAoC06s input = do
  let arrOfTokens = parseUniversal input $ do
        pure ()
  print $ aoc06s $ fromRight [] arrOfTokens