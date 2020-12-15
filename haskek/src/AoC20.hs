module AoC20 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc20 :: [()] -> Int
aoc20 a = 2

aoc20s :: [()] -> Int
aoc20s a = 2

runAoC20 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc20 $ arr

runAoC20s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc20s $ arr

filterForToday = do
  pure ()