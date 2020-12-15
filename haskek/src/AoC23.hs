module AoC23 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc23 :: [()] -> Int
aoc23 a = 2

aoc23s :: [()] -> Int
aoc23s a = 2

runAoC23 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc23 $ arr

runAoC23s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc23s $ arr

filterForToday = do
  pure ()