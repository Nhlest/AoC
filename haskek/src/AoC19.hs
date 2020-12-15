module AoC19 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc19 :: [()] -> Int
aoc19 a = 2

aoc19s :: [()] -> Int
aoc19s a = 2

runAoC19 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc19 $ arr

runAoC19s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc19s $ arr

filterForToday = do
  pure ()