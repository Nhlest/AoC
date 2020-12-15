module AoC22 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc22 :: [()] -> Int
aoc22 a = 2

aoc22s :: [()] -> Int
aoc22s a = 2

runAoC22 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc22 $ arr

runAoC22s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc22s $ arr

filterForToday = do
  pure ()