module AoC18 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc18 :: [()] -> Int
aoc18 a = 2

aoc18s :: [()] -> Int
aoc18s a = 2

runAoC18 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc18 $ arr

runAoC18s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc18s $ arr

filterForToday = do
  pure ()