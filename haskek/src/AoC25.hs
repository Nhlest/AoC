module AoC25 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc25 :: [()] -> Int
aoc25 a = 2

aoc25s :: [()] -> Int
aoc25s a = 2

runAoC25 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc25 $ arr

runAoC25s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc25s $ arr

filterForToday = do
  pure ()