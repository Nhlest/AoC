module AoC21 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc21 :: [()] -> Int
aoc21 a = 2

aoc21s :: [()] -> Int
aoc21s a = 2

runAoC21 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc21 $ arr

runAoC21s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc21s $ arr

filterForToday = do
  pure ()