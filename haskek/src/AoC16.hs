module AoC16 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc16 :: [()] -> Int
aoc16 a = 2

aoc16s :: [()] -> Int
aoc16s a = 2

runAoC16 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc16 $ arr

runAoC16s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc16s $ arr

filterForToday = do
  pure ()