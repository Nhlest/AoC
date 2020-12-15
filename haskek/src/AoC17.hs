module AoC17 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc17 :: [()] -> Int
aoc17 a = 2

aoc17s :: [()] -> Int
aoc17s a = 2

runAoC17 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc17 $ arr

runAoC17s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc17s $ arr

filterForToday = do
  pure ()