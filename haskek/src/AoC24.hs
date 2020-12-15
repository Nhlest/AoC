module AoC24 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State


aoc24 :: [()] -> Int
aoc24 a = 2

aoc24s :: [()] -> Int
aoc24s a = 2

runAoC24 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc24 $ arr

runAoC24s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc24s $ arr

filterForToday = do
  pure ()