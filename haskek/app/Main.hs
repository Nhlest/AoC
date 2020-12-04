module Main where

import System.Environment
import Control.Monad

import AoCTest
import AoC01
import AoC02
import AoC03
import AoC04
import Util

allModules = [
              ("aoctest", runAoCTest, "aoctest"),
              ("aoctest2", runAoCTest, "aoctest"),
              ("aoc01", runAoC01, "aoc01"), 
              ("aoc01s", runAoC01s, "aoc01"),
              ("aoc02", runAoC02, "aoc02"),
              ("aoc02s", runAoC02s, "aoc02"),
              ("aoc03", runAoC03, "aoc03"),
              ("aoc03s", runAoC03s, "aoc03"),
              ("aoc04", runAoC04, "aoc04"),
              ("aoc04s", runAoC04s, "aoc04")
              ]

main = do 
  args <- getArgs
  let cases_to_run = case args of
                       [] -> map (\(a,b,c) -> (runWithFile b c)) allModules
                       x -> intersectBy x allModules
  forM_ cases_to_run id
 where intersectBy _ [] = []
       intersectBy to ((x, f, l):xs) | x `elem` to = (runWithFile f l) : intersectBy to xs
                                     | otherwise   = intersectBy to xs
