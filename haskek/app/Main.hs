module Main where

import System.Environment
import Control.Monad

import AoCTest
import AoC01
import AoC02
import AoC03
import AoC04
import AoC05
import AoC06
import Util

allModules = [
              ("aoctest",  runAoCTest,  "aoctest"),
              ("aoctest2", runAoCTest,  "aoctest"),
              ("aoc01",    runAoC01,    "aoc01"), 
              ("aoc01s",   runAoC01s,   "aoc01"),
              ("aoc02",    runAoC02,    "aoc02"),
              ("aoc02s",   runAoC02s,   "aoc02"),
              ("aoc03",    runAoC03,    "aoc03"),
              ("aoc03s",   runAoC03s,   "aoc03"),
              ("aoc04",    runAoC04,    "aoc04"),
              ("aoc04s",   runAoC04s,   "aoc04"),
              ("aoc05",    runAoC05,    "aoc05"),
              ("aoc05s",   runAoC05s,   "aoc05"),
              ("aoc05alt", runAoC05alt, "aoc05"),
              ("aoc05salt",runAoC05salt,"aoc05"),
              ("aoc06",    runAoC06,    "aoc06"),
              ("aoc06s",   runAoC06s,   "aoc06")
             ]

runCase (name, c) = do
  putStrLn $ "-- Running case " <> name
  c

main = do 
  args <- getArgs
  let cases_to_run = case args of
                       [] -> map (\(a,b,c) -> (a, runWithFile b c)) allModules
                       x -> intersectBy x allModules
  forM_ cases_to_run runCase
 where intersectBy _ [] = []
       intersectBy to ((x, f, l):xs) | x `elem` to = (x, runWithFile f l) : intersectBy to xs
                                     | otherwise   = intersectBy to xs
