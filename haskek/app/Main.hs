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
import AoC07
import AoC08
import AoC09
import AoC10
import AoC11
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
              ("aoc06s",   runAoC06s,   "aoc06"),
              ("aoc07",    runAoC07,    "aoc07"),
              ("aoc07s",   runAoC07s,   "aoc07"),
              ("aoc08",    runAoC08,    "aoc08"),
              ("aoc08s",   runAoC08s,   "aoc08"),
              ("aoc09s",   runAoC09s,   "aoc09"),
              ("aoc09",    runAoC09,    "aoc09"),
              ("aoc10s",   runAoC10s,   "aoc10"),
              ("aoc10",    runAoC10,    "aoc10"),
              ("aoc11s",   runAoC11s,   "aoc11"),
              ("aoc11",    runAoC11,    "aoc11")
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
