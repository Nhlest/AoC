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
import AoC12
import AoC13
import AoC14
import AoC15
import AoC16
import AoC17
import AoC18
import AoC19
import AoC20
import AoC21
import AoC22
import AoC23
import AoC24
import AoC25
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
              ("aoc11",    runAoC11,    "aoc11"),
              ("aoc12s",   runAoC12s,   "aoc12"),
              ("aoc12",    runAoC12,    "aoc12"),
              ("aoc13s",   runAoC13s,   "aoc13"),
              ("aoc13",    runAoC13,    "aoc13"),
              ("aoc14s",   runAoC14s,   "aoc14"),
              ("aoc14",    runAoC14,    "aoc14"),
              ("aoc15s",   runAoC15s,   "aoc15"),
              ("aoc15",    runAoC15,    "aoc15"),
              ("aoc16",    runAoC16,    "aoc16"),
              ("aoc16s",   runAoC16s,   "aoc16"),
              ("aoc17s",   runAoC17s,   "aoc17"),
              ("aoc17",    runAoC17,    "aoc17"),
              ("aoc18s",   runAoC18s,   "aoc18"),
              ("aoc18",    runAoC18,    "aoc18"),
              ("aoc19s",   runAoC19s,   "aoc19"),
              ("aoc19",    runAoC19,    "aoc19"),
              ("aoc20s",   runAoC20s,   "aoc20"),
              ("aoc20",    runAoC20,    "aoc20"),
              ("aoc21s",   runAoC21s,   "aoc21"),
              ("aoc21",    runAoC21,    "aoc21"),
              ("aoc22s",   runAoC22s,   "aoc22"),
              ("aoc22",    runAoC22,    "aoc22"),
              ("aoc23s",   runAoC23s,   "aoc23"),
              ("aoc23",    runAoC23,    "aoc23"),
              ("aoc24s",   runAoC24s,   "aoc24"),
              ("aoc24",    runAoC24,    "aoc24"),
              ("aoc25s",   runAoC25s,   "aoc25"),
              ("aoc25",    runAoC25,    "aoc25")
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
