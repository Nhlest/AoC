module Main where

import Test.Tasty

import AocTest_Test
import AoC01_Test
import AoC02_Test
import AoC03_Test
import AoC04_Test
import AoC05_Test

tests :: TestTree
tests = testGroup "Tests" [test1, test2, test_aoc01, test_aoc02, test_aoc03, test_aoc04, test_aoc05, testParser]

main = defaultMain tests