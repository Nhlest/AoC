module Main where

import Test.Tasty

import AocTest_Test
import AoC01_Test
import AoC02_Test

tests :: TestTree
tests = testGroup "Tests" [test1, test2, test_aoc01, test_aoc02]

main = defaultMain tests