module Main where

import Test.Tasty

import AocTest_Test
import AoC01_Test

tests :: TestTree
tests = testGroup "Tests" [test1, test2, test_aoc01]

main = defaultMain tests