module AoC06_Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import AoC06

test_aoc06 :: TestTree
test_aoc06 = testGroup "UnitTest for AoC06" [
    testCase "[06][S] Simple case" $ assertEqual "" 6 $ aoc06 [["abc", "abcd"], ["ab", "ba"]],
    testCase "[06][G] Simple case" $ assertEqual "" 5 $ aoc06s [["abc", "abcd"], ["ab", "ba"]]
  ]