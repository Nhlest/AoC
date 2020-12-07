module AoC07_Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import AoC07

test_aoc07 :: TestTree
test_aoc07 = testGroup "UnitTest for AoC07" [
    testCase "[07][S] Simple case" $ assertEqual "" 2 $ aoc07  [(BagC shinygold []), (BagC (BagColor "a" "b") [Bag shinygold 1]), (BagC (BagColor "b" "c") [Bag (BagColor "a" "b") 1])],
    testCase "[07][G] Simple case" $ assertEqual "" 110 $ aoc07s [(BagC shinygold [Bag (BagColor "a" "b") 10]), (BagC (BagColor "a" "b") [Bag (BagColor "b" "c") 10]), (BagC (BagColor "b" "c") [])]
  ]