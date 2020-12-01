module AoC01_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC01

test_aoc01 :: TestTree
test_aoc01 = testGroup "UnitTest for AoC01" [
    testCase "[S] Easy test"          $ assertEqual "" (Just 2019)     $ aoc01    2020 [2019, 1],
    testCase "[S] Easy test 2"        $ assertEqual "" (Just 1020100)  $ aoc01    2020 [1010, 1010],
    testCase "[S] Harder test"        $ assertEqual "" (Just 100)      $ aoc01    20   [10, 11, 9, 10],
    testCase "[G] Test for gold star" $ assertEqual "" (Just 10100000) $ aoc01s 3 2020 [10, 13, 4124, 4124, 1000, 1010]
  ]