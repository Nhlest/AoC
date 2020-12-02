module AoC02_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC02

test_aoc02 :: TestTree
test_aoc02 = testGroup "UnitTest for AoC02" [
    testCase "[02][S] Empty test"          $ assertEqual "" 0 $ aoc02 [],
    testCase "[02][S] Silver test"         $ assertEqual "" 2 $ aoc02 [(Policy 1 3 'a', "aab"), (Policy 2 2 'b', "bbb"), (Policy 0 0 'a', "bcd")],
    testCase "[02][G] Gold test"           $ assertEqual "" 2 $ aoc02s [(Policy 1 3 'a', "aab"), (Policy 2 3 'b', "bbb"), (Policy 1 2 'a', "bad")]
  ]