module AoC03_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC03

test_aoc03 :: TestTree
test_aoc03 = testGroup "UnitTest for AoC02" [
    testCase "[03][S] Empty test"          $ assertEqual "" 0 $ aoc03 [],
    testCase "[03][S] Silver test"         $ assertEqual "" 2 $ aoc03 [(Policy 1 3 'a', "aab"), (Policy 2 2 'b', "bbb"), (Policy 0 0 'a', "bcd")],
    testCase "[03][G] Gold test"           $ assertEqual "" 2 $ aoc03s [(Policy 1 3 'a', "aab"), (Policy 2 3 'b', "bbb"), (Policy 1 2 'a', "bad")]
  ]