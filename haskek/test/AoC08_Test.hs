module AoC08_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC08
import qualified Data.Vector as V

test_aoc08 :: TestTree
test_aoc08 = testGroup "UnitTest for AoC08" [
    testCase "[08][S] Simple case" $ assertEqual "" 20 $ aoc08  $ V.fromList [acc 10, acc 10, jmp 2, acc 10, jmp (-3)],
    testCase "[08][G] Simple case" $ assertEqual "" 20 $ aoc08s $ map V.fromList [[acc 10, nop, jmp (-2)], [acc 10, acc 10, nop]]
  ]