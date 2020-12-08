module AoC08_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC08
import qualified Data.Vector as V

test_aoc08 :: TestTree
test_aoc08 = testGroup "UnitTest for AoC08" [
    testCase "[08][S] Simple case" $ assertEqual "" 20 $ aoc08  $ V.fromList [ACC 10, ACC 10, JMP 2, ACC 10, JMP (-3)],
    testCase "[08][G] Simple case" $ assertEqual "" 10 $ aoc08s $ V.fromList [ACC 10, NOP 2, JMP (-2)]
  ]