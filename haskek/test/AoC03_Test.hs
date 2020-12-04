module AoC03_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC03

test_aoc03 :: TestTree
test_aoc03 = testGroup "UnitTest for AoC03" [
    testCase "[03][S] Simple one"         $ assertEqual "" 2  $ aoc03 (1, 1) [[Tree,Tree,Tree], [Tree,Tree,Tree], [Tree,Tree,Tree]],
    testCase "[03][S] Another simple one" $ assertEqual "" 5  $ aoc03 (0, 1) [[Tree], [Tree], [Tree], [Tree], [Empty], [Tree], [Tree]],
    testCase "[03][G] Golden star"        $ assertEqual "" 16 $ aoc03s [[Tree,Tree,Tree], [Tree,Tree,Tree], [Tree,Tree,Tree]]
  ]