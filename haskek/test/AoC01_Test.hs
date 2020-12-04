module AoC01_Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Maybe
import AoC01

test_aoc01 :: TestTree
test_aoc01 = testGroup "UnitTest for AoC01" [
    testCase "[01][S] False negative"     $ assertEqual "" (Just ([2019, 1], 2019)) $ aoc01 2020 [2019, 1],
    testCase "[01][S] Easy test"          $ assertBool ""   $ satisfies aoc01      2 2020 [2019, 1],
    testCase "[01][S] Easy test 2"        $ assertBool ""   $ satisfies aoc01      2 2020 [1010, 1010],
    testCase "[01][S] Harder test"        $ assertBool ""   $ satisfies aoc01      2 20   [10, 11, 9, 10],
    testCase "[01][G] Test for gold star" $ assertBool ""   $ satisfies (aoc01s 3) 3 2020 [10, 13, 4124, 4124, 1000, 1010],
    QC.testProperty "[01][S] QuickChecking by property"   $ satisfies aoc01 2 10,
    QC.testProperty "[01][G] QuickChecking by property 2" $ satisfies (aoc01s 3) 3
  ]
 where satisfies f targetn targets list | length list < targetn = isNothing $ f targets list
                                        | otherwise = case f targets list of
                                             Nothing -> True
                                             Just (ys, yp) ->
                                                       and (flip elem list <$> ys)
                                                    && sum ys == targets
                                                    && product ys == yp