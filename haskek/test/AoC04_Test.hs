module AoC04_Test where

import Test.Tasty
import Test.Tasty.HUnit
import AoC04

test_aoc04 :: TestTree
test_aoc04 = testGroup "UnitTest for AoC04" [
    testCase "[04][S] Empty case"  $ assertEqual "" 0 $ aoc04 [],
    testCase "[04][S] Silver case" $ assertEqual "" 2 $ aoc04 [[BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, PID], [BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID]],
    testCase "[04][G] Golden case" $ assertEqual "" 3 $ aoc04 [[BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, PID], [BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID], [INVLD, BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID, CID], [BRK, BYR, IYR, EYR, HGT, HCL, PID], [INVLD, BRK, BYR, IYR, EYR, HGT, HCL, ECL, PID]]
  ]