module AoC05_Test where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import AoC05

test_aoc05 :: TestTree
test_aoc05 = testGroup "UnitTest for AoC05" [
    testCase        "[05][S] Simple case"      $ assertEqual "" 42  $ aoc05  [([F,B,F,B], [L,R,L])],
    testCase        "[05][S] Double case"      $ assertEqual "" 106 $ aoc05  [([F,B,F,B], [L,R,L]),([B,B,F,B], [L,R,L])],
    testCase        "[05][G] Gold case"        $ assertEqual "" 3   $ aoc05s [([F], [L,L,L]),([F], [L,L,R]),([F], [L,R,L]),([F], [R,L,L]),([F], [R,L,R])],
    QC.testProperty "[05][G] Property testing" $ forAll gen (\(l, k) -> k == aoc05s l)
  ]
 where gen = do
         a::Int <- choose (0,1014)
         b      <- choose (a+2,1024)
         c      <- choose (a+1,b-1)
         let list = [t | t <- [a..b], t /= c]
         pure (toBPass 0 ([], []) <$> list, c)
       toBPass _ (a, b) 0 = (a, b)
       toBPass k (a, b) l | k < 3     = let bn = l `mod` 2; lnext = l `div` 2 in toBPass (k + 1) (a, (if bn == 0 then L else R):b) lnext
                          | otherwise = let an = l `mod` 2; lnext = l `div` 2 in toBPass (k + 1) ((if an == 0 then F else B):a, b) lnext

