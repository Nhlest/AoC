module AoC06 where

import Util
import Data.Either
import Data.Functor
import Data.List
import Data.Char

aoc06 :: [[String]] -> Int
aoc06 = sum . map (length . nub . concat)

aoc06s :: [[String]] -> Int
aoc06s = sum . map (length . nub . foldr1 intersect)

runAoC06 input = do
  let arrOfTokens = parseUniversal input $ do
        res <- many1 $ do
              r <- wordF1 isAlphaNum
              anyOf [token "\n" $> (), endofstream]
              pure r
        anyOf [token "\n" $> (), endofstream]
        pure res
  print $ aoc06 $ fromRight [] arrOfTokens

runAoC06s input = do
  let arrOfTokens = parseUniversal input $ do
        res <- many1 $ do
              r <- wordF1 isAlphaNum
              anyOf [token "\n" $> (), endofstream]
              pure r
        anyOf [token "\n" $> (), endofstream]
        pure res
  print $ aoc06s $ fromRight [] arrOfTokens