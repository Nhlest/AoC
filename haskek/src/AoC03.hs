module AoC03 where

import Util
import Data.Either
import Data.Functor

import Data.Maybe

data CellType = Empty | Tree deriving (Eq, Show)

getRepeated :: [[a]] -> (Int, Int) -> Maybe a
getRepeated m (x,y) | y >= length m = Nothing
                    | otherwise = Just $ m !! y !! xx
                        where xx = x `mod` length (head m)

aoc03 :: (Int, Int) -> [[CellType]] -> Int
aoc03 (sx, sy) m = length $ filter (==Tree) $ map (fromMaybe Empty) $ takeWhile isJust [getRepeated m (t*sx, t*sy) | t <- [1..]]

aoc03s :: [[CellType]] -> Int
aoc03s m = product [aoc03 (sx, sy) m | (sx, sy) <- [(1,1), (3,1), (5,1), (7,1), (1,2)]]

runAoC03 input = do
  let arrOfTokens = parseUniversal input $ do
        line <- many $ anyOf [
                  token "#" $> Tree,
                  token "." $> Empty
                ]
        whitespace
        pure line
  print $ aoc03 (3, 1) $ fromRight [] arrOfTokens
  pure ()

runAoC03s input = do
  let arrOfTokens = parseUniversal input $ do
        line <- many $ anyOf [
                  token "#" $> Tree,
                  token "." $> Empty
                ]
        whitespace
        pure line
  print $ aoc03s $ fromRight [] arrOfTokens
  pure ()