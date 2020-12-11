{-# LANGUAGE TupleSections #-}
module AoC10 where

import Data.Either
import Util
import Data.List
import Control.Monad.State

aoc10 :: [Int] -> Int
aoc10 jolt = let (a,b,c) = foldl (\(a,b,c) d -> (d, b + (if d-a == 1 then 1 else 0), c + (if d-a == 3 then 1 else 0))) (0,0,1) $ sort jolt in b*c

concM p = do
  l <- get
  modify $ (:) (p l)

aoc10s :: [Int] -> Int
aoc10s jolt = snd $ head $ execState (mapM (\x -> concM $ (x,) . sum . map snd . takeWhile ((>(x-4)).fst)) jolt) [(0,1)]
-- aoc10s = snd.head.flip execState[(0, 1)].mapM(modify.join.((:).).liftM2(.)(,)(((sum.map snd).).takeWhile.(.fst).flip(>).subtract 4))

runAoC10 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc10 $ fromRight [] arrOfTokens

runAoC10s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc10s $ (sort arr)

filterForToday = do
  number <* whitespace