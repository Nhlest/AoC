module AoC15 where

import Util
import Data.Either
import qualified Data.IntMap.Strict as M
import Control.Monad.State
import Data.Functor
import Data.Maybe

continueToLen :: Int -> [Int] -> Int -> [Int]
continueToLen l list@(x:xs) len | l == len = list
                                | otherwise = continueToLen (l+1) (next:list) len
  where next = fromMaybe 0 $ findDistance xs x
        findDistance [] _ = Nothing
        findDistance (x:xs) y | x == y = Just 1
                              | otherwise = (+1) <$> findDistance xs y

-- continueToLenState :: [Int] -> Int -> 
continueToLenState list len = evalState (go (length list+1) 0) cache
  where cache = foldl (\m (a,b) -> M.insert a b m) M.empty $ zip list [0..]
        go :: Int -> Int -> State (M.IntMap Int) Int
        go cur prev = do
          cache <- get
          let next = M.lookup prev cache
          let (cache_new,n) = case next of 
                Nothing -> (M.insert prev (cur-1) cache, 0)
                Just a -> (M.insert prev (cur-1) cache, cur-a-1)
          put cache_new
          if cur == len then
            pure $ prev
          else go (cur + 1) n

aoc15 :: [Int] -> Int
aoc15 a = head $ continueToLen (length a) (reverse a) 2020

aoc15s :: [Int] -> Int
aoc15s a = continueToLenState (a) 30000000

runAoC15 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc15 $ arr

runAoC15s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc15s $ arr

filterForToday = do
  number <* anyOf [token "," $> (), endofstream]