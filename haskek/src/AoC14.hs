module AoC14 where

import Util
import Data.Either
import Data.Vector (Vector, fromList)
import qualified Data.Map as M
import Control.Monad.State
import Data.Bits

data Bit = B1 | B0 | BX deriving (Show, Eq)
toBit '0' = B0
toBit '1' = B1
toBit 'X' = BX
type Mask = [Bit]
type Addr  = Int
type Value = Int
data Instruction = IMask Mask | IWrite Addr Value deriving (Show)

withMask :: Value -> Mask -> Value
withMask val mask = foldl go val $ zip [35,34..] mask
  where go val (bit, B1) = setBit val bit
        go val (bit, B0) = clearBit val bit
        go val (bit, BX) = val

-- aoc14 :: [Instruction] -> Int
aoc14 :: [Instruction] -> Int
aoc14 a = evalState (go a mask) mem
  where mem = M.empty
        mask = replicate 36 BX
        go [] _ = do
          gets $ M.foldr (+) 0
        go ((IMask newmask) :xs) _    = go xs newmask
        go ((IWrite add val):xs) mask = do
          mem <- get
          let newval = withMask val mask
          put $ M.insert add newval mem
          go xs mask

withMaskM :: Addr -> Mask -> [Addr]
withMaskM addr mask = multiply [setted] onlyxs
  where z = zip [35,34..] mask
        onlysets = filter ((/=BX) . snd) z
        onlyxs = filter ((==BX) . snd) z
        setted = foldl go addr onlysets
        go val (bit, B1) = setBit val bit
        go val (bit, B0) = val
        go val (bit, BX) = val
        multiply lst [] = lst
        multiply lst ((bit, BX):xs) = multiply (lst >>= (\a -> [setBit a bit, clearBit a bit])) xs

aoc14s :: [Instruction] -> Int
aoc14s a = evalState (go a mask) mem
  where mem = M.empty
        mask = replicate 36 BX
        go [] _ = do
          gets $ M.foldr (+) 0
        go ((IMask newmask) :xs) _    = go xs newmask
        go ((IWrite add val):xs) mask = do
          mem <- get
          let newmem = withMaskM add mask
          put $ foldl (\m addr -> M.insert addr val m) mem newmem
          go xs mask

runAoC14 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc14 $ arr

runAoC14s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc14s $ arr

toMask :: String -> [Bit]
toMask m = map toBit m

filterForToday = do
  anyOf [
    do 
      token "mask"
      token " = "
      IMask . toMask <$> word,
    do
      token "mem["
      addr <- number
      token "] = "
      val <- number
      pure $ IWrite addr val
    ] <* whitespace