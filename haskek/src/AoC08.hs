{-# LANGUAGE TupleSections #-}
module AoC08 where

import Util
import Data.Either
import Data.Functor
import Control.Monad
import Data.Set
import qualified Data.Vector as V

data OPCode = 
    NOP Int
  | ACC Int
  | JMP Int
 deriving Show

type Acc = Int
type PC  = Int
type Prog             = V.Vector OPCode
data CPUState         = CPUState Prog Acc PC (Set PC)
data TerminationState = StillRunning | Looped | OutOfBounds | OneAfterLast
newtype CPU a = CPU { execution::(CPUState -> (CPUState, a)) }

instance Functor CPU where
  fmap f (CPU d) = CPU (\state -> let (newstate, newdata) = d state in (newstate, f newdata))
instance Applicative CPU where
  pure = return
  (<*>) = ap
instance Monad CPU where
  return a = CPU (,a)
  (>>=) (CPU d) f = CPU (\state -> let (newstate, newdata) = d state in execution (f newdata) newstate)

cpu :: Prog -> CPUState
cpu prog = CPUState prog 0 0 empty
acc :: CPU Int
acc = CPU (\s@(CPUState _ a _ _) -> (s, a))
pc :: CPU Int
pc = CPU (\s@(CPUState _ _ a _) -> (s, a))
seen :: Int -> CPU Bool
seen pc = CPU (\s@(CPUState _ _ _ past) -> (s, pc `member` past))
proglen :: CPU Int
proglen = CPU (\s@(CPUState prog _ _ _) -> (s, V.length prog))
opcode :: CPU OPCode
opcode = CPU (\s@(CPUState prog _ pc _) -> (s, prog V.! pc))
advance :: Int -> CPU ()
advance s = CPU (\(CPUState a b pc c) -> (CPUState a b (pc + s) c, ()))
checkForTerminationOr :: CPU a -> CPU TerminationState
checkForTerminationOr or = do
  p <- pc
  l <- proglen
  s <- seen p
  if s then pure Looped
  else if p == l then pure OneAfterLast
  else if p >= l then pure OutOfBounds
  else          or >> pure StillRunning
add :: Int -> CPU ()
add ad = CPU (\(CPUState a acc c d) -> (CPUState a (acc + ad) c d, ()))
saw :: Int -> CPU ()
saw pc = CPU (\(CPUState a b c past) -> (CPUState a b c (insert pc past), ()))
step :: CPU TerminationState
step = do
  checkForTerminationOr $ do
    op <- opcode
    p <- pc 
    saw p
    case op of
      NOP _ ->          advance 1
      ACC a -> add a >> advance 1
      JMP a ->          advance a

runCPU :: CPU (TerminationState, Int)
runCPU = do
  ter <- step
  case ter of
    StillRunning -> runCPU
    s            -> (s,) <$> acc

run = snd . execution runCPU

aoc08 :: Prog -> Int
aoc08 prog = snd . run $ cpu prog

aoc08s :: Prog -> Int
aoc08s prog = go 0
 where go itoc | itoc >= V.length prog = error "can't find"
               | otherwise           = case opcode of
                 ACC _ -> go  $ succ itoc
                 NOP a -> try $ JMP a
                 JMP a -> try $ NOP a
        where opcode = prog V.! itoc
              try op = case run $ cpu (prog V.// [(itoc, op)]) of
                   (OneAfterLast, acc) -> acc
                   _                   -> go $ succ itoc

runAoC08 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc08 $ V.fromList $ fromRight [] arrOfTokens

runAoC08s input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc08s $ V.fromList $ fromRight [] arrOfTokens

filterForToday = do
  anyOf [
    token "nop " *> signed <&> NOP,
    token "acc " *> signed <&> ACC,
    token "jmp " *> signed <&> JMP
    ]
 where signed = do
         sign <- anyOf [token "+" $> "", token "-"]
         n    <- word
         anyOf [token "\n" $> (), endofstream]
         pure $ read $ sign <> n