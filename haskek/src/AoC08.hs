module AoC08 where

import Util
import Data.Either
import Data.Functor
import Data.Set
import qualified Data.Vector as V

data OPCode = 
    NOP Int
  | ACC Int
  | JMP Int
 deriving Show

type Acc = Int
type PC  = Int
data CPUState         = CPUState Acc PC (Set PC)
type Prog             = V.Vector OPCode
data TerminationState = Looped | OutOfBounds | OneAfterLast

runCPU :: Prog -> CPUState -> (TerminationState, CPUState)
runCPU prog cpustate@(CPUState acc pc past) 
  | pc `member` past  = (Looped,       cpustate)
  | pc == length prog = (OneAfterLast, cpustate)
  | pc >  length prog = (OutOfBounds,  cpustate)
  | otherwise         =  runCPU prog $ case opcode of
    NOP _ -> CPUState acc       (succ pc) newpast
    ACC a -> CPUState (acc + a) (succ pc) newpast
    JMP a -> CPUState acc       (pc + a ) newpast
  where opcode  = prog V.! pc
        newpast = insert pc past

aoc08 :: Prog -> Int
aoc08 prog = let (Looped, CPUState acc _ _) = runCPU prog cpu in acc
  where cpu = CPUState 0 0 empty

aoc08s :: Prog -> Int
aoc08s prog = go 0
 where go itoc | itoc >= V.length prog = error "can't find"
               | otherwise           = case opcode of
                 ACC _ -> go  $ succ itoc
                 NOP a -> try $ JMP a
                 JMP a -> try $ NOP a
        where cpu    = CPUState 0 0 empty
              opcode = prog V.! itoc
              try op = case runCPU (prog V.// [(itoc, op)]) cpu of
                   (OneAfterLast, CPUState acc _ _) -> acc
                   _                                -> go $ succ itoc

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