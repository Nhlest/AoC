{-# LANGUAGE TupleSections, DeriveFunctor, DerivingVia, MultiParamTypeClasses #-}
module AoC08 where

import Data.Set
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Identity
import qualified Data.Vector as V

data OPCode = 
    NOP Int
  | ACC Int
  | JMP Int
 deriving Show

type Acc = Int
type PC  = Int
type Prog             = V.Vector OPCode
data CPUState         = CPUState { _acc::Acc, _pc::PC, _past::Set PC } deriving Show
data TerminationState = StillRunning | Looped | OutOfBounds | OneAfterLast deriving Show

newtype StackTrace = StackTrace String 
  deriving (Semigroup, Monoid, Show) via String

newtype CPU a = CPU { runCPU :: Prog -> CPUState -> ((a, CPUState), StackTrace) }
  deriving (Functor, Applicative, Monad, MonadReader Prog, MonadState CPUState) via (ReaderT Prog (StateT CPUState (Writer StackTrace)))

instance MonadWriter StackTrace CPU where
  tell what      = CPU (\prog state -> (((), state), what <> StackTrace "\n"))
  listen (CPU a) = CPU (\prog state -> let ((res, st), tr) = a prog state in (((res, tr), st), tr <> StackTrace "\n"))
  pass (CPU a)   = CPU (\prog state -> let (((res, func), newstate), tr) = a prog state in ((res, newstate), func tr <> StackTrace "\n"))

opcode :: CPU OPCode
opcode = do
  (CPUState _ pc _) <- get
  prog <- ask
  pure $ prog V.! pc

advance :: Int -> CPU ()
advance n = modify (\s -> s {_pc = _pc s + n})

progress :: CPU (TerminationState, Int)
progress = do
  op <- opcode
  CPUState acc _ _ <- get

  case op of
    NOP _ -> advance 1 >> pure (StillRunning, acc)
    ACC a -> modify (\s -> s {_acc = acc + a}) >> pure (StillRunning, acc + a)
    JMP a -> advance a >> pure (StillRunning, acc)

dostuff = runCPU opcode rd st
  where st = CPUState 0 0 empty
        rd = V.fromList [NOP 2]



-- runCPU :: CPU (TerminationState, Int)
-- runCPU = do
--   ter <- step
--   case ter of
--     StillRunning -> runCPU
--     s            -> (s,) <$> acc

-- run = snd . execution runCPU

-- aoc08 :: Prog -> Int
-- aoc08 prog = snd . run $ cpu prog

-- aoc08s :: Prog -> Int
-- aoc08s prog = go 0
--  where go itoc | itoc >= V.length prog = error "can't find"
--                | otherwise           = case opcode of
--                  ACC _ -> go  $ succ itoc
--                  NOP a -> try $ JMP a
--                  JMP a -> try $ NOP a
--         where opcode = prog V.! itoc
--               try op = case run $ cpu (prog V.// [(itoc, op)]) of
--                    (OneAfterLast, acc) -> acc
--                    _                   -> go $ succ itoc

-- runAoC08 input = do
--   let arrOfTokens = parseUniversal input filterForToday
--   print $ aoc08 $ V.fromList $ fromRight [] arrOfTokens

-- runAoC08s input = do
--   let arrOfTokens = parseUniversal input filterForToday
--   print $ aoc08s $ V.fromList $ fromRight [] arrOfTokens

-- filterForToday = do
--   anyOf [
--     token "nop " *> signed <&> NOP,
--     token "acc " *> signed <&> ACC,
--     token "jmp " *> signed <&> JMP
--     ]
--  where signed = do
--          sign <- anyOf [token "+" $> "", token "-"]
--          n    <- word
--          anyOf [token "\n" $> (), endofstream]
--          pure $ read $ sign <> n