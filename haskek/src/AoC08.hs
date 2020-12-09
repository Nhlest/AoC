{-# LANGUAGE TupleSections, DeriveFunctor, DerivingVia, MultiParamTypeClasses #-}
module AoC08 where

import Data.Set (empty, insert, member, Set)
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Identity
import qualified Data.Vector as V
import Util
import Data.Functor
import Data.Either
import Data.Maybe


type OPCode = CPU TerminationState
type Acc    = Int
type PC     = Int
type Prog             = V.Vector OPCode
data CPUState         = CPUState { _acc::Acc, _pc::PC, _past::Set PC } deriving Show
data TerminationState = StillRunning | Looped | OutOfBounds | OneAfterLast deriving (Show, Eq)

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

nop :: OPCode
nop = checkTermination $ do
    advance 1

acc :: Int -> OPCode
acc a = checkTermination $ do
  modify (\s -> s {_acc = _acc s + a})
  advance 1

jmp :: Int -> CPU TerminationState
jmp a = checkTermination $ do
    advance a

seen :: PC -> CPU Bool
seen pc = do
  CPUState _ _ past <- get
  pure $ pc `member` past

checkTermination :: CPU () -> CPU TerminationState
checkTermination action = do
  action
  CPUState _ pc _ <- get
  prog <- ask
  loop <- seen pc
  if      pc == V.length prog then pure OneAfterLast
  else if pc >  V.length prog then pure OutOfBounds
  else if loop                then pure Looped
  else                             pure StillRunning

saw :: PC -> CPU ()
saw pc = do
  modify (\s -> s {_past = pc `insert` _past s})

progress :: CPU TerminationState
progress = do
  op <- opcode
  CPUState _ pc _ <- get
  saw pc
  op

progressTill :: TerminationState -> CPU (Maybe Int)
progressTill k = do
  s <- progress
  CPUState acc _ _ <- get
  if      s == StillRunning then progressTill k
  else if k == s            then pure $ Just acc
  else                           pure Nothing

emptycpu = CPUState 0 0 empty

aoc08 :: Prog -> Int
aoc08 prog = fromJust . fst . fst $ runCPU (progressTill Looped) prog emptycpu

aoc08s :: [Prog] -> Int
aoc08s progs = head $ catMaybes [fst . fst $ runCPU (progressTill OneAfterLast) p emptycpu | p <- progs]

runAoC08 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc08 $ V.map fst $ V.fromList $ fromRight [] arrOfTokens

runAoC08s input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc08s $ map (V.fromList . (\(a, b, c) -> map fst a ++ [snd b] ++ map fst c)) $ splitEverywhere $ fromRight [] arrOfTokens

filterForToday = do
  anyOf [
    token "nop " *> signed <&> (\a -> (nop,   jmp a)),
    token "acc " *> signed <&> (\a -> (acc a, acc a)),
    token "jmp " *> signed <&> (\a -> (jmp a, nop  ))
    ]
 where signed = do
         sign <- anyOf [token "+" $> "", token "-"]
         n    <- word
         anyOf [token "\n" $> (), endofstream]
         pure $ read $ sign <> n