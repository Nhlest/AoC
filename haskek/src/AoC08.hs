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

newtype StackTrace = StackTrace [String] deriving Show

instance Semigroup StackTrace where
  (<>) (StackTrace a) (StackTrace b) = StackTrace (take 5 $ b <> a)

instance Monoid StackTrace where
  mempty = StackTrace []

newtype CPU a = CPU { runCPU :: Prog -> CPUState -> ((a, CPUState), StackTrace) }
  deriving (Functor, Applicative, Monad, MonadReader Prog, MonadState CPUState, MonadWriter StackTrace) via (ReaderT Prog (StateT CPUState (Writer StackTrace)))

opcode :: CPU OPCode
opcode = do
  (CPUState _ pc _) <- get
  prog <- ask
  pure $ prog V.! pc

advance :: Int -> CPU ()
advance n = modify (\s -> s {_pc = _pc s + n})

trace :: String -> CPU ()
trace t = do
  CPUState acc pc _ <- get
  tell $ StackTrace ["[" <> show pc <> "] {" <> show acc <> "} " <> t]

nop :: OPCode
nop = checkTermination $ do
  trace "nop"
  advance 1

acc :: Int -> OPCode
acc a = checkTermination $ do
  trace $ "acc " <> show a
  modify (\s -> s {_acc = _acc s + a})
  advance 1

jmp :: Int -> CPU TerminationState
jmp a = checkTermination $ do
  trace $ "jmp " <> show a
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
  if      pc == V.length prog then trace "!!OneAfterLast!!" >> pure OneAfterLast
  else if pc >  V.length prog then trace "!!OutOfBounds!!" >> pure OutOfBounds
  else if loop                then trace "!!Looped!!" >> pure Looped
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


aoc08b prog = let StackTrace a = snd $ runCPU (progressTill Looped) prog emptycpu in unlines $ reverse a
aoc08sb progs = let StackTrace a = snd $ head $ filter (\((a, b), c) -> isJust a) [runCPU (progressTill OneAfterLast) p emptycpu | p <- progs] in unlines $ reverse a

aoc08s :: [Prog] -> Int
aoc08s progs = head $ catMaybes [fst . fst $ runCPU (progressTill OneAfterLast) p emptycpu | p <- progs]

runAoC08 input = do
  let arrOfTokens = parseUniversal input filterForToday
  putStrLn $ aoc08b $ V.map fst $ V.fromList $ fromRight [] arrOfTokens

runAoC08s input = do
  let arrOfTokens = parseUniversal input filterForToday
  putStrLn $ aoc08sb $ map (V.fromList . (\(a, b, c) -> map fst a ++ [snd b] ++ map fst c)) $ splitEverywhere $ fromRight [] arrOfTokens

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