module AoC05 where

import Util
import Data.Either
import Data.Functor
import Data.List

data FB = F | B deriving Show
data LR = L | R deriving Show

class Binaryable a where
  oneOrZero :: a -> Int

instance Binaryable FB where
  oneOrZero F = 0
  oneOrZero B = 1
instance Binaryable LR where
  oneOrZero L = 0
  oneOrZero R = 1

type Pass = ([FB], [LR])

aoc05 :: [Pass] -> Int
aoc05 = foldr (max . toPassID) 0

aoc05s :: [Pass] -> Int
aoc05s passes = findMissing $ sort $ toPassID <$> passes
  where findMissing (l:ls) = go l ls
        go l [] = error "can't find"
        go l (x:xs) | l + 1 == x = go x xs
                    | otherwise = l + 1

toPassID :: Pass -> Int
toPassID (fb, lr) = 8 * fromBinary fb + fromBinary lr

fromBinary :: Binaryable a => [a] -> Int
fromBinary b = go $ reverse b
  where go [] = 0
        go (x:xs) = oneOrZero x + 2 * go xs

runAoC05 input = do
  let arrOfTokens = parseUniversal input $ do
        pass_fb <- many $ anyOf [
                  token "F" $> F, token "B" $> B
                ]
        pass_lr <- many $ anyOf [
                  token "L" $> L, token "R" $> R
                ]
        whitespace
        pure (pass_fb, pass_lr)
  print $ aoc05 $ fromRight [] arrOfTokens
  pure ()

runAoC05s input = do
  let arrOfTokens = parseUniversal input $ do
        pass_fb <- many $ anyOf [
                  token "F" $> F, token "B" $> B
                ]
        pass_lr <- many $ anyOf [
                  token "L" $> L, token "R" $> R
                ]
        whitespace
        pure (pass_fb, pass_lr)
  print $ aoc05s $ fromRight [] arrOfTokens
  pure ()