module AoC07 where

import Util
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe

data BagColor = BagColor String String
  deriving (Eq, Show)

shinygold = BagColor "shiny" "gold"

pattern SG = BagColor "shiny" "gold"

-- Todo : memoizing
aoc07 :: [BagC] -> Int
aoc07 inpt = length . filter canContainShinyGold $ inpt
  where canContainShinyGold (BagC SG _) = False
        canContainShinyGold (BagC _ []) = False
        canContainShinyGold (BagC _  l) = or $ 
          hasShinyGold l:[canContainShinyGold next | next@(BagC color _) <- inpt, hasC color l]
        hasC fcolor = any (\(Bag color _) -> color == fcolor)
        hasShinyGold = hasC shinygold

aoc07s :: [BagC] -> Int
aoc07s inpt = calculateInclusion shinygold
 where calculateInclusion color = 
         sum [(calculateInclusion newcolor + 1) * n | (Bag newcolor n) <- l]
         where (BagC _ l) = findC color inpt
               findC col = fromJust . find (\(BagC c _) -> c == col)

data BagC = BagC BagColor [Bag] deriving Show
data Bag  = Bag  BagColor Int   deriving Show

runAoC07 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc07 $ fromRight [] arrOfTokens

runAoC07s input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc07s $ fromRight [] arrOfTokens
  
filterForToday = do
  prefix1 <- word <* whitespace
  color1  <- word <* whitespace <* token "bags contain "
  containments <- anyOf [
      many1 $ do
        num    <- number <* whitespace
        prefix <- word   <* whitespace
        color  <- word   <* whitespace
        token "bag" *> many (token "s") *> anyOf [ token ".", token ", " ] *> many (token "\n")
        pure $ Bag (BagColor prefix color) num,
      token "no other bags." *> many (token "\n") $> []
    ]
  pure $ BagC (BagColor prefix1 color1) containments