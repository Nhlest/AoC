module AoC07 where

import Util
import Data.Either

data BagColor = BagColor String String
  deriving (Eq, Show)

shinygold = BagColor "shiny" "gold"

-- Todo : memoizing
aoc07 :: [BagC] -> Int
aoc07 inpt = length . filter canContainShinyGold $ inpt
  where canContainShinyGold (BagC (BagColor "shiny" "gold") _) = False
        canContainShinyGold (BagC _                        []) = False
        canContainShinyGold (BagC _                         l) = or $ hasShinyGold l:[canContainShinyGold next | next@(BagC color _) <- inpt, hasC l color]
        hasC [] _ = False
        hasC ((Bag color _):xs) tocompare | tocompare == color = True
                                          | otherwise = hasC xs tocompare
        hasShinyGold l = hasC l shinygold

aoc07s :: [BagC] -> Int
aoc07s inpt = calculateInclusion shinygold
 where calculateInclusion color = sum [(calculateInclusion newcolor + 1) * n | (Bag newcolor n) <- l]
         where (BagC _ l) = find inpt color
               find [] _ = error "can't find"
               find (a@(BagC c _):xs) color | color == c = a
                                            | otherwise = find xs color

data BagC = BagC BagColor [Bag] deriving Show
data Bag  = Bag  BagColor Int   deriving Show

runAoC07 input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc07 $ fromRight [] arrOfTokens

runAoC07s input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc07s $ fromRight [] arrOfTokens
  
filterForToday = do
  prefix1 <- word
  whitespace
  color1  <- word
  whitespace
  token "bags contain "
  containments <- anyOf [
      many1 $ do
        num <- number
        whitespace
        prefix <- word
        whitespace
        color  <- word
        whitespace
        token "bag"
        many (token "s")
        anyOf [
          token ".",
          token ", "
          ]
        many (token "\n")
        pure $ Bag (BagColor prefix color) num,
      do 
        token "no other bags."
        many (token "\n")
        pure []
    ]
  pure $ BagC (BagColor prefix1 color1) containments