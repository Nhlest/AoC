module AoC07 where

import Util
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M

data BagColor = BagColor String String
  deriving (Eq, Show, Ord)

shinygold = BagColor "shiny" "gold"

pattern SG = BagColor "shiny" "gold"

aoc07 :: [BagC] -> Int
aoc07 inpt = length . filter canContainShinyGold $ inpt
  where canContainShinyGold (BagC SG _) = False
        canContainShinyGold (BagC _ []) = False
        canContainShinyGold (BagC _  l) = or $ 
          hasShinyGold l:[canContainShinyGold next | next@(BagC color _) <- inpt, hasC color l]
        hasC fcolor = any (\(Bag color _) -> color == fcolor)
        hasShinyGold = hasC shinygold

-- Still shit but works, add caching on the very top level
aoc07c :: [BagC] -> Int
aoc07c bagdef = pred . length . filter fst $ [descend bagmap (M.insert shinygold True emptycache) a | (BagC a _) <- bagdef]
  where bagmap = M.fromList [(color, colors) | (BagC color contains) <- bagdef, let colors = [col | (Bag col _) <- contains]]
        emptycache = M.empty

descend :: M.Map BagColor [BagColor] -> M.Map BagColor Bool -> BagColor -> (Bool, M.Map BagColor Bool)
descend bagmap bagcache color = case M.lookup color bagcache of
                                  Nothing -> foldl go (False, bagcache) (fromJust $ M.lookup color bagmap)
                                  Just res -> (res, M.insert color res bagcache)
  where go (res, bagcache) color = let (newres, newbagcache) = descend bagmap bagcache color
                                   in (res || newres, newbagcache)

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

runAoC07c input = do
  let arrOfTokens = parseUniversal input filterForToday
  print $ aoc07c $ fromRight [] arrOfTokens
  
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