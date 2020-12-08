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

aoc07 :: [BagC] -> Int
aoc07 bagdef = pred . length . filter id . M.elems $ foldl (\cache col -> uncurry (M.insert col) $ descend bagmap cache col) emptycache allcolors
  where bagmap     = M.fromList [(c, cs) | (BagC c ls) <- bagdef, let cs = [col | (Bag col _) <- ls]]
        emptycache = M.insert shinygold True M.empty
        allcolors  = M.keys bagmap

descend :: M.Map BagColor [BagColor] -> M.Map BagColor Bool -> BagColor -> (Bool, M.Map BagColor Bool)
descend bagmap bagcache color = case M.lookup color bagcache of
  Nothing  -> foldl go (False, bagcache) (fromJust $ M.lookup color bagmap)
  Just res -> (res, M.insert color res bagcache)
  where go (res, bagcache) color = 
         let (newres, newbagcache) = descend bagmap bagcache color
         in  (res || newres, M.insert color newres newbagcache)

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