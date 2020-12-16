module AoC16 where

import Util
import Data.Either
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Data.List

data Range = Range Int Int deriving Show
type Name = String
data Rule = Rule Name Range Range deriving Show
type Ticket = [Int]
data ScanResult = ScanResult [Rule] Ticket [Ticket]

isValid :: Rule -> Int -> Bool
isValid (Rule _ low high) tocheck = inside low tocheck || inside high tocheck
  where inside (Range l h) t = l <= t && t <= h

isValidTicket :: [Rule] -> Ticket -> Bool
isValidTicket rules ticket = foldl (\s field -> (foldl (\ss rule -> (isValid rule field) || ss) False rules) && s) True ticket

aoc16 :: ScanResult -> Int
aoc16 (ScanResult rules your other) = foldl errrate 0 other
  where errrate s ticket = foldl (\ss ticketfield -> 
                             case foldl (\k n -> k || (isValid n ticketfield)) False rules of
                             True -> ss
                             False -> ss + ticketfield
                             ) s ticket

-- aoc16s :: ScanResult -> Int
aoc16s (ScanResult rules your other) = sumAllDeparture validforrules your --sumAllDeparture (fromJust ruleset) your
  where valids = filter (isValidTicket rules) other
        numoffields = length $ head valids
        allValid rule ticket = foldl (\s f -> s && isValid rule f) True ticket
        validforrules = reduce_ruleset [] $ sortBy (\(_,a) (_,b) -> compare (length a) (length b)) $ map (\r -> (r, [i | i <- [0..numoffields-1], allValid r (map (!!i) valids)])) rules
        reduce_ruleset _ []                            = []
        reduce_ruleset taken ((Rule name _ _, idc):rs) 
          = let i = head $ filter (`notElem` taken) idc
            in (i, name):reduce_ruleset (i:taken) rs

        sumAllDeparture [] your                                                  = 1
        sumAllDeparture ((idx, ('d':'e':'p':'a':'r':'t':'u':'r':'e':_)):rs) your = 
          your !! idx * sumAllDeparture rs your
        sumAllDeparture (_:rs) your                                              = sumAllDeparture rs your

runAoC16 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc16 $ head $ arr

runAoC16s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc16s $ head $ arr

filterForToday = do
  rules <- many $ do
    name <- wordF1 (/=':')
    token ": "
    r1 <- number
    token "-"
    r2 <- number
    token " or "
    r3 <- number
    token "-"
    r4 <- number
    whitespace
    pure $ Rule name (Range r1 r2) (Range r3 r4)
  whitespace
  token "your ticket:"
  whitespace
  your <- ticket
  whitespace
  token "nearby tickets:"
  whitespace
  other <- many (ticket <* whitespace)
  pure $ ScanResult rules your other
 where ticket = do many $ number <* many (token ",") 