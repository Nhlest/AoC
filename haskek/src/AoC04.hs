module AoC04 where

import Util
import Data.Either
import Data.Functor
import Data.Char

data PassField = INVLD | BRK | BYR | IYR | EYR | HGT | HCL | ECL | PID | CID deriving (Show, Eq)
type Document = [PassField]

aoc04 :: [Document] -> Int
aoc04 ilist = length $ filter (\a -> notElem INVLD a && (length a == 8 || length a == 7 && CID `notElem` a)) ilist

isHex h = isDigit h || (h >= 'a' && h <= 'f')

runAoC04 input = do
  let arrOfTokens = parseUniversal input $ do
        document <- many $ anyOf [
              token "byr:" *> word *> spaceornewline $> BYR,
              token "iyr:" *> word *> spaceornewline $> IYR,
              token "eyr:" *> word *> spaceornewline $> EYR,
              token "hgt:" *> word *> spaceornewline $> HGT,
              token "hcl:" *> many (token "#") *> word *> spaceornewline $> HCL,
              token "ecl:" *> many (token "#") *> word *> spaceornewline $> ECL,
              token "pid:" *> many (token "#") *> word *> spaceornewline $> PID,
              token "cid:" *> word *> spaceornewline $> CID
            ]
        anyOf [token "\n" $> (), endofstream]
        pure document
  print $ aoc04 $ fromRight [] arrOfTokens
  pure ()
 where spaceornewline = anyOf [token " " $> (), token "\n" $> (), endofstream]

data Unit = CM Int | IN Int | INV

runAoC04s input = do
  let arrOfTokens = parseUniversal input $ do
        document <- many $ anyOf [
              token "byr:" *> anyOf [number                   ?> byr *> spaceornewline $> BYR, discardrest],
              token "iyr:" *> anyOf [number                   ?> iyr *> spaceornewline $> IYR, discardrest],
              token "eyr:" *> anyOf [number                   ?> eyr *> spaceornewline $> EYR, discardrest],
              token "hgt:" *> anyOf [parseHeight              ?> hgt *> spaceornewline $> HGT, discardrest],
              token "hcl:" *> anyOf [token "#" *> wordF isHex ?> hcl *> spaceornewline $> HCL, discardrest],
              token "ecl:" *> anyOf [word                     ?> ecl *> spaceornewline $> ECL, discardrest],
              token "pid:" *> anyOf [wordF isDigit            ?> pid *> spaceornewline $> PID, discardrest],
              token "cid:" *> anyOf [wordF (not . isWhitespace)      *> spaceornewline $> CID
              ]
            ]
        anyOf [token "\n" $> (), endofstream]
        pure document
  print $ aoc04 $ fromRight [] arrOfTokens
  pure ()
 where spaceornewline = anyOf [token " " $> (), token "\n" $> (), endofstream]
       discardrest = wordF (not . isWhitespace) *> spaceornewline $> INVLD
       byr b = b >= 1920 && b <= 2002
       iyr b = b >= 2010 && b <= 2020
       eyr b = b >= 2020 && b <= 2030
       parseHeight = do
         h <- number
         t <- word
         pure $ case t of
           "cm" -> CM h
           "in" -> IN h
           _    -> INV
       hgt (CM h) = h >= 150 && h <= 193
       hgt (IN h) = h >= 59  && h <= 76
       hgt INV    = False
       hcl cl = length cl == 6
       ecl "amb" = True
       ecl "blu" = True
       ecl "brn" = True
       ecl "gry" = True
       ecl "grn" = True
       ecl "hzl" = True
       ecl "oth" = True
       ecl _ = False
       pid p = length p == 9