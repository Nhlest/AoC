module AoC12 where

import Data.Either
import Util

data CommandDirection = F | L | R | N | S | E | W deriving (Eq, Show, Read)
type Command = (CommandDirection, Int)

next N = E
next E = S
next S = W
next W = N

aoc12 :: [Command] -> Int
aoc12 = go (0,0) E
  where go (x,y) _   []            = abs x+abs y
        go (x,y) dir ((N, m)  :ds) = go (x, y-m) dir               ds
        go (x,y) dir ((E, m)  :ds) = go (x+m, y) dir               ds
        go (x,y) dir ((S, m)  :ds) = go (x, y+m) dir               ds
        go (x,y) dir ((W, m)  :ds) = go (x-m, y) dir               ds
        go (x,y) dir ((F, m)  :ds) = go (x, y)   dir      ((dir,m):ds)
        go (x,y) dir ((L, deg):ds) = go (x, y)   dir ((R, 360-deg):ds)
        go (x,y) dir ((R, deg):ds) = go (x, y)   (newdir dir deg)  ds
        newdir dir 0 = dir
        newdir dir deg = newdir (next dir) (deg - 90)

aoc12s :: [Command] -> Int
aoc12s = go (0,0) (10, 1)
  where go (x,y) _        []            = abs x+abs y
        go (x,y) (wx, wy) ((N, m)  :ds) = go (x, y)               (wx, wy+m)             ds
        go (x,y) (wx, wy) ((E, m)  :ds) = go (x, y)               (wx+m, wy)             ds
        go (x,y) (wx, wy) ((S, m)  :ds) = go (x, y)               (wx, wy-m)             ds
        go (x,y) (wx, wy) ((W, m)  :ds) = go (x, y)               (wx-m, wy)             ds
        go (x,y) (wx, wy) ((F, m)  :ds) = go (x+(wx*m), y+(wy*m)) (wx, wy)               ds
        go (x,y) (wx, wy) ((L, deg):ds) = go (x, y)               (wx, wy) ((R, 360-deg):ds)
        go (x,y) (wx, wy) ((R, deg):ds) = go (x, y)               (newwp (wx, wy) deg)   ds
        newwp (wx, wy) 0   = (wx, wy)
        newwp (wx, wy) deg = newwp (wy, -wx) (deg - 90)

runAoC12 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc12 arr

runAoC12s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc12s arr

filterForToday = do
  whitespace
  c <- read <$> anyOf (map (token . pure) "NESWFLR")
  n <- number
  pure (c,n)
  
    