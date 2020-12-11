module AoC11 where

import Data.Either
import Util
import Data.Functor
import Data.Vector (fromList, Vector, (!?), (!))
import Data.Maybe

data CellState = Empty | CEmpty | COccupied deriving Eq
type Ferry = Vector (Vector CellState)

nbrs :: Ferry -> (Int, Int) -> Int
nbrs f (x, y) = length $ filter (==COccupied) nlist
  where nlist = catMaybes [(!? xi) =<< (f !? yi) | xi <- [x-1..x+1], yi <- [y-1..y+1], xi /= x || yi /= y]

nbrslong :: Ferry -> (Int, Int) -> Int
nbrslong f (x, y) = length $ filter (==COccupied) nlist
  where nlist = [raytrace (x, y) (xi, yi) | xi <- [-1..1], yi <- [-1..1], xi /= 0 || yi /= 0]
        raytrace (x, y) (xi, yi) = let l = dropWhile (==Empty) $ catMaybes $ takeWhile isJust [f !? (y+i*yi) >>= (!? (x+i*xi)) | i <- [1..]]
                                   in if null l then Empty
                                      else head l

nextstep :: Ferry -> (Int, Bool) -> Ferry
nextstep f (todie, uselong) = fromList $ fromList <$> [[calc xi yi | xi <- [0..length (f ! 0)-1]] | yi <- [0..length f-1]]
  where calc xi yi = case state of
                       Empty -> Empty
                       CEmpty -> if nb f (xi, yi) == 0 then COccupied else CEmpty
                       COccupied -> if nb f (xi, yi) >= todie then CEmpty else COccupied
          where state = f ! yi ! xi
                nb = if uselong then nbrslong else nbrs

calcTotal :: Ferry -> CellState -> Int
calcTotal f s = length $ filter (==s) $ [f ! y ! x | y <- [0..length f-1], x <- [0..length (f ! 0)-1]]

aoc11 :: Ferry -> Int
aoc11 f = let newf = nextstep f (4, False) in if newf == f then calcTotal newf COccupied else aoc11 newf

aoc11s :: Ferry -> Int
aoc11s f = let newf = nextstep f (5, True) in if newf == f then calcTotal newf COccupied else aoc11s newf

runAoC11 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc11 $ fromList $ fromList <$> arr

runAoC11s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc11s $ fromList $ fromList <$> arr

filterForToday = do
  whitespace
  many $
    anyOf [
      token "." $> Empty,
      token "#" $> COccupied,
      token "L" $> CEmpty
      ]
  
    