module AoC13 where

import Data.Either
import Data.Maybe
import Data.Functor
import Data.List
import Util
import Data.Bifunctor

type Schedule = (Integer, [Maybe Integer])

aoc13 :: Schedule -> Integer
aoc13 (start, busses) = (\(a, b) -> a * (b - start)) $ head $ sortBy (\(_, a) (_, b) -> compare a b) $ map nearestAfter $ catMaybes busses
  where nearestAfter x = case start `mod` x of
                           0 -> (x, start)
                           _ -> (x, (start `div` x + 1) * x)

catMaybesBySnd :: [(a, Maybe b)] -> [(a, b)]
catMaybesBySnd = map (second fromJust) . filter (isJust . snd) 

aoc13s :: Schedule -> Integer
aoc13s (_, first:xs) = go 0 (fromJust first) z
  where z = map (\(md, dv) -> (dv - (md `mod` dv), dv)) $ catMaybesBySnd $ zip [1..] xs
        go prev _ []                    = prev
        go prev mul ((tomod, todiv):xs) = 
          let next = head [cnd | i<-[0..], let cnd = i * mul + prev, cnd `mod` todiv == tomod]
          in  go next (mul*todiv) xs

runAoC13 input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print arr
  print $ aoc13 $ head arr

runAoC13s input = do
  let arrOfTokens = parseUniversal input filterForToday
  let arr = fromRight [] arrOfTokens
  print $ aoc13s $ head arr

filterForToday :: ParseRule (Integer, [Maybe Integer])
filterForToday = do
  depart <- fromIntegral <$> number
  whitespace
  buses <- many $ do
    num <- anyOf [
        Just <$> number,
        token "x" $> Nothing
      ]
    many $ token ","
    pure num
  pure (depart, map (fmap fromIntegral) $ buses)
