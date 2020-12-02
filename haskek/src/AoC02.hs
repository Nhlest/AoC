module AoC02 where

xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False

data Policy   = Policy Int Int Char
type Password = String
type Row      = (Policy, Password)

-- | Validates a set of passwords according to the first part ruleset
aoc02 :: [Row] -> Int
aoc02 [] = 0
aoc02 ((Policy lower higher chr, pw):ps) = isValid + aoc02 ps
 where isValid = let numofc = length $ filter (==chr) pw in
                     if numofc >= lower && numofc <= higher then 1 else 0

-- | Validates a set of passwords according to the second part ruleset
aoc02s :: [Row] -> Int
aoc02s [] = 0
aoc02s ((Policy lower higher chr, pw):ps) = isValid + aoc02s ps
 where isValid = if fs `xor` sn then 1 else 0
       fs = pw !! (lower  - 1) == chr
       sn = pw !! (higher - 1) == chr

runAoC02 input = do
  let arrToDo = parseAsRows $ lines input
  print $ aoc02 arrToDo
  pure ()

runAoC02s input = do
  let arrToDo = parseAsRows $ lines input
  print $ aoc02s arrToDo
  pure ()

parseAsRows :: [String] -> [(Policy, [Char])]
parseAsRows = map (parseRow . words)
parseRow :: [[Char]] -> (Policy, [Char])
parseRow [bounds, [char, ':'], pw] = 
  let (l, h) = parseBounds 0 bounds 
  in  (Policy l h char, pw)
parseBounds :: Int -> [Char] -> (Int, Int)
parseBounds a ('-':xs) = (a, read xs)
parseBounds a (c  :xs) = parseBounds (a*10+read [c]) xs