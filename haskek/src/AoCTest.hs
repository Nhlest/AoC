module AoCTest where

import System.IO
import Data.Char
import Data.Maybe

-- | Test function in preparation for AoC, should sort an array of Integers
aocTest :: [Int] -> [Int]
aocTest [] = []
aocTest (x:xs)       = less_than ++ [x] ++ greater_than
  where less_than    = aocTest [y | y <- xs, y <= x]
        greater_than = aocTest [y | y <- xs, y > x]

runAoCTest = do
  input_file <- openFile "assets/aoctest.input" ReadMode
  input <- hGetContents input_file
  let arrToSort = parse Nothing input
  print $ aocTest arrToSort
  hClose input_file
  pure ()
 where parse Nothing  [] = []
       parse (Just a) [] = [a]
       parse ma (l:xs) | isDigit l = parse (Just $ read [l] + fromMaybe 0 ma * 10) xs
                       | l `elem` [' ', '\n', '\t'] = case ma of 
                                                        Nothing ->     parse Nothing xs
                                                        Just a  -> a : parse Nothing xs
                       | otherwise = error $ "Unexpected symbol `" ++ [l] ++ "` where a number was expected"