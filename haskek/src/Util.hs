module Util where

import System.IO
import Data.Char
import Data.Maybe

runWithFile f fname = do
  input_file <- openFile ("assets/" <> fname <> ".input") ReadMode
  input <- hGetContents input_file
  f input
  hClose input_file

parseAsInts Nothing  [] = []
parseAsInts (Just a) [] = [a]
parseAsInts ma (l:xs) | isDigit l = parseAsInts (Just $ read [l] + fromMaybe 0 ma * 10) xs
                      | l `elem` [' ', '\n', '\t'] = case ma of 
                                                 Nothing ->     parseAsInts Nothing xs
                                                 Just a  -> a : parseAsInts Nothing xs
                      | otherwise = error $ "Unexpected symbol `" ++ [l] ++ "` where a number was expected"