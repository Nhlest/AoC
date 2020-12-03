{-# LANGUAGE LambdaCase #-}
module Util where

import System.IO
import Data.Char
import Data.List
import Data.Either

xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False

runWithFile f fname = do
  input_file <- openFile ("assets/" <> fname <> ".input") ReadMode
  input <- hGetContents input_file
  f input
  hClose input_file

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace _    = False

data ParseError = PError | PErrorS String deriving (Show, Eq)
data ParseRule a = 
    PRWhitespace
  | PRToken      String (String -> a)
  | PRTokenSilent        String
  | PRWord              (String -> a)
  | PRNumber            (Int    -> a)
  | PRChar              (Char   -> a)
  | PREither [ParseRule a]
  | PRMany (ParseRule a)

tryParse :: ParseRule a -> String -> Either ParseError ([a], String)
tryParse PRWhitespace [] = Right ([], [])
tryParse _ [] = Left PError
tryParse PRWhitespace (x:xs) | isWhitespace x = Right ([], dropWhile isWhitespace xs)
                             | otherwise      = Left PError
tryParse (PRToken token trans) (xs) = case go token xs of
                                        Just rest -> Right ([trans token], rest)
                                        Nothing   -> Left PError
 where go [] rest = Just rest
       go _ []    = Nothing
       go (t:ts) (x:xs) | x == t    = go ts xs
                        | otherwise = Nothing
tryParse (PRTokenSilent token) (xs) = case go token xs of
                                        Just rest -> Right ([], rest)
                                        Nothing   -> Left PError
 where go [] rest = Just rest
       go _ []    = Nothing
       go (t:ts) (x:xs) | x == t    = go ts xs
                        | otherwise = Nothing
tryParse (PRWord trans) (x:xs) | isAlphaNum x = Right ([trans $ takeWhile isAlphaNum (x:xs)], dropWhile isAlphaNum (x:xs))
                               | otherwise    = Left PError
tryParse (PRNumber trans) (x:xs) | isNumber x = Right ([trans $ read $ takeWhile isNumber (x:xs)], dropWhile isNumber (x:xs))
                                 | otherwise  = Left PError
tryParse (PRChar trans) (x:xs) = Right ([trans x], xs)
tryParse (PREither []) _ = Left PError
tryParse (PREither rules) line = if null results then Left PError
                                                 else head results
  where results = dropWhile isLeft $ map (`tryParse` line) rules
tryParse (PRMany rule) line = applyUntill line
  where applyUntill [] = Right ([], [])
        applyUntill line = case tryParse rule line of
                             Right (r, newline) -> case applyUntill newline of
                               Right (rs, restoftheline) -> Right (r <> rs, restoftheline)
                               Left err -> Left err
                             Left err -> Right ([], line)

parseUniversal :: [ParseRule a] -> ([a] -> b) -> String -> Either ParseError [b]
parseUniversal _ trans[] = Right []
parseUniversal rules trans stream = case go rules stream of
                                Left err -> Left err
                                Right (result, rest) -> case parseUniversal rules trans rest of
                                                          Left err -> Left err
                                                          Right results -> Right $ trans result : results
  where go [] rest = Right ([], rest)
        go (r:rs) rest = case tryParse r rest of
                           Left err -> Left err
                           Right (result, rest) -> case go rs rest of
                                                     Left err -> Left err
                                                     Right (results, newline) -> Right (result <> results, newline)