{-# LANGUAGE LambdaCase #-}
module Util where

import System.IO
import Data.Char
import Data.List

xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False

runWithFile f fname = do
  input_file <- openFile ("assets/" <> fname <> ".input") ReadMode
  input <- hGetContents input_file
  f input
  hClose input_file

data ParseRule = PRToken String | PRNumber | PRChar | PRWhitespace | PRWord deriving Show
data ParseResult = ResultToken String | ResultNumber String | ResultChar Char | ResultWord String | ResultOk | ResultErr String
  deriving (Show, Eq)

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace _    = False

data RetainTokens = DoRetain | DoNotRetain deriving Eq

parseNextToken :: ParseRule -> String -> RetainTokens -> (ParseResult, String)
parseNextToken PRWhitespace [] _ = (ResultOk, [])
parseNextToken PRWhitespace (x:xs) r | isWhitespace x = let (_, nexts) = parseNextToken PRWhitespace xs r in
                                                        (ResultOk, nexts)
                                     | otherwise      = (ResultErr "whitespace", x:xs)
parseNextToken PRWord (x:xs) r | isWhitespace x = (ResultErr "word", x:xs)
                               | otherwise      = let (nextres, nextstr) = parseNextToken PRWord xs r in
                                                       case nextres of
                                                         ResultErr _  -> (ResultWord [x], nextstr)
                                                         ResultWord w -> (ResultWord (x:w), nextstr)
                                                         _            -> (ResultErr "word", nextstr)
parseNextToken (PRToken []) s _ = (ResultOk, s)
parseNextToken (PRToken (sx:ss)) (x:xs) r | x /= sx = (ResultErr "token", xs)
                                          | x == sx = let (nextres, nextstr) = parseNextToken (PRToken ss) xs r in
                                                           case nextres of
                                                             ResultOk           -> (if r == DoRetain then ResultToken [sx] else ResultOk, nextstr)
                                                             ResultToken nexts  -> (if r == DoRetain then ResultToken (sx:nexts) else ResultOk, nextstr)
                                                             _                  -> (ResultErr "token", nextstr)
parseNextToken PRNumber (x:xs) r | isDigit x = let (nextres, nextstr) = parseNextToken PRNumber xs r in
                                                      case nextres of
                                                        ResultErr _    -> (ResultNumber [x], nextstr)
                                                        ResultNumber n -> (ResultNumber (x:n), nextstr)
                                                        _              -> (ResultErr "number", nextstr)
                                 | otherwise = (ResultErr "number", x:xs)
parseNextToken PRChar (x:xs) _ = (ResultChar x, xs)
parseNextToken PRChar [] _ = (ResultErr "char", [])
parseNextToken _ [] _ = (ResultErr "misc", [])

parseUniversal :: [ParseRule] -> String -> [[ParseResult]]
parseUniversal _ [] = []
parseUniversal rules string = if not $ null anyErrors then error ("Error during parsing" <> show anyErrors) else filter (/=ResultOk) results : parseUniversal rules restofthestring
  where (restofthestring, results) = mapAccumL (\a rule -> let (res, s) = parseNextToken rule a DoNotRetain in (s, res)) string rules
        anyErrors = filter (\case ResultErr _ -> True
                                  _ -> False) results