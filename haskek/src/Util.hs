module Util where

import System.IO
import Data.Char
import Data.Functor

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
newtype ParseRule a = ParseRule { runParse :: String -> Either ParseError (a, String) }

instance Functor ParseRule where
  fmap f (ParseRule r) = ParseRule (\s -> case r s of
    Left err -> Left err
    Right (res, rest) -> Right (f res, rest))

instance Applicative ParseRule where
  pure a = ParseRule (\s -> Right (a, s))
  (<*>) (ParseRule f) (ParseRule g) = ParseRule (\s -> case f s of
    Left err -> Left err
    Right (res, rest) -> case g rest of
      Left err -> Left err
      Right (res2, rest2) -> Right (res res2, rest2))

instance Monad ParseRule where
  (>>=) (ParseRule g) f = ParseRule go
    where go str = case g str of
                     Left err -> Left err
                     Right (res, rest) -> runParse (f res) rest

number :: ParseRule Int
number = ParseRule go
  where go [] = Left $ PErrorS "Couldn't parse number, encountered EOL"
        go (x:xs) | isNumber x = Right (read $ takeWhile isNumber (x:xs), dropWhile isNumber (x:xs))
                  | otherwise  = Left $ PErrorS $ "Couldn't parse number, encountered " <> [x]
token :: String -> ParseRule String
token tok = ParseRule $ go tok
  where go [] rest = Right ([], rest)
        go ts [] = Left $ PErrorS $ "Couldn't parse token " <> tok <> " (" <> ts <> ")" <> "encountered EOL"
        go (t:ts) (x:xs) | t == x = case go ts xs of
                                      Left err -> Left err
                                      Right (ts, rest) -> Right (t:ts, rest)
                         | otherwise = Left $ PErrorS $ "Couldn't parse token " <> tok <> " (" <> (t:ts) <> ")" <> "encountered " <> [x]
wordF :: (Char -> Bool) -> ParseRule String
wordF filter = ParseRule $ Right . span filter
word :: ParseRule String
word = wordF isAlphaNum
wordF1 filter = do
  w <- wordF filter
  case w of
    [] -> failparse
    k -> pure k
whitespace :: ParseRule ()
whitespace = wordF isWhitespace $> ()
char :: ParseRule Char
char = ParseRule go
  where go [] = Left $ PErrorS "Couldn't parse character, encountered EOL"
        go (x:xs) = Right (x, xs)
anyOf :: [ParseRule a] -> ParseRule a
anyOf rules = ParseRule $ go rules
  where go [] line = Left $ PErrorS $ "Exhausted or empty ruleset for anyOf parser " <> line
        go (r:rs) line = case runParse r line of
                           Left _ -> go rs line
                           Right result -> Right result
many :: ParseRule a -> ParseRule [a]
many rule = ParseRule $ go rule
  where go _ [] = Right ([], [])
        go rule line = case runParse rule line of
                            Left _ -> Right ([], line)
                            Right (r, rest) -> case go rule rest of
                                                 Left _ -> Right ([r], rest)
                                                 Right (rs, newline) -> Right (r:rs, newline)
many1 :: ParseRule a -> ParseRule [a]
many1 rule = do
  a <- many rule
  case a of
    [] -> failparse
    b -> pure b

endofstream :: ParseRule ()
endofstream = ParseRule go
  where go [] = Right ((), [])
        go (x:_) = Left $ PErrorS $ "Expected end of stream, encountered " <> [x]

parseUniversal :: String -> ParseRule a -> Either ParseError [a]
parseUniversal stream parser = go stream
  where go [] = Right []
        go s = case runParse parser s of
                  Left err -> Left err
                  Right (res, rest) -> case go rest of
                    Left err -> Left err
                    Right res2 -> Right $ res : res2
                  
(?>) :: ParseRule a -> (a -> Bool) -> ParseRule a
(?>) rule check = ParseRule go
  where go str = case runParse rule str of
          Left err -> Left err
          ok@(Right (candidate, _)) -> if check candidate then ok else Left PError

failparse = ParseRule (\_ -> Left PError)