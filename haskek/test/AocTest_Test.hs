{-# LANGUAGE LambdaCase #-}
module AocTest_Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad
import Data.Maybe

import AoCTest
import Util

test1 :: TestTree
test1 = testGroup "UnitTests general" [
    testCase "1 + 1 = 2" $ assertEqual "1 + 1 is indeed 2" (1+1) 2,
    testCase "1 + 2 = 3" $ assertEqual "1 + 2 is indeed 3" (1+2) 3
  ]

test2 :: TestTree
test2 = testGroup "UnitTests for aocTest" [
    testCase "should 'sort' empty list" $ assertEqual "should be empty" [] $ aocTest [],
    testCase "should 'sort' some random numbers" $ assertEqual "should be sorted" [1,1,2,5,5,8,9] $ aocTest [1,2,5,1,9,5,8],
    testCase "should 'sort' another list of random numbers" $ assertEqual "should be sorted" [1248,125157,4512478,51275812873] $ aocTest [1248,125157,51275812873,4512478],
    QC.testProperty "should 'sort' arbitrary list of integers" $ is_sorted . aocTest
  ]
 where is_sorted []       = True
       is_sorted [_]      = True
       is_sorted (x:y:xs) = x<=y && is_sorted (y:xs)

testParser :: TestTree
testParser = testGroup "UnitTests for Parser" [
    testProperty "[PAR] Should, indeed, parse" $ forAllShrink sampleGenerator shrinkage (\(ruleset, string, result, _) -> (==) result $ parseUniversal ruleset string)
  ]

deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

shrinkage :: ([ParseRule], String, [[ParseResult]], [[ParseRule]]) -> [([ParseRule], String, [[ParseResult]], [[ParseRule]])]
shrinkage (_, _, _, s) | length s <= 2 = []
                       | otherwise = map (\a -> let (b,c) = generateStringAndResultFromRuleSet' (concat a) in (concat a, b, [c], a)) shrinks
  where shrinks = map (\a -> deleteAt a s) [0..length s] 

-- TODO: proper shrinkage and not this pepega workaround
generateStringAndResultFromRuleSet' :: [ParseRule] -> (String, [ParseResult])
generateStringAndResultFromRuleSet' rules = do
  let (s, r) = unzip $ map (\case
        PRWhitespace -> ("   ", Nothing)
        PRToken tok -> (tok, Nothing)
        PRWord -> ("asdf", Just $ ResultWord "asdf")
        PRNumber -> ("123", Just $ ResultNumber "123")
        PRChar -> (['a'], Just $ ResultChar 'a')
       ) rules in (concat s, catMaybes r)

parserGenerator :: Gen ([ParseRule], [[ParseRule]])
parserGenerator = do
  a <- listOf1 (oneof [generateNumber, generateToken, generateWord, generateChar])
  pure (concat a, a)
 where generateNumber = oneof [pure [PRNumber, PRWhitespace], pure [PRNumber, PRToken "-"]]
       generateToken  = pure . PRToken <$> listOf1 (oneof $ map pure "asdf-+:")
       generateWord   = pure [PRWord, PRWhitespace]
       generateChar   = pure [PRChar]

generateStringAndResultFromRuleSet :: [ParseRule] -> Gen (String, [ParseResult])
generateStringAndResultFromRuleSet rules = do
  (s, r) <- unzip <$> forM rules (\case
      PRWhitespace -> (,) <$> listOf1 (pure ' ') <*> pure Nothing
      PRToken tok -> pure (tok, Nothing)
      PRWord -> do
          a <- listOf1 $ choose ('a', 'z')
          pure (a, Just $ ResultWord a)
      PRNumber -> do
          a::Int <- choose (1, 99999)
          pure (show a, Just $ ResultNumber $ show a)
      PRChar -> do
          a <- choose ('a', 'z')
          pure ([a], Just $ ResultChar a)
    )
  pure (concat s, catMaybes r)

sampleGenerator :: Gen([ParseRule], String, [[ParseResult]], [[ParseRule]])
sampleGenerator = do
  (a, dbg) <- parserGenerator
  (b, c) <- generateStringAndResultFromRuleSet a
  pure (a, b, [c], dbg)