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
    testProperty "[PAR] Should, indeed, parse" $ forAll sampleGenerator (\(ruleset, string, result) -> (==) result $ parseUniversal ruleset string)
  ]

parserGenerator :: Gen [ParseRule]
parserGenerator = do
  concat <$> listOf1 (oneof [generateNumber, generateToken, generateWord, generateChar])
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

sampleGenerator :: Gen([ParseRule], String, [[ParseResult]])
sampleGenerator = do
  a <- parserGenerator
  (b, c) <- generateStringAndResultFromRuleSet a
  pure (a, b, [c])