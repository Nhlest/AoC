module Main where

import Test.Tasty

import AocTest_Test

tests :: TestTree
tests = testGroup "Tests" [test1, test2]

main = defaultMain tests