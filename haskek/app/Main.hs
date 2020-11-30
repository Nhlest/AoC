module Main where

import System.Environment
import Control.Monad

import AoCTest

allModules = [("aoctest", runAoCTest), ("aoctest2", runAoCTest)]

main = do 
  args <- getArgs
  let cases_to_run = case args of
                       [] -> map snd allModules
                       x -> intersectBy x allModules
  forM_ cases_to_run id
 where intersectBy _ [] = []
       intersectBy to ((x, f):xs) | x `elem` to = f : intersectBy to xs
                                  | otherwise   = intersectBy to xs
