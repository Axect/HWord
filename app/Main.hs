module Main (main) where

import qualified WordTest (main)
import qualified RandTest (main)
import qualified AccumulateRand (main)

main :: IO ()
main = do
  putStrLn $ "What kind do you like test? 1.Normal 2.Random 3.Accumulate Random"
  kind <- getLine
  let kind' = read kind :: Int
  if kind' == 1
     then WordTest.main
     else if kind' == 2
     then RandTest.main
     else AccumulateRand.main
