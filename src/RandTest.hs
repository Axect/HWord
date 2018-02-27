module RandTest(main) where

import Data.Char
import Data.List
import System.Random
import System.Directory
import System.Environment

main :: IO()
main = do
  g <- getEnv "HOME"
  putStrLn "What level do you like test? (0 ~ 20)"
  level <- getLine
  words <- readFile (g ++ "/.local/lib/RandomWordList/word" ++ level ++ ".txt")
  means <- readFile (g ++ "/.local/lib/RandomMeanList/mean" ++ level ++ ".txt")
  putStrLn "How many words do you want to test?"
  n <- getLine
  let m = read n :: Int
  g <- newStdGen
  let ranList = take m . nub $ (randomRs (0,39) g)
  iterateList (randProcess (wordProcess words) ranList, randProcess (wordProcess means) ranList)

-- Declare type for convenience
type Text = String
type Words = [String]

-- Split Text
splitext :: (Text, Text) -> Words
splitext (x:y, w) | [x]/="\n" && y/=[] = splitext(y, w ++ [x])
                  | [x]=="\n" && y/=[] = w:splitext(y, [])
                  | [x]=="\n" && y==[] = [w]
                  | otherwise = [w++[x]]

wordProcess :: Text -> Words
wordProcess txt = splitext(map toLower txt, "")

randProcess :: Words -> [Int] -> Words
randProcess words keys | keys/=[] = words!!(head keys):randProcess words (tail keys)
                       | keys==[] = []


iterateList :: (Words, Words) -> IO ()
iterateList (x:y, a:b) | y/=[] = do {putStrLn ("> " ++ x); line <- getLine; putStrLn a; putStrLn "" ;iterateList(y, b)}
                       | y==[] = do {putStrLn ("> " ++ x); line <- getLine; putStrLn a; putStrLn "" ;putStrLn "------Finish------"}

