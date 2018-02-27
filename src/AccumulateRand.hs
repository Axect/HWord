module AccumulateRand(main) where

import Data.Char
import Data.List
import System.Random
import System.Directory
import System.Environment

main :: IO()
main = do
  g <- getEnv "HOME"
  putStrLn "What level until do you like test? (0 ~ 20)"
  choose <- getLine
  let level = read choose :: Int
  -- mapM returns IO => <- operator recieve IO String to String
  words <- mapM readFile [g ++ "/.local/lib/RandomWordList/word" ++ lev ++ ".txt" | lev <- map show [0..level]]
  means <- mapM readFile [g ++ "/.local/lib/RandomMeanList/mean" ++ lev ++ ".txt" | lev <- map show [0..level]]
  let words' = flatten words
  let means' = flatten means
  putStrLn "How many words do you want to test?"
  n <- getLine
  let m = read n :: Int
  g <- newStdGen
  let ranList = take m . nub $ (randomRs (0,39 + 40*level) g)
  iterateList (randProcess (wordProcess words') ranList, randProcess (wordProcess means') ranList)

-- Declare type for convenience
type Text = String
type Words = [String]

-- Flatten
flatten :: Words -> Text
flatten words = foldr (++) [] words

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

