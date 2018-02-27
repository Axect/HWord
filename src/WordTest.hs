module WordTest (main) where

import Data.Char
import Data.List -- nub
import System.Random -- random
import System.Directory -- listDirectory
import System.Environment

main :: IO()
main = do
  g <- getEnv "HOME"
  -- genRandomWords
  -- cutText
  putStrLn "What level do you like memorize? (0 ~ 20)"
  level <- getLine
  words <- readFile (g ++ "/.local/lib/RandomWordList/word" ++ level ++ ".txt")
  means <- readFile (g ++ "/.local/lib/RandomMeanList/mean" ++ level ++ ".txt")
  putStrLn "What kind of test do you like? 1:Words, 2:Means"
  test <- getLine
  case reads test :: [(Integer, String)] of
    [(1, _)] -> iterateList(wordProcess words, wordProcess means)
    [(2, _)] -> iterateList(wordProcess means, wordProcess words)
    _ -> putStrLn "Invalid Input"

-- Declare type for convenience
type Text = String
type Words = [String]

-- Split Text
splitext :: (Text, Text) -> Words
splitext (x:y, w) | [x]/="\n" && y/=[] = splitext(y, w ++ [x])
                  | [x]=="\n" && y/=[] = w:splitext(y, [])
                  | [x]=="\n" && y==[] = [w]
                  | otherwise = [w++[x]]

-- Combine map toLower & Split Text
wordProcess :: Text -> Words
wordProcess txt = splitext(map toLower txt, "")

-- Iteration IO
iterateList :: (Words, Words) -> IO ()
iterateList (x:y, a:b) | y/=[] = do {putStrLn ("> " ++ x); line <- getLine; putStrLn a; putStrLn "" ;iterateList(y, b)}
                       | y==[] = do {putStrLn ("> " ++ x); line <- getLine; putStrLn a; putStrLn "" ;putStrLn "------Finish------"}

-------------------------------------
----- Random Generate Algorithm -----
----- You can Skip here -------------
-------------------------------------

-- Right Directory
rightDir :: ([FilePath], Integer) -> [FilePath]
rightDir (x:y, n) | n==1 && y/=[] = ("Wordlist/"++x) : rightDir(y, n)
                  | n==1 && y==[] = ["Wordlist/"++x]
                  | n==2 && y/=[] = ("Meanlist/"++x) : rightDir(y, n)
                  | n==2 && y==[] = ["Meanlist/"++x]

-- genRandomWords
-- genRandomWords :: IO()
-- genRandomWords = do
--  wordst <- listDirectory "Wordlist/"
--  meanst <- listDirectory "Meanlist/"
--  tword <- mapM readFile $ rightDir (sort wordst, 1)
--  mword <- mapM readFile $ rightDir (sort meanst, 2)
--  print $ tword
--  print $ mword
--  let rtword = flatten $ map wordProcess tword
--  let rmword = flatten $ map wordProcess mword
--  let l = length rtword
--  g <- newStdGen
--  let ranList = take l . nub $ (randomRs (0,l-1) g)
--  let wordR = sortT (rtword, ranList, "")
--  let meanR = sortT (rmword, ranList, "")
--  writeFile "RandWords/word.txt" wordR
--  writeFile "RandMeans/mean.txt" meanR

-- flatten
flatten :: Eq a => [[a]] -> [a]
flatten (x:y) | y/=[] = x ++ flatten(y)
              | y == [] = x

-- Sort Text
sortT :: (Words, [Int], Text) -> Text
sortT (origin, [], result) = result
sortT (origin, keys, result) = sortT (origin, tail keys, result ++ (origin!!(head keys)) ++ "\n")

-------------------------------------
----- Cutting Algorithm -------------
----- You can Skip here -------------
-------------------------------------

-- Cut
cutText :: IO()
cutText = do
  wordToCut <- readFile "RandWords/word.txt"
  meanToCut <- readFile "RandMeans/mean.txt"
  let wordList = wordProcess wordToCut
  let meanList = wordProcess meanToCut
  saveLevel (splitLevel (wordList, 1), 0, 1)
  saveLevel (splitLevel (meanList, 1), 0, 2)


-- Cut Level
cutLevel :: (Words, Integer) -> String
cutLevel (x:y, n) | n==0 = ""
                  | y/=[] = x++"\n"++cutLevel(y, n-1)
                  | y==[] = x++"\n"

-- Split Level
splitLevel :: (Words, Int) -> [String]
splitLevel (words, n) | n==22 || words==[] = []
                      | otherwise = cutLevel (words, 40) : splitLevel(drop 40 words, n+1)

-- Save Level
saveLevel :: (Words,Int, Int) -> IO ()
saveLevel (x:y, n, i) | y==[] && i==1 = do { writeFile ("RandomWordList/word" ++ show n ++ ".txt") x; print $ "Finish Save" }
                      | y==[] && i==2 = do { writeFile ("RandomMeanList/mean" ++ show n ++ ".txt") x; print $ "Finish Save" }
                      | y/=[] && i==1 = do { writeFile ("RandomWordList/word" ++ show n ++ ".txt") x; saveLevel(y, n+1, i) }
                      | y/=[] && i==2 = do { writeFile ("RandomMeanList/mean" ++ show n ++ ".txt") x; saveLevel(y, n+1, i) }

