module AdventLibrary where

import Data.List as List
import System.IO (openFile, IOMode (ReadMode), hGetContents)

getFileContent :: FilePath -> IO String 
getFileContent filePath = do
  handle <- openFile filePath ReadMode 
  hGetContents handle

elfCaloriesList :: String -> [[String]]
elfCaloriesList fileContent = split "" $ lines fileContent

caloriesPerElf :: [[String]] -> [Int]
caloriesPerElf = map elfCaloriesTotal

maxElfCalories :: [Int] -> Int
maxElfCalories = List.maximum 

elfCaloriesTotal :: [String] -> Int
elfCaloriesTotal calories =
  sum $ map (\x -> read x :: Int) calories

firstThreeElves :: [Int] -> [Int]
firstThreeElves caloriesPerElf = cutAt 3 $ reverse $ sort caloriesPerElf

sumOfFirstThree :: [Int] -> Int
sumOfFirstThree caloriesPerElf = sum $ firstThreeElves caloriesPerElf

-- GENERAL

-- Doesn't fail if list is too short, just gives what it could
cutAt :: Int -> [a] -> [a]
cutAt ix xs = go ix xs []
  where 
    go _ [] acc = acc
    go ix (x:xs) acc = if length acc == ix then acc else go ix xs (x:acc) 

split :: Eq a => a -> [a] -> [[a]]
split splitter splitted = go splitter splitted ([], [])
  where
    go :: Eq a => a -> [a] -> ([[a]], [a]) -> [[a]]
    go _ [] acc = fst acc ++ [snd acc]
    go splitter (x:xs) acc =
      if x == splitter
        then go splitter xs (fst acc ++ [snd acc], [])
        else go splitter xs (fst acc, snd acc ++ [x])
