module Day1 where

import System.IO (hGetContents, openFile, IOMode (ReadMode))
import AdventLibrary ( sumOfFirstThree, caloriesPerElf, elfCaloriesList)
import Data.List as List

main = do
  handle <- openFile "Day1Input" ReadMode 
  fileContent <- hGetContents handle
  let 
    caloriesPerElf' = caloriesPerElf . elfCaloriesList $ fileContent
    result1 = List.maximum caloriesPerElf'
    result2 = sumOfFirstThree caloriesPerElf'
  print (result1, result2)


