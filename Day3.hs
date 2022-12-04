module Day3 where
import AdventLibrary (getFileContent)
import Data.List (intersect)
import Data.Function ((&))

main = do
  rawRucksacks <- lines <$> getFileContent "Day3Input"
  print (singleRucksackPriority rawRucksacks, groupRucksackPriority rawRucksacks)

singleRucksackPriority :: [String] -> Int
singleRucksackPriority rawRucksacks =
  sum $ map (((toPriority . head) . uncurry intersect) . (\x -> splitAt ( length x `div` 2) x)) rawRucksacks

groupRucksackPriority :: [String] -> Int
groupRucksackPriority rawRucksacks =
  sum $ map ((toPriority . head) . (\(x, y, z) -> x `intersect` intersect y z )) (groupBy3 [] rawRucksacks )
  where
    groupBy3 acc (x:y:z:xs) = groupBy3 ((x,y,z) : acc) xs
    groupBy3 acc _ = acc

toPriority :: Char -> Int
toPriority 'a' = 1
toPriority 'b' = 2
toPriority 'c' = 3
toPriority 'd' = 4
toPriority 'e' = 5
toPriority 'f' = 6
toPriority 'g' = 7
toPriority 'h' = 8
toPriority 'i' = 9
toPriority 'j' = 10
toPriority 'k' = 11
toPriority 'l' = 12
toPriority 'm' = 13
toPriority 'n' = 14
toPriority 'o' = 15
toPriority 'p' = 16
toPriority 'q' = 17
toPriority 'r' = 18
toPriority 's' = 19
toPriority 't' = 20
toPriority 'u' = 21
toPriority 'v' = 22
toPriority 'w' = 23
toPriority 'x' = 24
toPriority 'y' = 25
toPriority 'z' = 26
toPriority 'A' = 26 + 1
toPriority 'B' = 26 + 2
toPriority 'C' = 26 + 3
toPriority 'D' = 26 + 4
toPriority 'E' = 26 + 5
toPriority 'F' = 26 + 6
toPriority 'G' = 26 + 7
toPriority 'H' = 26 + 8
toPriority 'I' = 26 + 9
toPriority 'J' = 26 + 10
toPriority 'K' = 26 + 11
toPriority 'L' = 26 + 12
toPriority 'M' = 26 + 13
toPriority 'N' = 26 + 14
toPriority 'O' = 26 + 15
toPriority 'P' = 26 + 16
toPriority 'Q' = 26 + 17
toPriority 'R' = 26 + 18
toPriority 'S' = 26 + 19
toPriority 'T' = 26 + 20
toPriority 'U' = 26 + 21
toPriority 'V' = 26 + 22
toPriority 'W' = 26 + 23
toPriority 'X' = 26 + 24
toPriority 'Y' = 26 + 25
toPriority 'Z' = 26 + 26
toPriority _ = 0