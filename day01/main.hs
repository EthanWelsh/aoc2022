import System.IO
import Control.Monad
import Data.List.Split
import Data.List


readInts :: [String] -> [Int]
readInts xs = map readInt xs

calculateTotals :: [[Int]] -> [Int]
calculateTotals xs = map sum xs

qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = let 
  larger = qs $ filter (>x) xs
  equal = x : filter (==x) xs
  smaller = qs $ filter (<x) xs
  in larger ++ equal ++ smaller


main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let elfs = map readInts $ map lines $ splitOn "\n\n" contents
        let totals = map sum elfs
        
        let part1 = maximum totals       
        print ("part 1: " ++ show part1)
        
        let sortedTotals = qs totals
        let part2 = sum $ take 3 sortedTotals
        print ("part 2: " ++ show part2)

        hClose handle   

readInt :: String -> Int
readInt = read

