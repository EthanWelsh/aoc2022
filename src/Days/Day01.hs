module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import qualified Data.Text as T
import System.IO
import Control.Monad
import qualified Data.List as L


{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

-- parseInput :: T.Text -> [[Int]]
-- parseInput contents = map readInts $ map L.lines $ T.splitOn "\n\n" contents

inputParser :: Parser Input
inputParser = (decimal `sepBy` endOfLine) `sepBy` count 2 endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int
------------ PART A ------------
partA :: Input -> OutputA
partA input = maximum $ map sum input

------------ PART B ------------
qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = let 
  larger = qs $ filter (>x) xs
  equal = x : filter (==x) xs
  smaller = qs $ filter (<x) xs
  in larger ++ equal ++ smaller

partB :: Input -> OutputB
partB input = let
    sortedTotals = qs (map sum input)
    in sum $ L.take 3 sortedTotals







 


