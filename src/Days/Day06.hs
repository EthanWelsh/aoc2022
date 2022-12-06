module Days.Day06 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 anyChar

------------ TYPES ------------
type Input = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf size xs = (Data.List.take size xs) : groupsOf size (drop 1 xs)

noDupes :: [Char] -> Bool
noDupes xs = (length xs) == length (nub xs)

findIndexOfFirstUniqueGroup :: Int -> String -> Int
findIndexOfFirstUniqueGroup groupSize input = let
    groups = groupsOf groupSize input                        :: [String]
    uniqueGroup = fromJust $ find noDupes groups             :: String
    groupIndex = fromJust $ findIndex (==uniqueGroup) groups :: Int
    in groupIndex + length uniqueGroup

partA :: Input -> OutputA
partA input = findIndexOfFirstUniqueGroup 4 input

------------ PART B ------------
partB :: Input -> OutputB
partB input = findIndexOfFirstUniqueGroup 14 input
