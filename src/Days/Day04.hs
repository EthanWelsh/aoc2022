module Days.Day04 (runDay) where

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
import Control.Monad (void)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

rangeParser :: Parser Range
rangeParser = do
    a <- decimal
    void $ char '-'
    b <- decimal
    return (a, b)

pairParser :: Parser Pair
pairParser = do
    a <- rangeParser
    void $ char ','
    b <- rangeParser
    return (a, b)

inputParser :: Parser Input
inputParser = pairParser `sepBy` endOfLine

------------ TYPES ------------

type Range = (Int, Int)

type Pair = (Range, Range)

type Input = [Pair]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

rangeToList :: Range -> [Int]
rangeToList (a, b) = [a..b]

fullyContains :: Pair -> Bool
fullyContains (a, b) = let
    al = rangeToList a
    bl = rangeToList b
    isSubsetOf needle haystack = all (`elem` haystack) needle
    in isSubsetOf al bl || isSubsetOf bl al

partA :: Input -> OutputA
partA input = length $ filter (fullyContains) input

------------ PART B ------------

overlaps :: Pair -> Bool
overlaps (a, b) = let
    al = rangeToList a
    bl = rangeToList b
    containsAny needle haystack = any (`elem` haystack) needle
    in containsAny al bl

partB :: Input -> OutputB
partB input = length $ filter (overlaps) input
