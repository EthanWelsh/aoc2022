module Days.Day08 (runDay) where

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
import Util.Matrix
import qualified Util.Parsers as P

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.linesParser

------------ TYPES ------------
type Trees = Matrix Char

type Input = Trees

type OutputA = Int

type OutputB = Int

------------ PART A ------------

range :: Int -> Int -> [Int]
range start end
    | start < end  = [start..end]
    | start > end  = [start, start - 1 .. end]
    | start == end = []

getLinesOfSite :: Trees -> Point -> [[Point]]
getLinesOfSite ts (r, c) = let
    height = (getHeight ts) - 1
    width = (getWidth ts) - 1
    up = zip    (range r 0)      (repeat c)
    down = zip  (range r height) (repeat c)
    left = zip  (repeat r)       (range c 0)
    right = zip (repeat r)       (range c width)
    in [up, down, left, right]

isStrictlyDecreasing :: Ord a => [a] -> Bool
isStrictlyDecreasing [] = True
isStrictlyDecreasing (x:xs) = all (<x) xs

isVisible :: Trees -> Point -> Bool
isVisible ts p = let
    linesOfSitePoints = getLinesOfSite ts p                   :: [[Point]]
    linesOfSiteValues = map (map (get ts)) linesOfSitePoints  :: [[Char]]
    isDecreasing = map isStrictlyDecreasing linesOfSiteValues :: [Bool]
    in or isDecreasing

countWhere :: (a -> Bool) -> [a] -> Int
countWhere pred xs = length $ filter pred xs

partA :: Input -> OutputA
partA trees = let
    allPoints = getAllPoints trees
    in countWhere (isVisible trees) allPoints

------------ PART B ------------

countDecreasingFrom :: Ord a => a -> [a] -> Int
countDecreasingFrom _ [] = 0
countDecreasingFrom t (x:xs) = if x >= t then 1 else 1 + countDecreasingFrom t xs

countDecreasing :: Ord a => [a] -> Int
countDecreasing [] = 0
countDecreasing (x:xs) = countDecreasingFrom x xs

getScore :: Trees -> Point -> Int
getScore ts p = let
    linesOfSitePoints = getLinesOfSite ts p                   :: [[Point]]
    linesOfSiteValues = map (map (get ts)) linesOfSitePoints  :: [[Char]]
    in product $ map countDecreasing linesOfSiteValues

partB :: Input -> OutputB
partB ts = let
    allPoints = getAllPoints ts :: [Point]
    allScores = map (getScore ts) allPoints :: [Int]
    in maximum allScores
