module Days.Day12 (runDay) where

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
import Data.Char
import qualified Data.HashSet as HashSet
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Algorithm.Search
import Util.Matrix
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (notChar '\n') `sepBy` endOfLine

------------ TYPES ------------
type Maze = (Matrix Char)

type Input = Maze

type OutputA = String

type OutputB = String

------------ PART A ------------

allowedStep :: Maze -> Point -> Point -> Bool
allowedStep maze currP nextP = let
    curr = getNormalized maze currP
    next = getNormalized maze nextP
    in (nextChar curr) >= next

neighborPoints :: Maze -> Point -> HashSet.HashSet Point
neighborPoints maze p = HashSet.filter (allowedStep maze p) (cardinalPoints maze p)

findMatching :: Maze -> (Point -> Bool) -> Point
findMatching maze p = fromJust $ find p (getAllPoints maze)

startPosition :: Maze -> Point
startPosition maze = findMatching maze (\p -> 'S' == (get maze p))

endPosition :: Maze -> Point
endPosition maze = findMatching maze (\p -> 'E' == (get maze p))

nextChar :: Char -> Char
nextChar 'z' = 'z'
nextChar c = chr (ord c + 1)

getNormalized :: Maze -> Point -> Char
getNormalized maze p = case (get maze p) of
    'S' -> 'a'
    'E' -> 'z'
    c -> c

heuristicDistanceToGoal :: Maze -> Point -> Int
heuristicDistanceToGoal maze (r, c) = let
    (r', c') = endPosition maze
    in abs (r - r') + abs (c - c')

isGoal :: Maze -> Point -> Bool
isGoal maze p = p == (endPosition maze)

distanceBetweenNeighbors :: Point -> Point -> Int
distanceBetweenNeighbors _ _ = 1

search :: Maze -> Maybe (Int, [Point])
search maze = aStar (neighborPoints maze) distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) (startPosition maze)

partA :: Input -> OutputA
partA maze = show $ fst $ fromJust $ search maze

------------ PART B ------------
allStartPoints :: Maze -> [Point]
allStartPoints maze = filter (\p -> 'a' == (getNormalized maze p)) (getAllPoints maze)

searchFromMultiplePoints :: Maze -> [Point] -> Maybe (Int, [Point])
searchFromMultiplePoints maze starts = let
    -- aStar only supports searching from a single start point, but we can easily add 
    -- that capability using a fake start point, and marking all real start points as
    -- neighbors of that point.
    fakeStartPoint = (-123, -123)
    neighborOrStartPoint point = if (point == fakeStartPoint) then (HashSet.fromList starts) else (neighborPoints maze point)
    in aStar neighborOrStartPoint distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) fakeStartPoint

partB :: Input -> OutputB
partB maze = let
    shortestPath = searchFromMultiplePoints maze (allStartPoints maze)
    in show $ fst (fromJust $ shortestPath) - 1
