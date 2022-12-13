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
import Data.Graph.AStar
import Data.Char
import qualified Data.HashSet as HashSet
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (notChar '\n') `sepBy` endOfLine

------------ TYPES ------------
type Maze = [String]

type Point = (Int, Int)

type Input = Maze

type OutputA = String

type OutputB = String

------------ PART A ------------
getHeight :: Maze -> Int
getHeight maze = length maze

getWidth :: Maze -> Int
getWidth maze = length (maze !! 0)

getAllPoints :: Maze -> [Point]
getAllPoints maze = let
    height = (getHeight maze) - 1
    width = (getWidth maze) - 1
    in  [(r, c) | r <- [0..height], c <- [0..width]]

get :: Maze -> Point -> Char
get maze (r, c) = (maze !! r) !! c

findMatching :: Maze -> (Point -> Bool) -> Point
findMatching maze p = fromJust $ find p (getAllPoints maze)

startPosition :: Maze -> Point
startPosition maze = findMatching maze (\p -> 'S' == (get maze p))

endPosition :: Maze -> Point
endPosition maze = findMatching maze (\p -> 'E' == (get maze p))

pointInBounds :: Maze -> Point -> Bool
pointInBounds maze (r, c) = let
    height = getHeight maze
    width = getWidth maze
    inBetween test start end = if test >= start && test < end then True else False 
    in inBetween r 0 height && inBetween c 0 width

nextChar :: Char -> Char
nextChar 'z' = 'z'
nextChar c = chr (ord c + 1)

getNormalized :: Maze -> Point -> Char
getNormalized maze p = case (get maze p) of
    'S' -> 'a'
    'E' -> 'z'
    c -> c

allowedStep :: Maze -> Point -> Point -> Bool
allowedStep maze currP nextP = let
    curr = getNormalized maze currP
    next = getNormalized maze nextP
    in (nextChar curr) >= next

neighborPoints :: Maze -> Point -> HashSet.HashSet Point
neighborPoints maze (r, c) = let
    points = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
    inBounds = filter (pointInBounds maze) points
    allowed = filter (allowedStep maze (r, c)) inBounds 
    in HashSet.fromList allowed

heuristicDistanceToGoal :: Maze -> Point -> Int
heuristicDistanceToGoal maze (r, c) = let
    (r', c') = endPosition maze
    in abs (r - r') + abs (c - c')

isGoal :: Maze -> Point -> Bool
isGoal maze p = p == (endPosition maze)

distanceBetweenNeighbors :: Point -> Point -> Int
distanceBetweenNeighbors _ _ = 1

search :: Maze -> Maybe [Point]
search maze = aStar (neighborPoints maze) distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) (startPosition maze)

partA :: Input -> OutputA
partA maze = show $ length $ fromJust $ search maze

------------ PART B ------------
allStartPoints :: Maze -> [Point]
allStartPoints maze = filter (\p -> 'a' == (getNormalized maze p)) (getAllPoints maze)

searchFromMultiplePoints :: Maze -> [Point] -> Maybe [Point]
searchFromMultiplePoints maze starts = let
    -- aStar only supports searching from a single start point, but we can easily add that capability using a fake start point
    fakeStartPoint = (-123, -123)
    neighborOrStartPoint point = if (point == fakeStartPoint) then (HashSet.fromList starts) else (neighborPoints maze point)
    in aStar neighborOrStartPoint distanceBetweenNeighbors (heuristicDistanceToGoal maze) (isGoal maze) fakeStartPoint

partB :: Input -> OutputB
partB maze = let
    shortestPath = searchFromMultiplePoints maze (allStartPoints maze)
    in show $ (length $ fromJust $ shortestPath) - 1
