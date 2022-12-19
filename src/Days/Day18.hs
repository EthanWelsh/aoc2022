module Days.Day18 (runDay) where

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
import Algorithm.Search
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
pointParser :: Parser Point
pointParser = do
    x <- decimal
    void $ char ','
    y <- decimal
    void $ char ','
    z <- decimal
    return (x, y, z)

inputParser :: Parser Input
inputParser = do
    points <- pointParser `sepBy` endOfLine
    return points

------------ TYPES ------------
type Point = (Int, Int, Int)

type Input = [Point]

data Direction = North | South | East | West | Up | Down

type OutputA = Int

type OutputB = Int

------------ PART A ------------

-- X: left and right
-- Y: forward and backward
-- Z: up and down
movePoint :: Point -> Direction -> Point
movePoint (x, y, z) North = (x,     y + 1, z)
movePoint (x, y, z) South = (x,     y - 1, z)
movePoint (x, y, z) East =  (x + 1, y,     z)
movePoint (x, y, z) West =  (x - 1, y,     z)
movePoint (x, y, z) Up =    (x,     y,     z + 1)
movePoint (x, y, z) Down =  (x,     y,     z - 1)

openFacesFromPoint :: Set Point -> Point -> [Point]
openFacesFromPoint points point = let
    faces = map (movePoint point) [North, South, East, West, Up, Down]
    in filter (not . (`elem` points)) faces

openFaces :: Set Point -> [Point]
openFaces points = concatMap (openFacesFromPoint points) points

startMap :: [Point] -> Map Point [Point]
startMap ps = Map.fromList $ map (\p -> (p, [p])) ps

deleteAllFromMap :: Ord k => [k] -> Map k v -> Map k v
deleteAllFromMap ks m = foldl (\mm k -> Map.delete k mm) m ks

partA :: Input -> OutputA
partA input = length $ openFaces (Set.fromList input)

------------ PART B ------------

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

neighbors :: Set Point -> Point -> [Point]
neighbors rock point = let 
    points = map (movePoint point) [North, South, East, West, Up, Down]
    in filter (not . (`elem` rock)) points

reachableFromOutside :: Set Point -> Point -> Bool
reachableFromOutside rock p = let
    goal = (0, 0, 0)
    in isJust $ aStar (neighbors rock) (\_ _ -> 1) (manhattanDistance goal) (==goal) p

partB :: Input -> OutputB
partB input = let
    rock = Set.fromList input
    open = openFaces rock
    in length $ filter (reachableFromOutside rock) open
    
