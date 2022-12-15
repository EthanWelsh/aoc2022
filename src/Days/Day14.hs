module Days.Day14 (runDay) where

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
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Function
import Data.Void
import qualified Data.HashSet as HashSet
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

pointParser :: Parser Point
pointParser = do
    x <- decimal
    void $ char ','
    y <- decimal
    return (x, y)

arrowParser :: Parser Arrow
arrowParser = pointParser `sepBy` (string " -> ")

inputParser :: Parser Input
inputParser = arrowParser `sepBy` endOfLine

------------ TYPES ------------
type Point = (Int, Int)
type Arrow = [Point]
type Blocks = HashSet.HashSet Point

type Input = [Arrow]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

down (x, y) =      (x, y + 1)
downLeft (x, y) =  (x - 1, y + 1)
downRight (x, y) = (x + 1, y + 1)

isHigherThan :: Point -> Point -> Bool
isHigherThan (x, y) (x', y') = y' < y

isObstructed :: Point -> Blocks -> Bool
isObstructed point blocks = HashSet.member point blocks

notObstructed :: Point -> Blocks -> Bool
notObstructed point blocks = not $ isObstructed point blocks

inAbyss :: Point -> Blocks -> Bool
inAbyss point blocks = all (isHigherThan point) blocks

simulate :: Point -> Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
simulate point blocks stopCondition
    | stopCondition point blocks    = (blocks, True)
    | notObstructed d blocks  = simulate d  blocks stopCondition
    | notObstructed dl blocks = simulate dl blocks stopCondition
    | notObstructed dr blocks = simulate dr blocks stopCondition
    | otherwise               = (HashSet.insert point blocks, False)
    where
        d = down point
        dl = downLeft point
        dr = downRight point

dropSand :: Blocks -> (Point -> Blocks -> Bool) -> (Blocks, Bool)
dropSand blocks stopCondition = simulate (500,0) blocks stopCondition

dropSandForever :: Blocks -> (Point -> Blocks -> Bool) -> [(Blocks, Bool)]
dropSandForever blocks stopCondition = iterate helper (blocks, False)
  where helper (blocks, _) = dropSand blocks stopCondition

laggedPairs :: [a] -> [(a, a)]
laggedPairs [] = []
laggedPairs [x] = []
laggedPairs (x:y:ys) = (x, y) : laggedPairs (y:ys)

arrowToSegment :: Arrow -> [(Point, Point)]
arrowToSegment arrow = laggedPairs arrow

range :: Int -> Int -> [Int]
range start end
    | start < end  = [start..end]
    | start > end  = [start, start - 1 .. end]
    | start == end = repeat start

segmentBlocks :: (Point, Point) -> Blocks
segmentBlocks ((a, b), (c, d)) = HashSet.fromList $ zip (range a c) (range b d)

unionAll :: [Blocks] -> Blocks
unionAll blocks = HashSet.unions blocks

blocksFromArrow :: Arrow -> Blocks
blocksFromArrow arrow = unionAll $ map segmentBlocks (arrowToSegment arrow)

blocksFromArrows :: [Arrow] -> Blocks
blocksFromArrows arrows = unionAll $ map blocksFromArrow arrows

partA :: Input -> OutputA
partA input = (fromJust $ findIndex (snd) (dropSandForever (blocksFromArrows input) inAbyss)) - 1

------------ PART B ------------

lowestPoint :: Blocks -> Point
lowestPoint blocks = maximumBy (compare `on` snd) blocks

addFloor :: Blocks -> Blocks
addFloor blocks = let 
    (x, y) = lowestPoint blocks
    floorY = y + 2
    floorPoints = zip (range (-1000) 1000) (repeat floorY) -- this is practically infinite
    in HashSet.union blocks (HashSet.fromList floorPoints)

startBlocked :: Point -> Blocks -> Bool
startBlocked _ blocks = HashSet.member (500, 0) blocks

partB :: Input -> OutputB
partB input = let
    blocks = blocksFromArrows input
    blocksWithFloor = addFloor blocks
    in (fromJust $ findIndex (snd) (dropSandForever blocksWithFloor startBlocked)) - 1
