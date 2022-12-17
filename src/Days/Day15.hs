module Days.Day15 (runDay) where

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
import Util.Matrix hiding (cardinalPoints)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Range
import Control.Monad (void)
import qualified Data.HashSet as HashSet

{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

lineParser :: Parser (Point, Point)
lineParser = do
    void $ string "Sensor at x="
    sensorX <- signed decimal
    void $ string ", y="
    sensorY <- signed decimal
    void $ string ": closest beacon is at x="
    beaconX <- signed decimal
    void $ string ", y="
    beaconY <- signed decimal

    -- Convert to (r,c) instead of (x,y)
    let sensor = (sensorY, sensorX)
    let beacon = (beaconY, beaconX)

    return (sensor, beacon)

inputParser :: Parser Input
inputParser = lineParser `sepBy` endOfLine

------------ TYPES ------------
type Points = HashSet.HashSet Point

type Input = [(Point, Point)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

convertToRowsAndCols (x, y) = (y, x)

getRow :: Point -> Int
getRow (r, _) = r

getCol :: Point -> Int
getCol (_, c) = c

getSignal :: (Point, Point) -> Point
getSignal (s, _) = s

getBeacon :: (Point, Point) -> Point
getBeacon (_, b) = b

manhattanDistance :: Point -> Point -> Int
manhattanDistance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

coversRows :: Point -> Point -> Range Int
coversRows (r1, _) (r2, _) = r1 +=+ r2

rangeSize :: Range Int -> Int
rangeSize (SpanRange (Bound a Inclusive) (Bound b Inclusive)) = abs (a-b) + 1
rangeSize (SpanRange (Bound a Exclusive) (Bound b Exclusive)) = abs (a-b) - 2
rangeSize (SpanRange (Bound a Inclusive) (Bound b Exclusive)) = abs (a-b)
rangeSize (SpanRange (Bound a Exclusive) (Bound b Inclusive)) = abs (a-b)

shrinkInBothDirectionsBy :: Range Int -> Int -> Maybe (Range Int)
shrinkInBothDirectionsBy (SpanRange (Bound aval Inclusive) (Bound bval Inclusive)) x = let
    newA = aval + x
    newB = bval - x
    in if newA >= newB then Nothing else Just (SpanRange (Bound (aval+x) Inclusive) (Bound (bval-x) Inclusive))

coversTargetRow :: Int -> (Point, Point) -> Bool
coversTargetRow target (a, b) = inRange (coversRows a b) target

projectOutwards :: Int -> (Point, Point) -> Maybe (Range Int)
projectOutwards target (s@(r1, c1), b@(r2, c2)) = let
    distance = manhattanDistance s b
    verticalDistance = abs (r1 - target)
    coveredAtOrigin = (c1 - distance) +=+ (c1 + distance)
    in shrinkInBothDirectionsBy coveredAtOrigin verticalDistance

impossibleBeaconLocationsOnRow :: [(Point, Point)] -> Int -> [Range Int]
impossibleBeaconLocationsOnRow input targetRow = let 
    colsCoveredOnTargetRow = catMaybes $ map (projectOutwards targetRow) input      :: [Range Int]
    joined = joinRanges colsCoveredOnTargetRow                                      :: [Range Int]
    beacons = map (getBeacon) input                                                 :: [Point]
    beaconsOnTargetRow = filter ( (==targetRow) . getRow) beacons                   :: [Point]
    beaconColsOnTargetRow = map (SingletonRange . getCol) beaconsOnTargetRow        :: [Range Int]
    rowsWithNoBeacons = difference joined beaconColsOnTargetRow                     :: [Range Int]
    in rowsWithNoBeacons

partA :: Input -> Int
partA input = let
    targetRow = 10
    rowsWithNoBeacons = impossibleBeaconLocationsOnRow input targetRow
    in sum $ map rangeSize rowsWithNoBeacons

------------ PART B ------------

rangeToList :: Range Int -> [Int]
rangeToList (SingletonRange a) = [a]
rangeToList (SpanRange (Bound a Inclusive) (Bound b Inclusive)) = [a     .. b]
rangeToList (SpanRange (Bound a Inclusive) (Bound b Exclusive)) = [a     .. (b-1)]
rangeToList (SpanRange (Bound a Exclusive) (Bound b Inclusive)) = [(a+1) .. (b)]
rangeToList (SpanRange (Bound a Exclusive) (Bound b Exclusive)) = [(a+1) .. (b-1)]

possibleBeaconLocationsOnRow :: [(Point, Point)] -> Range Int -> Int -> [Point]
possibleBeaconLocationsOnRow input inBounds targetRow = let 
    impossibleRanges = impossibleBeaconLocationsOnRow input targetRow
    possibleRanges = difference [inBounds] impossibleRanges
    possibleCols = concatMap rangeToList possibleRanges
    in zip (repeat targetRow) possibleCols 

possibleLocations :: [(Point, Point)] -> Int -> Int -> [Point]
possibleLocations input lower upper = let
    inBounds = lower +=+ upper
    allCols = [lower..upper]
    in concat $ map (possibleBeaconLocationsOnRow input inBounds) allCols

partB :: Input -> Int
partB input = let 
    locations = HashSet.fromList $ possibleLocations input 0 4000000
    beacons = HashSet.fromList $ map getBeacon input
    beaconLocation = head $ HashSet.toList $ HashSet.difference locations beacons
    (r, c) = beaconLocation
    in (c*4000000) * r
