module Days.Day17 (runDay) where

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
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Program.RunDay as R (runDayWithIO, Day)
import Data.Attoparsec.Text hiding (D)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDayWithIO inputParser partA partB

------------ PARSER ------------
directionParser :: Parser Direction
directionParser = do
    c <- (char '<') <|> (char ('>'))
    return $ if c == '<' then L else R

inputParser :: Parser Input
inputParser = many1 directionParser

------------ TYPES ------------
data Direction = L | R | D deriving (Eq, Show)
type Point = (Int, Int)

data Shape = HorizontalLine | Plus | Angle | VerticalLine | Block

type Input = [Direction]

type Points = Set Point

type OutputA = String

type OutputB = String

------------ PART A ------------

getRow :: Point -> Int
getRow p = fst p

getCol :: Point -> Int
getCol p = snd p

-- Given point should be bottom left corner of the shape.
spawnShapeFromPoint :: Point -> Shape -> Points
spawnShapeFromPoint (r, c) HorizontalLine = Set.fromList [(r, c), (r, c + 1), (r, c + 2), (r, c + 3)]
spawnShapeFromPoint (r, c) VerticalLine = Set.fromList [(r, c), (r + 1 , c), (r + 2, c), (r + 3, c)]
spawnShapeFromPoint (r, c) Plus = Set.fromList [(r, c + 1), (r + 1, c), (r + 1, c + 1), (r + 1, c + 2), (r + 2, c + 1)]
spawnShapeFromPoint (r, c) Angle = Set.fromList [(r, c), (r, c + 1), (r, c + 2), (r + 1, c + 2), (r + 2, c + 2)]
spawnShapeFromPoint (r, c) Block = Set.fromList [(r, c), (r, c + 1), (r + 1, c), (r + 1, c + 1)]

spawnShape :: Points -> Shape -> Points
spawnShape points shape = let
    leftEdge = 2
    bottomEdge = getHighestPoint points + 4
    in spawnShapeFromPoint (bottomEdge, leftEdge) shape

getRectangle :: Int -> Int -> [[Point]]
getRectangle width height = [[(r,c) | c <- [0..width]] | r <- [0..height]]

rotatingShapes :: [Shape]
rotatingShapes = cycle [HorizontalLine, Plus, Angle, VerticalLine, Block]

rotatingDirections :: [Direction] -> [Direction]
rotatingDirections directions = intersperse D (cycle directions)

draw :: Int -> Int -> Points -> String
draw width height rockPoints = let
    emptyPoints = reverse $ getRectangle width height
    lines = map (map (\p -> if p `elem` rockPoints then '#' else '.')) emptyPoints
    in intercalate "\n" (map (\l -> "+" ++ l ++ "+" ) lines)

collision :: Points -> Points -> Bool
collision a b = not $ Set.null (Set.intersection a b)

nudge :: Direction -> Points -> Points
nudge L points = Set.map (\(r, c) -> (r, c - 1)) points
nudge R points = Set.map (\(r, c) -> (r, c + 1)) points
nudge D points = Set.map (\(r, c) -> (r - 1, c)) points

getHighestPoint :: Points -> Int
getHighestPoint points = let
    rows = Set.map (getRow) points
    in if Set.null rows then -1 else maximum rows

canMove :: Points -> Points -> Direction -> Bool
canMove points shape dir = not (cantMove points shape dir)

cantMove :: Points -> Points -> Direction -> Bool
cantMove points shape dir = let
    nudged = nudge dir shape
    rows = Set.map getRow nudged
    cols = Set.map getCol nudged
    minRow = Set.findMin rows
    minCol = Set.findMin cols
    maxCol = Set.findMax cols
    collidesWithOther = collision points nudged
    collidesWithFloor = minRow < 0
    collidesWithWall = minCol < 0 || maxCol >= 7
    in collidesWithOther || collidesWithFloor || collidesWithWall

dropShape :: (Points, [Direction], [Shape]) -> (Points, [Direction], [Shape])
dropShape (points, dirs, shape:shapes) = let
    shapePoints = spawnShape points shape
    (newPoints, newDirs) = settleShape points shapePoints dirs
    in (newPoints, newDirs, shapes)

settleShape :: Points -> Points -> [Direction] -> (Points, [Direction])
settleShape points shape (D:dirs)
    | cantMove points shape D = (Set.union points shape, dirs)
settleShape points shape (dir:dirs)
    | canMove points shape dir = settleShape points (nudge dir shape) dirs
    | cantMove points shape dir = settleShape points shape dirs
    

infiniteSimulation :: [Direction] -> [(Points, [Direction], [Shape])]
infiniteSimulation dirs = iterate dropShape (Set.empty, dirs, rotatingShapes)

partA :: Input -> IO ()
partA input = do 
    let dirs = (rotatingDirections input)
    let (points, _, _) = infiniteSimulation dirs !! 2022
    putStrLn $ show $ length points
    putStrLn $ show $ (getHighestPoint points) + 1

------------ PART B ------------
partB :: Input -> IO ()
partB input = do 
    let dirs = (rotatingDirections input)
    let (points, _, _) = infiniteSimulation dirs !! 10000 --1000000000000
    putStrLn $ show $ (getHighestPoint points) + 1
