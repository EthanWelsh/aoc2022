module Days.Day02 (runDay) where

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
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

charToFirst :: Char -> FirstColumn
charToFirst 'A' = A
charToFirst 'B' = B
charToFirst 'C' = C

charToSecond :: Char -> SecondColumn
charToSecond 'X' = X
charToSecond 'Y' = Y
charToSecond 'Z' = Z

row :: Parser (FirstColumn, SecondColumn)
row = do
    first <- char 'A' <|> char 'B' <|> char 'C'
    void $ char ' '
    second <- char 'X' <|> char 'Y' <|> char 'Z'
    return (charToFirst first, charToSecond second)

inputParser :: Parser Input
inputParser = row `sepBy` endOfLine

------------ TYPES ------------

data Shape = Rock | Paper | Scissor
  deriving (Eq, Show)

data GameState = Win | Lose | Draw

data FirstColumn  = A | B | C
  deriving (Eq, Show)

data SecondColumn = X | Y | Z
  deriving (Eq, Show)

type Input = [(FirstColumn, SecondColumn)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
firstToShape :: FirstColumn -> Shape
firstToShape A = Rock
firstToShape B = Paper
firstToShape C = Scissor

secondToShape :: SecondColumn -> Shape
secondToShape X = Rock
secondToShape Y = Paper
secondToShape Z = Scissor

playGame :: (FirstColumn -> Shape) -> (SecondColumn -> Shape) -> (FirstColumn, SecondColumn) -> (Shape, Shape)
playGame fcToShape scToShape (first, second) = (fcToShape first, scToShape second)

playGames :: [(FirstColumn, SecondColumn)] -> (FirstColumn -> Shape) -> (SecondColumn -> Shape) -> [(Shape, Shape)]
playGames games fcToShape scToShape = map (playGame fcToShape scToShape) games

gameState :: (Shape, Shape) -> GameState
gameState (Rock,    Rock)    = Draw
gameState (Paper,   Paper)   = Draw
gameState (Scissor, Scissor) = Draw
gameState (Rock,    Paper)   = Win
gameState (Paper,   Scissor) = Win
gameState (Scissor, Rock)    = Win
gameState (Rock,    Scissor) = Lose
gameState (Paper,   Rock)    = Lose
gameState (Scissor, Paper)   = Lose

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

gameStateScore :: GameState -> Int
gameStateScore Win = 6
gameStateScore Draw = 3
gameStateScore Lose = 0

gameScore :: (Shape, Shape) -> Int
gameScore (theirs, mine) = shapeScore mine + gameStateScore (gameState (theirs, mine))

scoreGames :: [(Shape, Shape)] -> Int
scoreGames games = sum $ map gameScore games

partA :: Input -> OutputA
partA input = scoreGames $ playGames input firstToShape secondToShape

------------ PART B ------------

pickRightShape :: Shape -> GameState -> Shape
pickRightShape Rock    Lose = Scissor
pickRightShape Rock    Draw = Rock
pickRightShape Rock    Win  = Paper
pickRightShape Paper   Lose = Rock
pickRightShape Paper   Draw = Paper
pickRightShape Paper   Win  = Scissor
pickRightShape Scissor Lose = Paper
pickRightShape Scissor Draw = Scissor
pickRightShape Scissor Win  = Rock

secondToGameState :: SecondColumn -> GameState
secondToGameState X = Lose
secondToGameState Y = Draw
secondToGameState Z = Win

toShapeAndGameState :: (FirstColumn, SecondColumn) -> (Shape, GameState)
toShapeAndGameState (fc, sc) = (firstToShape fc, secondToGameState sc)

toShapes :: (Shape, GameState) -> (Shape, Shape)
toShapes (shape, gameState) = (shape, pickRightShape shape gameState)

partB :: Input -> OutputB
partB input = let a = map toShapeAndGameState input
                  b = map toShapes a
                  in scoreGames b

