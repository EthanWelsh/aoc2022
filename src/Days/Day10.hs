module Days.Day10 (runDay) where

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
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
addParser :: Parser Operation
addParser = do
    void $ string "addx "
    x <- signed decimal
    return (Addx x)

noopParser :: Parser Operation
noopParser = do
    void $ string "noop"
    return Noop

operationParser :: Parser Operation
operationParser = addParser <|> noopParser

inputParser :: Parser Input
inputParser = operationParser `sepBy` endOfLine

------------ TYPES ------------
data Operation = Addx Int | Noop deriving (Eq, Show)

type Input = [Operation]

type OutputA = Int

type OutputB = String

------------ PART A ------------

getStateEachCycle :: [Operation] -> [Int]
getStateEachCycle ops = reverse $ foldl stateHelper [1] ops

stateHelper :: [Int] -> Operation -> [Int]
stateHelper states@(x:_) Noop = (x:states)
stateHelper states@(x:_) (Addx y) = ((x+y):x:states)

getSignalStrengths :: [Int] -> [Int]
getSignalStrengths xs = map (uncurry (*)) (zip [1..] xs)

partA :: Input -> OutputA
partA input = let 
    stateEachCycle = getStateEachCycle input
    signalStrengths = getSignalStrengths stateEachCycle
    interestingCycles = [20, 60, 100, 140, 180, 220]
    cycles = map (\index -> signalStrengths !! (index - 1)) interestingCycles
    in sum $ cycles

------------ PART B ------------
spritePosition :: Int -> [Int]
spritePosition x = [x - 1, x, x + 1]

getCrtRows :: [Int] -> String
getCrtRows xs = let 
    states = zip (concat (replicate 6 [0..39])) xs
    in foldl crtHelper "" states

crtHelper :: String -> (Int, Int) -> String
crtHelper row (crtIndex, x) = let
    charToAdd = if crtIndex `elem` (spritePosition x) then '#' else '.'
    in row ++ [charToAdd] 

breakIntoGroupsOf :: Int -> [Char] -> [[Char]]
breakIntoGroupsOf _ [] = []
breakIntoGroupsOf groupSize xs = let
    splits = splitAt groupSize xs
    in [fst splits] ++ (breakIntoGroupsOf groupSize (snd splits))


partB :: Input -> OutputB
partB input = let
    stateEachCycle = getStateEachCycle input
    in intercalate "\n" (breakIntoGroupsOf 40 (getCrtRows stateEachCycle))
