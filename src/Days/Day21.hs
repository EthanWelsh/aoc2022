module Days.Day21 (runDay) where

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

nameParser :: Parser String
nameParser = many1 letter

numberParser :: Parser (Map String Operation)
numberParser = do
    name <- nameParser
    void $ string ": "
    num <- decimal
    return $ Map.fromList [(name, Number num)]

binaryOperationPartsParser :: Parser (String, Char, String)
binaryOperationPartsParser = do
    name1 <- nameParser
    void $ string " "
    op <- anyChar 
    void $ string " "
    name2 <- nameParser
    return (name1, op, name2)

binaryOperationParser :: Parser (Map String Operation)
binaryOperationParser = do
    name <- nameParser
    void $ string ": "
    (a, op, b) <- binaryOperationPartsParser
    let operation = case op of 
                        '+' -> Plus a b
                        '-' -> Minus a b
                        '*' -> Multiply a b
                        '/' -> Divide a b
    return $ Map.fromList [(name, operation)]



inputParser :: Parser Input
inputParser = do
    ops <- (numberParser <|> binaryOperationParser) `sepBy` endOfLine
    return $ Map.unions ops


------------ TYPES ------------
data Operation = Number Int | Plus String String | Minus String String | Multiply String String | Divide String String deriving (Eq, Ord, Show)

type Input = Map String Operation

type OutputA = Int

type OutputB = String

------------ PART A ------------
solve :: Map String Operation -> String -> Int
solve m name = let
    op = m Map.! name
    result = case op of
                Number n -> n
                Plus a b ->     (solve m a) +     (solve m b)
                Minus a b ->    (solve m a) -     (solve m b)
                Multiply a b -> (solve m a) *     (solve m b)
                Divide a b ->   (solve m a) `div` (solve m b)
    in result

partA :: Input -> OutputA
partA input = solve input "root"

------------ PART B ------------

getTwoSides :: Operation -> (String, String)
getTwoSides (Plus a b) = (a, b)
getTwoSides (Minus a b) = (a, b)
getTwoSides (Multiply a b) = (a, b)
getTwoSides (Divide a b) = (a, b)

test :: Map String Operation -> Int -> Bool
test m human = let
    newMap = Map.insert "humn" (Number human) m
    (a, b) = getTwoSides $ m Map.! "root"
    leftSide = solve newMap a
    rightSide = solve newMap b
    in leftSide == rightSide

findEqualityPoint :: Map String Operation -> Int
findEqualityPoint m = fromJust $ find (test m) [1..]

partB :: Input -> OutputB
partB input = let
    in show $ findEqualityPoint input
    
    --solve input "root"
