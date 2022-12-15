module Days.Day09 (runDay) where

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
import Util.Matrix

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
dirParser :: Char -> CardinalDirection -> Parser [CardinalDirection]
dirParser c d = do
    void $ char c
    void $ char ' '
    count <- decimal
    return $ replicate count d

dirsParser :: Parser [CardinalDirection]
dirsParser = (dirParser 'U' North) <|> (dirParser 'D' South) <|> (dirParser 'L' West) <|> (dirParser 'R' East)

inputParser :: Parser Input
inputParser = do
    dirs <- dirsParser `sepBy` endOfLine
    return $ concat dirs

------------ TYPES ------------

type Input = [CardinalDirection]

type OutputA = String

type OutputB = String

------------ PART A ------------

areTouching :: Point -> Point -> Bool
areTouching (a, b) (x, y) = if abs (a - x) <= 1 && abs (b - y) <= 1 then True else False

getTailLocs :: [Point] -> [Point]
getTailLocs = reverse . foldl update []
  where
    update [] v = [v]
    update pos@((x', y'):_) (x, y)
      | areTouching (x', y') (x, y) = pos
      | otherwise = (x' + signum (x-x'), y' + signum (y-y')):pos

partA :: Input -> OutputA
partA input = let
    headLocs = getPath (0, 0) input
    in show $ length . nub . flip (!!) 1 . iterate getTailLocs $ headLocs

------------ PART B ------------

partB :: Input -> OutputB
partB input = let
    headLocs = getPath (0, 0) input
    in show $ length . nub . flip (!!) 9 . iterate getTailLocs $ headLocs
