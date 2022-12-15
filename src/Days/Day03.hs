module Days.Day03 (runDay) where

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
import qualified Util.Parsers as P
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.linesParser

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

splitInHalf :: String -> (String, String)
splitInHalf s = splitAt ((length s) `div`  2) s

inCommon :: (String, String) -> String
inCommon (a, b) = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)

score :: Char -> Int
score c = let charToScore = Map.fromList $ zip (['a'..'z']++['A'..'Z']) [1..52] 
          in charToScore Map.! c

scoreLine :: String -> Int
scoreLine l = sum $ map score $ inCommon $ splitInHalf l

partA :: Input -> OutputA
partA input = sum $ map scoreLine input

------------ PART B ------------
groupsOfThree :: [String] -> [(String, String, String)]
groupsOfThree [] = []
groupsOfThree (a:b:c:xs) = (a, b, c) : groupsOfThree xs

inCommonThree :: (String, String, String) -> Char
inCommonThree (a, b, c) = head $ inCommon (inCommon (a, b), c)

partB :: Input -> OutputB
partB input = sum $ map (score . inCommonThree) (groupsOfThree input)
