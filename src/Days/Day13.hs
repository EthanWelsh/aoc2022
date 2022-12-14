module Days.Day13 (runDay) where

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
import Text.Megaparsec 
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Program.RunDay as R (runDayWithParser, megaparsecParser, Day)
import Data.Void
{- ORMOLU_ENABLE -}

type Parser = Parsec Void String

runDay :: R.Day
runDay = R.runDayWithParser (R.megaparsecParser inputParser) partA partB

------------ PARSER ------------

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:ys) = (x, y) : (pairs ys)

packetParser :: Parser Packet
packetParser = choice [Item <$> decimal, List <$> between (single '[') (single ']') (packetParser `sepBy` single ',')] 

inputParser :: Parser Input
inputParser = packetParser `sepEndBy` some eol

------------ TYPES ------------

data Packet = Item Int | List [Packet] deriving (Eq)

instance Show Packet where
  show (Item x) = show x
  show (List ps) = let
    packets = map show ps
    in "[" ++ (intercalate "," packets) ++ "]"

type Input = [Packet]

type OutputA = String

type OutputB = String

data Validity = Valid | Invalid | Unknown deriving (Show, Eq)

------------ PART A ------------

instance Ord Packet where
    compare (Item l) (Item r) = compare l r
    compare (List []) (List []) = EQ
    compare (List []) (List (r:rs)) = LT
    compare (List (l:ls)) (List []) = GT
    compare (List l) (Item r) = compare (List l) (List [(Item r)])
    compare (Item l) (List r) = compare (List [(Item l)]) (List r)
    compare (List (l:ls)) (List (r:rs)) = let
        headValidity = compare l r
        in if headValidity == EQ then compare (List ls) (List rs) else headValidity

isValid :: (Packet, Packet) -> Bool
isValid (l, r) = l < r

partA :: Input -> OutputA
partA input = let
    packetPairs = pairs input
    pairsWithIndex = zip [1..] packetPairs
    onlyValid = filter (\(i, pair) -> isValid pair) pairsWithIndex
    in show $ sum $ map fst onlyValid

------------ PART B ------------

partB :: Input -> OutputB
partB input = let
    extras = [List[List[Item 2]], List[List[Item 6]]]
    packets = sort $ (extras ++) $ input
    in show $ product $ map (+1) $ findIndices (`elem` extras) packets
