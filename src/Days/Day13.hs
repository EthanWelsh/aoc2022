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

data Packet = Item Int | List [Packet] deriving (Show, Eq)

type Input = [Packet]

type OutputA = String

type OutputB = String

------------ PART A ------------

partA :: Input -> OutputA
partA input = "part a"

------------ PART B ------------
partB :: Input -> OutputB
partB input = "part b"
