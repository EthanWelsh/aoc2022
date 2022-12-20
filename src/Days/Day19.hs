module Days.Day19 (runDay) where

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
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

--Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.

blueprintParser :: Parser Blueprint
blueprintParser = do
    void $ string "Blueprint "
    id <- decimal
    void $ string ": Each ore robot costs "
    oreRobotOreCost <- decimal
    void $ string " ore. Each clay robot costs "
    clayRobotOreCost <- decimal
    void $ string " ore. Each obsidian robot costs "
    obsidianRobotOreCost <- decimal
    void $ string " ore and "
    obsidianRobotClayCost <- decimal
    void $ string " clay. Each geode robot costs "
    geodeRobotOreCost <- decimal
    void $ string " ore and "
    geodeRobotObsidianCost <- decimal
    void $ string " obsidian."
    let oreRobotResources = Resources { _ore = oreRobotOreCost, _clay = 0, _obsidian = 0, _geode = 0 }
    let clayRobotResources = Resources { _ore = clayRobotOreCost, _clay = 0, _obsidian = 0, _geode = 0 }
    let obsidianRobotResources = Resources { _ore = obsidianRobotOreCost, _clay = obsidianRobotClayCost, _obsidian = 0, _geode = 0 }
    let geodeRobotResources = Resources { _ore = geodeRobotOreCost, _clay = 0, _obsidian = geodeRobotObsidianCost, _geode = 0 }
    let robots = Map.fromList [(Ore, oreRobotResources), (Clay, clayRobotResources), (Obsidian, obsidianRobotResources), (Geode, geodeRobotResources)]
    return $ Blueprint { _id = id, _robots = robots }


inputParser :: Parser Input
inputParser = blueprintParser `sepBy` endOfLine

------------ TYPES ------------

data Robot = Ore | Clay | Obsidian | Geode deriving (Eq, Show, Ord)

data Resources = Resources { _ore :: Int
                           , _clay :: Int
                           , _obsidian :: Int
                           , _geode :: Int } deriving (Eq, Show)

data Blueprint = Blueprint { _id :: Int
                           , _robots :: Map Robot Resources } deriving (Eq, Show) 

type Input = [Blueprint]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
