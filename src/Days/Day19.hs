{-# LANGUAGE TemplateHaskell #-}

module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (void)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Maybe
import Control.Lens hiding (element)
import Options.Applicative.Help (blue)
import qualified Data.Set as Set
{- ORMOLU_ENABLE -}

------------ TYPES ------------
data Material = Ore | Clay | Obsidian | Geode deriving (Eq, Show, Ord)

data Blueprint = Blueprint { _blueprintId :: Int
                           , _costs :: Map Material (Map Material Int) } deriving (Eq, Show)

data State = State { _minute :: Int 
                   , _robots :: Map Material Int
                   , _resources :: Map Material Int } deriving (Eq, Show)

type Input = [Blueprint]

type OutputA = String

type OutputB = String

$(makeLenses ''Blueprint)
$(makeLenses ''State)

------------ PARSER ------------
blueprintParser :: Parser Blueprint
blueprintParser = do
    void $ string "Blueprint "
    idd <- decimal
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
    let oreRobotResources = Map.fromList [(Ore, oreRobotOreCost), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    let clayRobotResources = Map.fromList [(Ore, clayRobotOreCost), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    let obsidianRobotResources = Map.fromList [(Ore, obsidianRobotOreCost), (Clay, obsidianRobotClayCost), (Obsidian, 0), (Geode, 0)] 
    let geodeRobotResources = Map.fromList [(Ore, geodeRobotOreCost), (Clay, 0), (Obsidian, geodeRobotObsidianCost), (Geode, 0)]
    let rbs = Map.fromList [(Ore, oreRobotResources), (Clay, clayRobotResources), (Obsidian, obsidianRobotResources), (Geode, geodeRobotResources)]
    return $ Blueprint { _blueprintId = idd, _costs = rbs }

inputParser :: Parser Input
inputParser = blueprintParser `sepBy` endOfLine

------------ PART A ------------

startState :: State 
startState = State { _minute = 0
                   , _robots = Map.fromList [(Ore, 1), (Clay, 0), (Obsidian, 0), (Geode, 0)]
                   , _resources = Map.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]}

maxResourcesNeeded :: Blueprint -> Map Material Int
maxResourcesNeeded blueprint = foldl (Map.unionWith max) Map.empty (Map.elems (blueprint^.costs))

elapseMinute :: State -> State
elapseMinute state = let
    newMinute = (state^.minute) + 1
    newResources = Map.unionWith (+) (state^.robots) (state^.resources)
    in state { _minute = newMinute, _resources = newResources }

elapseMinutes :: State -> Int -> State
elapseMinutes state c = (iterate elapseMinute state) !! c

purchaseRobot :: Blueprint -> State -> Material -> Maybe State
purchaseRobot blueprint state material = let
    materialCosts = (blueprint ^. costs) Map.! material
    newResources = Map.unionWith (-) (state ^. resources) materialCosts
    newRobots = Map.adjust (+1) material (state ^. robots)
    cantAfford = any (<0) $ Map.elems newResources
    moreThanNeeded = any (<0) $ Map.elems $ Map.unionWith (-) (maxResourcesNeeded blueprint) newResources
    in if cantAfford || moreThanNeeded 
        then Nothing 
        else Just state { _robots = newRobots, _resources = newResources }

minutesNeeded :: Blueprint -> State -> Material -> Maybe Int
minutesNeeded blueprint state buy = let
    costPerResource = map (minutesNeededForMaterial blueprint state buy) [Ore, Clay, Obsidian]
    unattainable = any isNothing costPerResource
    in if unattainable then Nothing else Just $ maximum $ map fromJust costPerResource
    where
        minutesNeededForMaterial :: Blueprint -> State -> Material -> Material -> Maybe Int
        minutesNeededForMaterial blueprint state buy spend = let
            cost = ((blueprint ^. costs) Map.! buy) Map.! spend
            currentAmt = (state ^. resources) Map.! spend
            robotCount = (state ^. robots) Map.! spend
            missingAmt = cost - currentAmt
            in 
                if missingAmt == 0 
                    then Just 0
                else if robotCount == 0
                    then Nothing
                else Just $ ceiling (fromIntegral missingAmt / fromIntegral robotCount)

tryToBuy :: Blueprint -> State -> Material -> Maybe State
tryToBuy blueprint state buy = let
    mins = minutesNeeded blueprint state buy                :: Maybe Int
    withElapse = elapseMinutes state (fromJust mins)        :: State
    withPurchase = purchaseRobot blueprint withElapse buy   :: Maybe State
    newMin = fromJust withPurchase ^. minute
    timeIsUp = newMin > 24
    in if isNothing mins || isNothing withPurchase || timeIsUp then Nothing else withPurchase

getGeodeCount :: State -> Int
getGeodeCount s = (s ^. resources) Map.! Geode

search :: Blueprint -> State -> Int
search blueprint state = let
    states = mapMaybe (tryToBuy blueprint state) [Ore, Clay, Obsidian, Geode] :: [State]
    explored = map (search blueprint) states
    in if null states then getGeodeCount state else maximum explored

partA :: Input -> OutputA
partA input = let
    blueprint = input !! 0
    state = startState
    in show $ search blueprint state

------------ PART B ------------
partB :: Input -> OutputB
partB input = let
    blueprint = input !! 0
    state = startState
    in show $ tryToBuy blueprint state Ore

------------ RUN ---------------
runDay :: R.Day
runDay = R.runDay inputParser partA partB
