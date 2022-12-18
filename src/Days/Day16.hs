module Days.Day16 (runDay) where

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
import qualified Program.RunDay as R (runDayWithIO, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDayWithIO inputParser partA partB

------------ PARSER ------------
valveParser :: Parser String
valveParser = (count 2 letter)

singleValve :: Parser [String]
singleValve = do
    void $ string "; tunnel leads to valve "
    valve <- valveParser
    return [valve]

multipleValve :: Parser [String]
multipleValve = do
    void $ string "; tunnels lead to valves "
    valves <- valveParser `sepBy` (string ", ")
    return valves

lineParser :: Parser (String, Valve)
lineParser = do
    void $ string "Valve "
    origin <- valveParser
    void $ string " has flow rate="
    flow <- decimal
    valves <- try singleValve <|> try multipleValve
    return (origin, Valve flow (Set.fromList valves))

inputParser :: Parser Graph
inputParser = do
    valves <- lineParser `sepBy` endOfLine
    return $ Map.fromList valves

------------ TYPES ------------
type Input = Graph

type Graph = Map String Valve

data Valve = Valve { valvePressure  :: Int
                   , valveNeighbors :: Set String } deriving (Eq, Show, Ord)

data State = State { location          :: String
                   , minute            :: Int
                   , pressurePerMinute :: Int
                   , totalPressure     :: Int 
                   , openValves        :: Set String } deriving (Eq, Show, Ord)

type OutputA = Int

type OutputB = Int

------------ PART A ------------

startState :: State
startState = State { location = "AA", minute = 0, pressurePerMinute = 0, totalPressure = 0, openValves = Set.empty }

getValve :: Graph -> String -> Valve
getValve graph valve = fromJust $ Map.lookup valve graph

isAlreadyOpen :: Graph -> State -> String -> Bool
isAlreadyOpen graph state v = let
    valve = getValve graph v
    valveOpen = v `elem` (openValves state)
    pressureZero = (valvePressure valve) == 0
    in valveOpen || pressureZero

isTimeUp :: State -> Bool
isTimeUp state = (minute state) > 30

openValve :: Graph -> State -> Set State
openValve graph state = let
    valve = location state
    (Valve valvePressure _) = getValve graph valve
    newMinute = (minute state) + 1
    newPressurePerMinute = (pressurePerMinute state) + valvePressure
    newTotalPressure = (totalPressure state) + (pressurePerMinute state)
    newOpenValves = Set.insert valve (openValves state)
    alreadyOpen = isAlreadyOpen graph state valve
    timeUp = isTimeUp state
    in if alreadyOpen || timeUp then Set.empty else Set.singleton $ state { 
        minute = newMinute, 
        pressurePerMinute = newPressurePerMinute,
        totalPressure = newTotalPressure,
        openValves = newOpenValves }

visitNeighbor :: Graph -> State -> String -> Set State
visitNeighbor graph state newLocation = let
    newMinute = (minute state) + 1
    newTotalPressure = (totalPressure state) + (pressurePerMinute state)
    timeUp = isTimeUp state
    in if timeUp then Set.empty else Set.singleton $ state { 
        location = newLocation, 
        minute = newMinute,
        totalPressure = newTotalPressure }

visitNeighbors :: Graph -> State -> Set State
visitNeighbors graph state = let
    valve = location state
    (Valve _ ns) = getValve graph valve
    neighborStates = Set.map (visitNeighbor graph state) ns
    in Set.unions neighborStates

search :: Graph -> State -> Set State
search graph state = let
    min = minute state
    openOrNot = Set.unions [Set.singleton state, openValve graph state]
    nextStates = Set.unions $ Set.map (visitNeighbors graph) openOrNot :: Set State
    in if (minute state) == 30 then Set.singleton state else Set.unions $ Set.map (search graph) nextStates 

partA :: Input -> IO ()
partA graph = do
    let allStates = search graph startState
    let pressures = Set.map (totalPressure) allStates
    print $ maximum pressures

------------ PART B ------------
partB :: Input -> IO ()
partB graph = do
    print $ Map.keys graph
