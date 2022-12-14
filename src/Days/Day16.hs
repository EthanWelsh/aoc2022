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
import Algorithm.Search
import Data.Functor (($>))
import Data.Function
import qualified Program.RunDay as R (runDayWithIO, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDayWithIO inputParser partA partB

------------ PARSER ------------
valveParser :: Parser String
valveParser = count 2 letter

singleValve :: Parser [String]
singleValve = do
    void $ string "; tunnel leads to valve "
    valve <- valveParser
    return [valve]

multipleValve :: Parser [String]
multipleValve = do
    void $ string "; tunnels lead to valves "
    valveParser `sepBy` string ", "

lineParser :: Parser (String, Valve)
lineParser = do
    void $ string "Valve "
    origin <- valveParser
    void $ string " has flow rate="
    flow <- decimal
    valves <- singleValve <|> multipleValve
    return (origin, Valve flow (Set.fromList valves))

inputParser :: Parser Graph
inputParser = do
    valves <- lineParser `sepBy` endOfLine
    return $ Map.fromList valves

------------ TYPES ------------
type Input = Graph

type Graph = Map String Valve

type Costs = Map String (Map String Int)

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

helpfulValves :: Graph -> [String]
helpfulValves graph = Map.keys $ Map.filter ((> 0) . valvePressure) graph

pathCost :: Graph -> String -> String -> Int
pathCost graph origin destination = fst . fromJust $ dijkstra neighbors cost isGoal origin
    where
        neighbors v = valveNeighbors (graph Map.! v)
        cost _ _ = 1
        isGoal = (== destination)

mapify :: (Ord a, Ord b) => [(a, b, c)] -> Map a (Map b c)
mapify = foldl' (\m (o, d, c) -> Map.insertWith Map.union o (Map.singleton d c) m) Map.empty

getCosts :: Graph -> Costs
getCosts graph = let
    helpful = helpfulValves graph
    allPaths = [(o, d) | o <- "AA":helpful, d <- helpful, o /= d]
    pathsWithCost = map (\(o, d) -> (o, d, pathCost graph o d)) allPaths :: [(String, String, Int)]
    in mapify pathsWithCost

costFromTo :: Costs -> String -> String -> Int
costFromTo costs origin destination = (costs Map.! origin) Map.! destination

visitDestination :: Graph -> Costs -> State -> String -> Maybe State
visitDestination graph costs state destination = let
    origin = location state
    costToTravel = costFromTo costs origin destination
    costToOpenValve = 1
    timeElapsed = costToTravel + costToOpenValve
    newMinute = minute state + timeElapsed
    newTotalPressure = totalPressure state + timeElapsed * pressurePerMinute state
    newPressurePerMinute = pressurePerMinute state + valvePressure (graph Map.! destination)
    newOpenValves = Set.insert destination (openValves state)
    timeUp = newMinute > 30
    in if timeUp then Nothing else Just $ state {
        location = destination,
        minute = newMinute,
        totalPressure = newTotalPressure,
        pressurePerMinute = newPressurePerMinute,
        openValves = newOpenValves }

totalPressureAtEnd :: State -> Int
totalPressureAtEnd state = let
    timeRemaining = 30 - minute state
    in totalPressure state + (timeRemaining * pressurePerMinute state)

visitAll :: Graph -> Costs -> State -> State
visitAll graph costs state = let
    origin = location state                                                        :: String
    unopened = filter (`notElem` openValves state) . Map.keys $ costs Map.! origin :: [String]
    children = mapMaybe (visitDestination graph costs state) unopened              :: [State]
    bestForEachChild = map (visitAll graph costs) children                         :: [State]
    bestOverall = maximumBy (compare `on` totalPressure) bestForEachChild          :: State
    timeRemaining = 30 - minute state
    pressureIfWaitTillEnd = totalPressure state + timeRemaining * pressurePerMinute state
    in if null children
        then state { minute = 30, totalPressure = pressureIfWaitTillEnd }
        else bestOverall

partA :: Input -> IO ()
partA graph = do
    let costs = getCosts graph
    print $ helpfulValves graph
    print $ visitAll graph costs startState

------------ PART B ------------
partB :: Input -> IO ()
partB graph = print "hello"
