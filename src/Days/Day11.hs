module Days.Day11 (runDay) where

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
import Data.Vector (Vector)
import Control.Monad.State as State
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

indexParser :: Parser Int
indexParser = do
    void $ string "Monkey "
    index <- decimal
    void $ char ':'
    void $ endOfLine
    return index

startingItemsParser :: Parser [Int]
startingItemsParser = do
    void $ string "  Starting items: "
    items <- decimal `sepBy` (string ", ")
    void $ endOfLine
    return items

operationParser :: Parser (Item -> Int)
operationParser = do
    void $ string "  Operation: new = old "
    opr <- (char '+') <|> (char '*')
    void $ char ' '
    numberOrOld <- eitherP (decimal) (string "old")
    void $ endOfLine
    return (case numberOrOld of 
        Left num    -> if opr == '+' then (\old -> old + num) else (\old -> old * num)
        Right "old" -> if opr == '+' then (\old -> old + old) else (\old -> old * old))

divisibleBy n x = x `rem` n == 0

testParser :: Parser Int
testParser = do
    void $ string "  Test: divisible by "
    n <- decimal
    void $ endOfLine
    return n

trueParser :: Parser Int
trueParser = do
    void $ string "    If true: throw to monkey "
    index <- decimal
    void $ endOfLine
    return index

falseParser :: Parser Int
falseParser = do
    void $ string "    If false: throw to monkey "
    index <- decimal
    void $ endOfLine <|> endOfInput
    return index

decisionParser :: Parser Test
decisionParser = do 
    test <- testParser
    trueIndex <- trueParser
    falseIndex <- falseParser
    return $ (Test test trueIndex falseIndex)

monkeyParser :: Parser Monkey
monkeyParser = do
    index <- indexParser
    items <- startingItemsParser
    operation <- operationParser
    decision <- decisionParser
    return (Monkey index items operation decision 0)

inputParser :: Parser Input
inputParser = monkeyParser `sepBy` (string "\n")

------------ TYPES ------------
type Item = Int
type Index = Int

data Test = Test Int Index Index

data Monkey = Monkey { getIndex :: Int
                     , getItems :: [Item]
                     , getOperation :: (Item -> Int)
                     , getDecision :: Test
                     , getObservationCount :: Int }

instance Show Monkey where
  show monkey = "{MONKEY " ++ show (getIndex monkey) ++ ", " ++ show (getItems monkey) ++ "}"

type Input = [Monkey]

type OutputA = String

type OutputB = String

------------ PART A ------------

makeDecision :: Test -> Int -> Int
makeDecision (Test d t f) n = if (d `divisibleBy` n) then t else f

reduceWorryLevel :: Int -> Int
reduceWorryLevel x = x `div` 3

removeItemFromMonkey :: Int -> Monkey -> Monkey
removeItemFromMonkey item monkey = monkey { getItems = delete item (getItems monkey) }

addItemToMonkey :: Int -> Monkey -> Monkey
addItemToMonkey item monkey = monkey { getItems = (getItems monkey) ++ [item] }

getMonkey :: Int -> State (Vector Monkey) Monkey
getMonkey index = do
    monkeys <- get
    let monkey = monkeys Vec.! index
    return monkey

putMonkey :: Monkey -> State (Vector Monkey) ()
putMonkey monkey = do
    let index = getIndex monkey
    monkeys <- get
    put $ monkeys Vec.// [(index, monkey)]
    return ()

incrementObservations :: Int -> Int -> State (Vector Monkey) ()
incrementObservations index count = do
    monkey <- getMonkey index
    let updatedMonkey = monkey { getObservationCount = (getObservationCount monkey) + count }
    putMonkey updatedMonkey
    return ()

takeTurn :: (Int -> Int) -> Int -> State (Vector Monkey) ()
takeTurn f fromIndex = do
    fromMonkey <- getMonkey fromIndex
    let items = getItems fromMonkey
    case items of 
        [] -> return ()
        (x:_) -> do
            let worryLevel = (getOperation fromMonkey) x
            
            let reducedWorry = f worryLevel

            let test = getDecision fromMonkey
            let toIndex = makeDecision test reducedWorry
            toMonkey <- getMonkey toIndex

            let updatedFrom = removeItemFromMonkey x fromMonkey 
            let updatedTo = addItemToMonkey reducedWorry toMonkey

            putMonkey updatedFrom
            putMonkey updatedTo

            incrementObservations fromIndex 1

            takeTurn f fromIndex
            return ()

playRounds :: (Int -> Int) -> State (Vector Monkey) ()
playRounds f = do
    monkeys <- get
    mapM (takeTurn f) [0..((length monkeys) - 1)]
    return ()

getCounts :: State (Vector Monkey) [Int]
getCounts = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getObservationCount monkeys

getItemsForEachMonkey :: State (Vector Monkey) [[Int]]
getItemsForEachMonkey = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getItems monkeys

playA :: Int -> State (Vector Monkey) Int
playA rounds = do
    replicateM rounds (playRounds reduceWorryLevel)
    counts <- getCounts
    let sorted = (reverse . sort) counts :: [Int]
    let topTwo = Data.List.take 2 sorted :: [Int]
    let score = Data.List.product topTwo :: Int
    return $ score

partA :: Input -> OutputA
partA input = show $ evalState (playA 20) (Vec.fromList input)

------------ PART B ------------

getTestsForEachMonkey :: State (Vector Monkey) [Test]
getTestsForEachMonkey = do
    monkeys <- get
    return $ Vec.toList $ Vec.map getDecision monkeys


playB :: Int -> State (Vector Monkey) Int
playB rounds = do
    tests <- getTestsForEachMonkey
    let divs = map (\(Test n _ _) -> n) tests
    let prodDivs = Data.List.product divs
    let reduceB = (`mod` prodDivs)
    replicateM rounds (playRounds reduceB)
    counts <- getCounts
    let sorted = (reverse . sort) counts :: [Int]
    let topTwo = Data.List.take 2 sorted :: [Int]
    let score = Data.List.product topTwo :: Int
    return $ score

partB :: Input -> OutputB
partB input = show $ evalState (playB 10000) (Vec.fromList input)
