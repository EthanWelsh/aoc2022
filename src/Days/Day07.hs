module Days.Day07 (runDay) where

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
cdParser :: Parser Command
cdParser = do
    void $ string "$ cd "
    name <- many1 (notChar '\n')
    void $ endOfLine
    return (CD name)

lsFileResultParser :: Parser Entity
lsFileResultParser = do
    size <- decimal
    void $ char ' '
    name <- many1 (notChar '\n')
    return File {name = name, size = size} 

lsDirResultParser :: Parser Entity
lsDirResultParser = do
    void $ string "dir "
    name <- many1 (notChar '\n')
    return Directory { name = name, children = [], parent = Nil}

lsParser :: Parser Command
lsParser = do
    void $ string "$ ls"
    void $ endOfLine
    results <- (lsFileResultParser <|> lsDirResultParser) `sepBy` endOfLine
    void $ endOfLine <|> endOfInput
    return (LS results)

inputParser :: Parser Input
inputParser = many1 (cdParser <|> lsParser)

------------ TYPES ------------

data Command = CD String | LS [Entity] deriving (Eq, Show)

data Entity = Nil | File { name :: String, size :: Int } | Directory { name :: String, children :: [Entity], parent :: Entity } deriving (Eq, Show)

type Input = [Command]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

isDir :: Entity -> Bool
isDir (Directory _ _ _) = True
isDir _ = False

setResults :: Entity -> [Entity] -> Entity
setResults (Directory n _ p) results = (Directory n results p)

setParent :: Entity -> Entity -> Entity
setParent (Directory n r _) p = Directory n r p
setParent (File n s) _ = File n s

removeChild :: Entity -> String -> Entity
removeChild (Directory n rs p) toRemove = let
    newRs = filter ((/= toRemove) . name) rs
    in (Directory n newRs p)

addChild :: Entity -> Entity -> Entity
addChild (Directory n rs p) r = (Directory n (r:rs) p)

repeatUntil :: (a -> a) -> (a -> Bool) -> a -> a
repeatUntil f p a = if (p a) then a else repeatUntil f p (f a)

up :: Entity -> Entity
up focus = addChild (parent focus) focus

hasParent :: Entity -> Bool
hasParent (Directory _ _ Nil) = False
hasParent _ = True

allTheWayUp :: Entity -> Entity
allTheWayUp d = repeatUntil up (not . hasParent) d

setParentAndRemoveSelf :: Entity -> Entity -> Entity
setParentAndRemoveSelf focus parent = let
    parentWithoutChild = (removeChild parent (name focus))
    in setParent focus parentWithoutChild

commandToEntity :: Entity -> Command -> Entity
commandToEntity focus (LS results) = let
    newFocus = setResults focus results
    newResults = map (\r -> setParentAndRemoveSelf r newFocus) results
    in setResults focus newResults
commandToEntity focus (CD "..") = addChild (parent focus) focus
commandToEntity focus (CD n) = Directory { name = n, children = [], parent = (removeChild focus n) }

commandsToEntity :: [Command] -> Entity
commandsToEntity cs = let
    root = Directory { name = "/", children = [], parent = Nil}
    in helper root cs

helper :: Entity -> [Command] -> Entity
helper focus [] = focus
helper focus (c:cs) = helper (commandToEntity focus c) cs

getEntitySize :: Entity -> Int
getEntitySize (File _ s) = s
getEntitySize (Directory _ cs _) = sum $ map getEntitySize cs

getAllDescendents :: Entity -> [Entity]
getAllDescendents (Directory n cs p) = (Directory n cs p) : (concatMap getAllDescendents cs)
getAllDescendents file = [file]

partA :: Input -> OutputA
partA input = let
    tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
    allDescendents = getAllDescendents tree
    justDirs = filter isDir allDescendents
    dirSizes = map getEntitySize justDirs
    smallDirs = filter (<=100000) dirSizes
    in sum smallDirs

------------ PART B ------------
calculateRequiredSpace :: Int -> Int
calculateRequiredSpace usedSpace = let
    totalAvailableSpace = 70000000
    requiredSpace = 30000000
    unusedSpace = totalAvailableSpace - usedSpace
    requiredAdditionalSpace = requiredSpace - unusedSpace
    in requiredAdditionalSpace

partB :: Input -> OutputB
partB input = let
    tree = allTheWayUp $ commandsToEntity (drop 1 input) :: Entity
    usedSpace = getEntitySize tree
    requiredSpace = calculateRequiredSpace usedSpace
    allDescendents = getAllDescendents tree
    justDirs = filter isDir allDescendents
    dirSizes = map getEntitySize justDirs
    bigEnoughDirs = filter (>=requiredSpace) dirSizes
    in head $ sort $ bigEnoughDirs
