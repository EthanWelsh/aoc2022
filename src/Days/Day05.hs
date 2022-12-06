module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl', transpose, head )
import Data.Maybe ( catMaybes )
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser,
      count,
      many1,
      sepBy,
      decimal,
      space,
      anyChar,
      char,
      endOfLine,
      string )
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Control.Monad (void)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser Input
inputParser = (,) <$> stacks <*> instructions
  where
    -- Parses our list of stacks
    -- Takes the stack description row by row, then transposes it and removes the empty spaces
    stacks =
      (Vec.fromList . fmap catMaybes . transpose <$> (row `sepBy` endOfLine))
        <* many1 (void space <|> void decimal) -- Skipping the numbers at the bottom of the stacks, and the newlines

    -- Parses a row of the stack description
    row =
      ( (count 3 (char ' ') $> Nothing)
          <|> (char '[' *> (Just <$> anyChar) <* char ']')
      )
        `sepBy` char ' '
    instructions = instruction `sepBy` endOfLine
    instruction = do
      string "move "
      quant <- decimal
      string " from "
      from <- decimal
      string " to "
      to <- decimal
      return (quant, from - 1, to - 1)

------------ TYPES ------------

type Stack = [Char]

type Instruction = (Int, Int, Int)

type Input = (Vector Stack, [Instruction])

type OutputA = String

type OutputB = String

------------ PART A ------------

push :: a -> [a] -> [a]
push x xs = x:xs

pop :: [a] -> (a, [a])
pop (x:xs) = (x, xs)

popThenPush :: Int -> Int -> Vector Stack -> Vector Stack
popThenPush fromIndex toIndex stacks = let
    from = stacks Vec.! fromIndex
    to = stacks Vec.! toIndex
    (x, popped) = pop from
    pushed = push x to
    in stacks Vec.// [(fromIndex, popped), (toIndex, pushed)]

getNormalizedInstructions :: [Instruction] -> [(Int, Int)]
getNormalizedInstructions ins = let
    norm (quantity, from, to) = replicate quantity (from, to)
    in concatMap norm ins

run :: [(Int, Int)] -> Vector Stack -> Vector Stack
run ins stacks = foldl (\stacks' (fromIndex, toIndex) -> popThenPush fromIndex toIndex stacks') stacks ins

heads' :: Vector String -> Vector Char
heads' v = Vec.map Data.List.head v

partA :: Input -> OutputA
partA (stacks, ins) = let 
    normalized = getNormalizedInstructions ins :: [(Int, Int)]
    result     = run normalized stacks         :: Vector Stack
    heads      = Vec.map head result           :: Vector Char
    in Vec.toList heads

------------ PART B ------------

pushMultiple :: [Char] -> [Char] -> [Char]
pushMultiple pushes stack  = pushes ++ stack

popMultiple :: Int -> [Char] -> ([Char], [Char])
popMultiple count stack = (take count stack, drop count stack)

popThenPushB :: Int -> Int -> Int -> Vector Stack -> Vector Stack
popThenPushB quantity fromIndex toIndex stacks = let
    from = stacks Vec.! fromIndex
    to = stacks Vec.! toIndex
    (xs, popped) = popMultiple quantity from
    pushed = pushMultiple xs to
    in stacks Vec.// [(fromIndex, popped), (toIndex, pushed)]

runB :: [Instruction] -> Vector Stack -> Vector Stack
runB ins stacks = foldl (\stacks' (quantity, fromIndex, toIndex) -> 
    popThenPushB quantity fromIndex toIndex stacks') stacks ins

partB :: Input -> OutputB
partB (stacks, ins) = let 
    result = runB ins stacks     :: Vector Stack
    heads  = Vec.map head result :: Vector Char
    in Vec.toList heads
