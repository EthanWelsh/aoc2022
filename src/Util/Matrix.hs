module Util.Matrix where

import qualified Data.HashSet as HashSet

type Point = (Int, Int)
type Matrix a = [[a]]
data CardinalDirection = North | South | East | West deriving (Eq, Show)

getHeight :: Matrix a -> Int
getHeight maze = length maze

getWidth :: Matrix a -> Int
getWidth maze = length (maze !! 0)

getAllPoints :: Matrix a -> [Point]
getAllPoints maze = let
    height = (getHeight maze) - 1
    width = (getWidth maze) - 1
    in  [(r, c) | r <- [0..height], c <- [0..width]]

get :: Matrix a -> Point -> a
get maze (r, c) = (maze !! r) !! c

pointInBounds :: Matrix a -> Point -> Bool
pointInBounds maze (r, c) = let
    height = getHeight maze
    width = getWidth maze
    inBetween test start end = if test >= start && test < end then True else False 
    in inBetween r 0 height && inBetween c 0 width

cardinalPoints :: Matrix a -> Point -> HashSet.HashSet Point
cardinalPoints maze (r, c) = let
    points = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
    inBounds = filter (pointInBounds maze) points
    in HashSet.fromList inBounds

movePoint :: Point -> CardinalDirection -> Point
movePoint (r, c) North = (r - 1, c)
movePoint (r, c) South = (r + 1, c)
movePoint (r, c) East = (r, c + 1)
movePoint (r, c) West = (r, c - 1)

getPath :: Point -> [CardinalDirection] -> [Point]
getPath _ [] = []
getPath p (d:ds) = let
    newP = movePoint p d
    in newP : getPath newP ds