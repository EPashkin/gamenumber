module GameLogic.Data.World where

import Data.Array
import GameLogic.Data.Cell
import System.Random
import Util.Shuffle


type WorldPos = (Int, Int)
startWorldPos = (1,1)

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorldPos, (size, size)) [(pos, mkCell 0 0) | x <- [1..size], y <- [1..size], let pos = (x,y)]

mkStartWorld :: Int ->Int -> StdGen -> (World, StdGen)
mkStartWorld size numPlayers gen = placeWorldPlayers (mkEmptyWorld size) numPlayers gen

setWorldCell :: World -> WorldPos -> Cell -> World
setWorldCell world pos cell = world // [(pos, cell)]

getWorldCell :: World -> WorldPos -> Cell
getWorldCell world pos = world ! pos

getWorldSize :: World -> Int
getWorldSize world = 
    let (startWorldPos, (size, _)) = bounds world
    in size

placeWorldPlayers :: World -> Int -> StdGen -> (World, StdGen)
placeWorldPlayers world numPlayers gen =
    let players = [1..numPlayers]
        positions = [calcStartPos world numPlayers pl | pl <- players]
        (positions', gen') = shuffle gen positions
        playersPosition = zipWith (\a b -> (a,b)) positions' players
        world' = foldl (\w (pos, pl) -> setWorldCell w pos (mkCell 1 pl)) world playersPosition :: World
    in (world', gen')

calcStartPos :: World -> Int -> Int -> WorldPos
calcStartPos world numPlayers num = 
    let list = playersStartPosXList (getWorldSize world) numPlayers 
        cols = playersStartPosCols numPlayers
        xInd = ((num-1) `mod` cols)
        yInd = ((num-1) `div` cols)
    in (list !! xInd, list !! yInd)

-- return number of columns of players start positions by number of players
playersStartPosCols :: Int -> Int
playersStartPosCols 4 = 2
playersStartPosCols 16 = 4

-- return list x-coords start positions
playersStartPosXList :: Int -> Int -> [Int]
playersStartPosXList size numPlayers =
    let cols = playersStartPosCols numPlayers
        dist = size `div` cols
    in map (\i -> (i-1)*dist + (dist `div` 2) + 1 ) [1..cols]

-- apply function to all cells
mapW :: ((WorldPos, Cell) -> a) -> World -> [a]
mapW func world = map func (assocs world)

-- find first pos owned by playerIndex
-- can generate error is no pos
findPlayerPos :: World -> Int -> WorldPos
findPlayerPos world playerInd = 
    let [(pos, _)] = take 1 $ filter (\ (_, cell) -> playerIndex cell == playerInd) $ assocs world
    in pos
