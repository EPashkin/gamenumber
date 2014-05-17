module GameLogic.Data.World where

import Data.Array
import GameLogic.Data.Cell


type WorldPos = (Int, Int)
startWorldPos = (1,1)

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorldPos, (size, size)) [(pos, mkCell 0 0) | x <- [1..size], y <- [1..size], let pos = (x,y)]

setWorldCell :: World -> WorldPos -> Cell -> World
setWorldCell world pos cell = world // [(pos, cell)]

getWorldCell :: World -> WorldPos -> Cell
getWorldCell world pos = world ! pos

getWorldSize :: World -> Int
getWorldSize world = 
    let (startWorldPos, (size, _)) = bounds world
    in size

-- apply function to all cells
mapW :: ((WorldPos, Cell) -> a) -> World -> [a]
mapW func world = map func (assocs world)

-- find first pos owned by playerIndex
-- can generate error is no pos
findPlayerPos :: World -> Int -> WorldPos
findPlayerPos world playerInd = 
    let [(pos, _)] = take 1 $ filter (\ (_, cell) -> playerIndex cell == playerInd) $ assocs world
    in pos
