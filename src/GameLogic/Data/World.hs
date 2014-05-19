module GameLogic.Data.World where

import Data.Array
import GameLogic.Data.Cell


type WorldPos = (Int, Int)
startWorldPos = (1,1)

--TODO: find way do only +
instance (Num a,Num b) => Num (a, b) where
    (a,b) + (c,d) = (a+c, b+d)
    (a,b) * (c,d) = undefined
    (a,b) - (c,d) = undefined
    abs     (a,b) = undefined 
    signum  (a,b) = undefined 
    fromInteger i = undefined

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorldPos, (size, size))
    [(pos, mkCell 0 0) | x <- [1..size], y <- [1..size], let pos = (x,y)]

setWorldCell :: World -> WorldPos -> Cell -> World
setWorldCell world pos cell = world // [(pos, cell)]

getWorldCell :: World -> WorldPos -> Cell
getWorldCell world pos = world ! pos

getWorldSize :: World -> Int
getWorldSize = fst . snd . bounds 

-- apply function to all cells
mapW :: ((WorldPos, Cell) -> a) -> World -> [a]
mapW func world = map func (assocs world)

-- find first pos owned by playerIndex
-- can generate error is no pos
findPlayerPos :: World -> Int -> WorldPos
findPlayerPos world playerInd = 
    let p (_, cell) = isOwnedBy playerInd cell
        [(pos, _)] = take 1 $ filter p $ assocs world
    in pos

isPosInWorld :: World -> WorldPos -> Bool
isPosInWorld world = inRange $ bounds world

getNearestOwnedCells :: World -> WorldPos -> Int -> [(WorldPos, Cell)]
getNearestOwnedCells world pos playerInd
    = filter (isOwnedBy playerInd . snd) $ getNearestCells world pos

getNearestCells :: World -> WorldPos -> [(WorldPos, Cell)]
getNearestCells world pos = map p $ getNearestWorldPoses world pos
    where p pos' = (pos', getWorldCell world pos')

getNearestWorldPoses :: World -> WorldPos -> [WorldPos]
getNearestWorldPoses world pos
    = filter (isPosInWorld world) $ getNearestPoses pos  

getNearestPoses :: WorldPos -> [WorldPos]
getNearestPoses pos = map (\add -> pos + add) nearestCellsPosAdds    

nearestCellsPosAdds
    = [
        (-1, -1),
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0)
      ]
