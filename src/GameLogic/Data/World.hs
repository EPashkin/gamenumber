{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.World where

import Data.Array
import Control.Lens
import GameLogic.Data.Cell


type WorldPos = (Int, Int)
startWorldPos = (1,1)

--TODO: find way do only + -
instance (Num a,Num b) => Num (a, b) where
    (a,b) + (c,d) = (a+c, b+d)
    (a,b) * (c,d) = undefined
    (a,b) - (c,d) = (a-c, b-d)
    abs     (a,b) = undefined 
    signum  (a,b) = undefined 
    fromInteger i = undefined

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorldPos, (size, size))
    [(pos, mkCell 0 0) | x <- [1..size], y <- [1..size], let pos = (x,y)]

getWorldSize :: World -> Int
getWorldSize = fst . snd . bounds 

-- apply function to all cells
mapW :: ((WorldPos, Cell) -> a) -> World -> [a]
mapW func world = fmap func (assocs world)

-- find first pos owned by playerIndex
-- can generate error is no pos
findPlayerPos :: Int -> World -> WorldPos
findPlayerPos playerInd
    = fst . head . take 1 . filter (isOwnedBy playerInd . snd) . assocs

isPosInWorld :: World -> WorldPos -> Bool
isPosInWorld world = inRange $ bounds world

getNearestOwnedCells :: Int -> World -> WorldPos -> [(WorldPos, Cell)]
getNearestOwnedCells playerInd world
    = filter (isOwnedBy playerInd . snd) . getNearestCells world

getNearestCells :: World -> WorldPos -> [(WorldPos, Cell)]
getNearestCells world = fmap p . getNearestWorldPoses world
    where p pos' = (pos', cell)
            where Just cell = world ^? ix pos'

getNearestWorldPoses :: World -> WorldPos -> [WorldPos]
getNearestWorldPoses world pos
    = filter (isPosInWorld world) $ getNearestPoses pos  

getNearestPoses :: WorldPos -> [WorldPos]
getNearestPoses pos = fmap (\add -> pos + add) nearestCellsPosAdds    

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
