{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.World where

import Data.Array
import Control.Lens
import GameLogic.Data.Cell


type WorldPos = (Int, Int)

startWorldPos :: WorldPos
startWorldPos = (1,1)

--TODO: find way do only + -
instance (Num a,Num b) => Num (a, b) where
    (a,b) + (c,d) = (a+c, b+d)
    (_a,_b) * (_c,_d) = undefined
    (a,b) - (c,d) = (a-c, b-d)
    abs     (_a,_b) = undefined
    signum  (_a,_b) = undefined
    fromInteger _i = undefined

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorldPos, (size, size))
    [(pos, mkCell 0 0) | x <- [1..size], y <- [1..size], let pos = (x,y)]

setWorldCell :: WorldPos -> World -> Cell -> World
setWorldCell pos world cell = world // [(pos, cell)]

getWorldCell :: WorldPos -> World -> Cell
getWorldCell pos world = world ! pos

toCell :: WorldPos -> Lens' World Cell
toCell pos = lens (getWorldCell pos) (setWorldCell pos)

getWorldSize :: World -> Int
getWorldSize = fst . snd . bounds 

-- apply function to all cells
{-# INLINE mapW #-}
mapW :: ((WorldPos, Cell) -> a) -> World -> [a]
mapW func world = fmap func (assocs world)

{-# INLINABLE mapWR #-}
mapWR :: ((WorldPos, Cell) -> a) -> (WorldPos, WorldPos) -> World -> [a]
mapWR func range' world = fmap func . filter (inRange range' . fst) $ assocs world

-- find first pos owned by playerIndex
-- can generate error is no pos
{-# INLINABLE findPlayerPos #-}
findPlayerPos :: Int -> World -> WorldPos
findPlayerPos playerInd
    = fst . head . take 1 . filter (isOwnedBy playerInd . snd) . assocs

{-# INLINE isPosInWorld #-}
isPosInWorld :: World -> WorldPos -> Bool
isPosInWorld = inRange . bounds

getNearestOwnedCells :: Int -> World -> WorldPos -> [(WorldPos, Cell)]
getNearestOwnedCells playerInd world
    = filter (isOwnedBy playerInd . snd) . getNearestCells world

getNearestCells :: World -> WorldPos -> [(WorldPos, Cell)]
getNearestCells world = fmap p . getNearestWorldPoses world
    where p pos' = (pos', cell)
            where cell = world ^. toCell pos'

getNearestWorldPoses :: World -> WorldPos -> [WorldPos]
getNearestWorldPoses world = filter (isPosInWorld world) . getNearestPoses  

getNearestPoses :: WorldPos -> [WorldPos]
getNearestPoses pos = fmap (\add -> pos + add) nearestCellsPosAdds    

nearestCellsPosAdds :: [WorldPos]
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
