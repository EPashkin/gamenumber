module GameLogic.Util where

import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game


isPosInGame :: Game -> WorldPos -> Bool
isPosInGame = isPosInWorld . view world

limitPosToWorld :: WorldPos -> Game -> WorldPos
limitPosToWorld pos = limitPosToWorld' pos . getWorldSize . view world

limitPosToWorld' (x, y) max
    | x < 1
    = limitPosToWorld' (1, y) max
    | x > max
    = limitPosToWorld' (max, y) max
    | y < 1
    = limitPosToWorld' (x, 1) max
    | y > max
    = limitPosToWorld' (x, max) max
    | otherwise
    = (x,y)

calcSumOwnedNearest :: Game -> Int -> WorldPos -> Int
calcSumOwnedNearest game playerInd
    = foldl p 0 . getNearestOwnedCells playerInd (view world game)
    where p acc val = (+) acc $ (view value . snd) val
