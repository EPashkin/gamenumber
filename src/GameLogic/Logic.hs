module GameLogic.Logic where

import GameLogic.Data.World
import GameLogic.Data.Game

setCenterPos :: Game -> WorldPos -> Game
setCenterPos game pos = 
    let pos' = limitPosToWorld game pos
    in game { gCenterPos = pos' }
    
doCellAction :: Game -> WorldPos -> Game
doCellAction game pos
    | not $ isPosInWorld game pos
    = game
    | otherwise
    = game { gSelectedPos = pos }

isPosInWorld :: Game -> WorldPos -> Bool
isPosInWorld game (x, y)
    | x >= 1
    , x <= getWorldSize (getWorld game)
    , y >= 1
    , y <= getWorldSize (getWorld game)
    = True
    | otherwise
    = False

limitPosToWorld :: Game -> WorldPos -> WorldPos
limitPosToWorld game pos = limitPosToWorld' pos $ getWorldSize $ getWorld game

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
