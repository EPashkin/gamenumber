module GameLogic.Logic where

import GameLogic.Data.Cell
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
    = doCellAction' game' pos cell activePlayerIndex 
    where
        game' = game { gSelectedPos = pos }
        cell = getGameCell game pos
    
doCellAction' :: Game -> WorldPos -> Cell -> Int -> Game
doCellAction' game pos cell playerInd
    | getCellPlayerIndex cell == playerInd
    = increaseCell game pos cell 
    | isFree cell
    = increaseCell game pos $ mkCell 0 playerInd
    | otherwise
    = game
    
increaseCell :: Game -> WorldPos -> Cell -> Game
increaseCell game pos cell
    | getCellValue cell >= 9
    = game
    | otherwise
    = setGameCell game pos cell'
    where cell' = changeCellValue cell $ getCellValue cell + 1

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
