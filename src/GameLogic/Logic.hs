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
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = doCellAction' game' pos cell activePlayerIndex 
      where
        game' = game { gSelectedPos = pos }
        cell = getGameCell game pos
    
doCellAction' :: Game -> WorldPos -> Cell -> Int -> Game
doCellAction' game pos cell playerInd
    | getCellPlayerIndex cell == playerInd
    = increaseCell game pos cell playerInd
    | isFree cell
    = increaseCell game pos (mkCell 0 playerInd) playerInd 
    | otherwise
    = game

increaseCell :: Game -> WorldPos -> Cell -> Int -> Game
increaseCell game pos cell playerInd
    = increaseCellWithMax game pos cell maxVal
    where maxVal = min 9 $ calcSumOwnedNearest game pos playerInd

increaseCellWithMax :: Game -> WorldPos -> Cell -> Int -> Game
increaseCellWithMax game pos cell maxVal
    | getCellValue cell >= maxVal
    = game
    | otherwise
    = setGameCell game pos cell'
    where cell' = changeCellValue cell $ getCellValue cell + 1

isPosInGame :: Game -> WorldPos -> Bool
isPosInGame game = isPosInWorld $ getWorld game

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

calcSumOwnedNearest :: Game ->WorldPos -> Int -> Int
calcSumOwnedNearest game pos playerInd
    = foldl p 0 $ getNearestOwnedCells (getWorld game) pos playerInd
    where p acc val = (+) acc $ (getCellValue.snd) val 
