module GameLogic.Logic where

import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game

setCenterPosLimited :: WorldPos -> Game -> Game
setCenterPosLimited pos game = 
    set centerPos (limitPosToWorld pos game) game
    
doSelectCellAction :: WorldPos -> Game -> Game
doSelectCellAction pos game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = set selectedPos pos game
    
doGameStep :: Game -> Game
doGameStep game
    | view leftClickDown game
     = doCellAction (view selectedPos game) game
    | otherwise
    = game

    
doCellAction :: WorldPos -> Game -> Game
doCellAction pos game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = doCellAction' pos activePlayerIndex game -- $ set selectedPos pos game
    
doCellAction' :: WorldPos -> Int -> Game -> Game
doCellAction' pos playerInd game
    | getCellPlayerIndex cell == playerInd || isFree cell 
    = increaseCell pos playerInd game
    | otherwise
    = game
    where cell = getGameCell pos game
    

increaseCell :: WorldPos -> Int -> Game -> Game
increaseCell pos playerInd game
    = increaseCellWithMax pos playerInd maxVal game
    where maxVal = min 9 $ calcSumOwnedNearest game playerInd pos

increaseCellWithMax :: WorldPos -> Int -> Int -> Game -> Game
increaseCellWithMax pos playerInd
    = overGameCell pos . increaseCellWithMax' playerInd

increaseCellWithMax' :: Int -> Int -> Cell -> Cell
increaseCellWithMax' playerInd maxVal cell
       | isFree cell
       = mkCell 1 playerInd
       | getCellPlayerIndex cell /=  playerInd
       || getCellValue cell >= maxVal
       = cell
       | otherwise
       = overCellValue (+1) cell

isPosInGame :: Game -> WorldPos -> Bool
isPosInGame = isPosInWorld . getWorld

limitPosToWorld :: WorldPos -> Game -> WorldPos
limitPosToWorld pos = limitPosToWorld' pos . getWorldSize . getWorld

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
    = foldl p 0 . getNearestOwnedCells playerInd (getWorld game)
    where p acc val = (+) acc $ (getCellValue.snd) val 
