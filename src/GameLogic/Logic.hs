module GameLogic.Logic where

import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game

setCenterPosLimited :: WorldPos -> Game -> Game
setCenterPosLimited pos game = 
    game & centerPos .~ limitPosToWorld pos game
    
doSelectCellAction :: WorldPos -> Game -> Game
doSelectCellAction pos game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = set selectedPos pos game
    
doGameStep :: Game -> Game
doGameStep game
    | view placementMode game
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
    | cell ^. playerIndex == playerInd || isFree cell 
    = increaseCell pos playerInd game
    | otherwise
    = game
    where cell = game ^. cellOfGame pos

increaseCell :: WorldPos -> Int -> Game -> Game
increaseCell pos playerInd game
    = increaseCellWithMax pos playerInd maxVal game
    where maxVal = min 9 $ calcSumOwnedNearest game playerInd pos

increaseCellWithMax :: WorldPos -> Int -> Int -> Game -> Game
increaseCellWithMax pos playerInd maxVal
    = cellOfGame pos %~ increaseCellWithMax' playerInd maxVal

increaseCellWithMax' :: Int -> Int -> Cell -> Cell
increaseCellWithMax' playerInd maxVal cell
       | cell ^. playerIndex == playerInd
       && cell ^. value < maxVal
       = cell & value %~ (+1)  
       | isFree cell
       && maxVal >= 1
       = mkCell 1 playerInd
       | otherwise
       = cell

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
