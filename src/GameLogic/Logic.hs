module GameLogic.Logic where

import Control.Lens
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Util
import GameLogic.Action.Defend

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
