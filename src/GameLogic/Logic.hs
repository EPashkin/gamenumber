module GameLogic.Logic where

import Control.Lens
import Control.Category ( (>>>) )
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
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
doGameStep =
    updatePlayersStats
    >>> doHumanGameStep

doHumanGameStep :: Game -> Game
doHumanGameStep game
    | view placementMode game
     = doCellAction (view selectedPos game) game
    | otherwise
    = game

updatePlayersStats :: Game -> Game
updatePlayersStats game =
    let maxnum = maximum $ remainDivMin : game ^.. players . each . num
        remainDiv = maxnum * remainDivMult
    in game & players. each %~ updatePlayerStats remainDiv

updatePlayerStats :: Int -> Player -> Player
updatePlayerStats remainDiv pl =
    let d = view remain pl
        d' = (+) d $ view num pl
        (free1, remain') = d' `divMod` remainDiv
        free2 = (+) free1 $ view free pl
        free3 = toRange (-100, 9999) free2
    in pl & set remain remain' . set free free3

toRange :: (Int, Int) -> Int -> Int
toRange (minval, maxval) = max minval . min maxval

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
