module GameLogic.Logic
    ( setCenterPosLimited
    , setCenterPos
    , doSelectCellAction
    , doGameStep
    , WorldAction
    ) where

import Debug.Trace
import Control.Lens
import Control.Category ((>>>))
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.AI.Actions
import GameLogic.AI.PossibleAction
import GameLogic.Action.Defend
import GameLogic.Action.Attack


type WorldAction = WorldPos -> GameData -> GameData

setCenterPosLimited :: WorldAction
setCenterPosLimited pos game = 
    game & centerPos .~ pos' & {- traceTest game pos' -} doSelectCellAction pos'
    where pos' = limitPosToWorld pos game

setCenterPos :: WorldAction
setCenterPos pos game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = game & centerPos .~ pos & doSelectCellAction pos

traceTest :: GameData -> WorldPos -> a -> a
traceTest game pos =
    traceShow (calcPossibleAction game 2 10 pos)
    $ traceShow (calcPossibleActions game 2)

doSelectCellAction :: WorldAction
doSelectCellAction pos game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = setSelectedPos pos activePlayerIndex game
    
doGameStep :: GameData -> GameData
doGameStep game
    | game ^. paused
    = game
    | otherwise
    = doGameStep' game
    
doGameStep' :: GameData -> GameData
doGameStep' =
    updatePlayersStats
    >>> doHumanGameStep
    >>> doAIsGameStep

doHumanGameStep :: GameData -> GameData
doHumanGameStep game
    | game ^. placementMode
    = doCellAction pos game
    | otherwise
    = game
    where Just pos = game ^? players . ix activePlayerIndex . selectedPos

updatePlayersStats :: GameData -> GameData
updatePlayersStats game =
    let maxnum = maximum $ remainDivMin : game ^.. players . each . num
        remainDiv = maxnum * remainDivMult (game ^. gameSpeed)
    in game & players. each %~ updatePlayerStats remainDiv

updatePlayerStats :: Int -> Player -> Player
updatePlayerStats remainDiv pl =
    let d = view remain pl
        d' = (+) d $ view num pl
        (free1, remain') = d' `divMod` remainDiv
        free2 = (+) free1 $ view free pl
        free3 = toRange (-100, 9999) free2
    in pl & set remain remain' . set free free3

doCellAction :: WorldAction
doCellAction pos game
    | not $ isPosInGame game pos
    = game
    | otherwise
    = doCellAction' pos activePlayerIndex game
    
doCellAction' :: WorldPos -> Int -> GameData -> GameData
doCellAction' pos playerInd game
    | cell ^. playerIndex == playerInd || isFree cell
    = increaseCell pos playerInd game
    | otherwise
    = attackCell pos playerInd game
    where Just cell = game ^? cellOfGame pos
