module GameLogic.Logic
    ( setCenterPosLimited
    , setCenterPos
    , doSelectCellAction
    , doGameStep
    , WorldAction
    ) where

import Debug.Trace
import Control.Lens hiding ((<|), (|>))
import Control.Conditional
import GameLogic.Data.Facade
import GameLogic.Util
import GameLogic.AI.Actions
import GameLogic.AI.PossibleAction
import GameLogic.Action.Defend
import GameLogic.Action.Attack


setCenterPosLimited :: WorldAction
setCenterPosLimited pos = do
    pos' <- gets $ limitPosToWorld pos
    centerPos .= pos' >> {- _traceTest pos' >> -} doSelectCellAction pos'

setCenterPos :: WorldAction
setCenterPos pos = whenM (isPosInGame pos)
    $ (centerPos .= pos) >> doSelectCellAction pos

_traceTest :: WorldPos -> GameState ()
_traceTest pos = do
    game <- get
    traceShow (calcPossibleAction game 2 10 pos)
      . traceShow (calcPossibleActions game 2)
      $ return ()

doSelectCellAction :: WorldAction
doSelectCellAction pos = whenM (isPosInGame pos)
    $ setSelectedPos pos activePlayerIndex

doGameStep :: GameState ()
doGameStep = unlessM (use paused)
    $ updatePlayersStats
    >> doHumanGameStep
    >> doAIsGameStep

doHumanGameStep :: GameState ()
doHumanGameStep = do
    pos <- use $ playerOfGame activePlayerIndex . selectedPos
    whenM (use placementMode) $ doCellAction pos

updatePlayersStats :: GameState ()
updatePlayersStats = do
    remainDiv <- gets calcRemainDiv
    players . each %= updatePlayerStats remainDiv

calcRemainDiv :: GameData -> Int
calcRemainDiv game = maxnum * remainDivMult (game ^. gameSpeed)
    where getPlayersNums = toListOf $ players . each . num
          maxnum = maximum $ remainDivMin : getPlayersNums game

updatePlayerStats :: Int -> Player -> Player
updatePlayerStats remainDiv pl =
    let d = view remain pl
        d' = (+) d $ view num pl
        (free1, remain') = d' `divMod` remainDiv
        free2 = (+) free1 $ view free pl
        free3 = toRange (-100, 9999) free2
    in pl & set remain remain' . set free free3

doCellAction :: WorldAction
doCellAction pos = whenM (isPosInGame pos)
    $ doCellAction' activePlayerIndex pos

doCellAction' :: Int -> WorldAction
doCellAction' playerInd pos = do
    cell <- use $ cellOfGame pos
    increaseCell pos playerInd
        <| cell ^. playerIndex == playerInd || isFree cell |>
        attackCell pos playerInd
