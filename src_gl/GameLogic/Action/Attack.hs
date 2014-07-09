module GameLogic.Action.Attack
    ( attackCell
    ) where

import Control.Lens hiding ((<|), (|>))
import Control.Conditional
import GameLogic.Util
import GameLogic.Data.Facade
import GameLogic.Action.ModifyPlayer
import GameLogic.Action.Shield


attackCell :: WorldPos -> Int -> GameState ()
attackCell pos playerInd
    = fromMaybeState $ maybeAttackCell pos playerInd

maybeAttackCell :: WorldPos -> Int -> MaybeGameState ()
maybeAttackCell pos playerInd = do
    (_, others, sameStrength, deltaStrength)
        <- gets $ calcStrengthsForPlayer playerInd pos
    ownerInd <- use $ cellOfGame pos . playerIndex
    let ownerStrength = getOtherStrength ownerInd others
    conquerCell pos playerInd
        <| deltaStrength > 0 
        || (deltaStrength == 0 && sameStrength > ownerStrength) |>
        decreaseCell pos playerInd

conquerCell :: WorldPos -> Int -> MaybeGameState ()
conquerCell pos playerInd = do
    oldCell <- use $ cellOfGame pos
    let oldPl = oldCell ^. playerIndex
    let oldVal = oldCell ^. value
    cellOfGame pos .= mkCell 1 playerInd
    decreaseGamePlayerFree playerInd 1
    playerOfGame playerInd . num += 1
    playerOfGame oldPl . num += (-oldVal)

decreaseCell :: WorldPos -> Int -> MaybeGameState ()
decreaseCell pos playerInd = do
    cost <- decreaseCellOrShield pos
    decreaseGamePlayerFree playerInd cost

decreaseCellOrShield :: WorldPos -> MaybeGameState Int
decreaseCellOrShield pos = do
    oldPlInd <- use $ cellOfGame pos . playerIndex
    oldPl <- use $ playerOfGame oldPlInd
    playerOfGame oldPlInd . free -= 1
        >> return 2
        <| isShieldWorking oldPl |>
        do
            cellOfGame pos %= decreaseCell'
            playerOfGame oldPlInd . num -= 1
            return 2

decreaseCell' :: Cell -> Cell
decreaseCell' cell
    | val > 1
    = cell & value -~ 1
    | otherwise
    = mkCell 0 0
    where val = cell ^. value
