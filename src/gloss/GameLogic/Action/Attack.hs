module GameLogic.Action.Attack
    ( attackCell
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Util
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Action.ModifyPlayer
import GameLogic.Action.Shield


attackCell :: WorldPos -> Int -> Game -> Game
attackCell pos playerInd game
    = fromMaybe game $ maybeAttackCell pos playerInd game

maybeAttackCell :: WorldPos -> Int -> Game -> Maybe Game
maybeAttackCell pos playerInd game
    | deltaStrength > 0 || (deltaStrength == 0 && sameStrength > ownerStrength)
    = conquerCell pos playerInd game
    | otherwise
    = decreaseCell pos playerInd game
    where (_, others, sameStrength, deltaStrength)
              = calcStrengthsForPlayerEx game playerInd pos
          Just ownerInd = game ^? cellOfGame pos . playerIndex
          ownerStrength = getOtherStrength ownerInd others

conquerCell :: WorldPos -> Int -> Game -> Maybe Game
conquerCell pos playerInd game
    = Just (1, game & cellOfGame pos .~ mkCell 1 playerInd)
    >>= decreaseGamePlayerFree playerInd
    >>== increasePlayerNum 1 playerInd
    >>== increasePlayerNum (-oldVal) oldPl
    where Just oldCell = game ^? cellOfGame pos
          oldPl = oldCell ^. playerIndex
          oldVal = oldCell ^. value

decreaseCell :: WorldPos -> Int -> Game -> Maybe Game
decreaseCell pos playerInd game
    = decreaseCellOrShield pos playerInd game
    >>= decreaseGamePlayerFree playerInd

decreaseCellOrShield :: WorldPos -> Int -> Game -> Maybe (Int, Game)
decreaseCellOrShield pos playerInd game
    | isShieldWorking oldPl
    = Just (2, game & players . ix oldPlInd . free -~ 1)
    | otherwise
    = Just (2, game & (cellOfGame pos %~ decreaseCell')
      . increasePlayerNum (-1) oldPlInd)
    where Just oldPlInd = game ^? cellOfGame pos . playerIndex
          Just oldPl = game ^? players . ix oldPlInd

decreaseCell' :: Cell -> Cell
decreaseCell' cell
    | val > 1
    = cell & value -~ 1
    | otherwise
    = mkCell 0 0
    where val = cell ^. value
