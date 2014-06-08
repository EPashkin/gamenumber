module GameLogic.Action.Attack
    ( attackCell
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Util
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Action.ModifyPlayer


attackCell :: WorldPos -> Int -> Game -> Game
attackCell pos playerInd game
    = fromMaybe game $ maybeAttackCell pos playerInd game

maybeAttackCell :: WorldPos -> Int -> Game -> Maybe Game
maybeAttackCell pos playerInd game
    | deltaStrength > 0
    = conquerCell pos playerInd game
    | otherwise
    = decreaseCell pos playerInd game
    where (_, _ , _, deltaStrength)
              = calcStrengthsForPlayerEx game playerInd pos

conquerCell :: WorldPos -> Int -> Game -> Maybe Game
conquerCell pos playerInd game
    = Just (1, game & cellOfGame pos .~ mkCell 1 playerInd)
    >>= decreaseGamePlayerFree playerInd
    >>= Just . increasePlayerNum 1 playerInd
    >>= Just . increasePlayerNum (-oldVal) oldPl
    where Just oldCell = game ^? cellOfGame pos
          oldPl = oldCell ^. playerIndex
          oldVal = oldCell ^. value

decreaseCell :: WorldPos -> Int -> Game -> Maybe Game
decreaseCell pos playerInd game
    = Just (2, game & cellOfGame pos %~ decreaseCell')
    >>= decreaseGamePlayerFree playerInd
    >>= Just . increasePlayerNum (-1) oldPl
    where Just oldPl = game ^? (cellOfGame pos . playerIndex)

decreaseCell' :: Cell -> Cell
decreaseCell' cell
    | val > 1
    = cell & value -~ 1
    | otherwise
    = mkCell 0 0
    where val = cell ^. value
