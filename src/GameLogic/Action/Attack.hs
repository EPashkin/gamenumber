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
    = fromMaybe game $ attackCellWithStrength pos playerInd deltaStrength game
    where (same, others) = calcStrengthsForPlayer game playerInd pos
          sameStrength = same ^. value
          deltaStrength = getDeltaOtherStrength sameStrength others

attackCellWithStrength :: WorldPos -> Int -> Int -> Game -> Maybe Game
attackCellWithStrength pos playerInd deltaStrength game
    | deltaStrength > 0
    = conquerCell pos playerInd game
    | otherwise
    = Just game

conquerCell :: WorldPos -> Int -> Game -> Maybe Game
conquerCell pos playerInd game
    = Just (1, game & cellOfGame pos .~ mkCell 1 playerInd)
    >>= decreaseGamePlayerFree playerInd
    >>= Just . increasePlayerNum 1 playerInd
    >>= Just . increasePlayerNum (-oldVal) oldPl
    where Just oldCell = game ^? cellOfGame pos
          oldPl = oldCell ^. playerIndex
          oldVal = oldCell ^. value
