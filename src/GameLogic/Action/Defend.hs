module GameLogic.Action.Defend
    ( increaseCell
    ) where

import Control.Lens
import GameLogic.Util
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game


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
