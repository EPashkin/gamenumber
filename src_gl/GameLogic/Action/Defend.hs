module GameLogic.Action.Defend
    ( increaseCell
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Util
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Action.ModifyPlayer


increaseCell :: WorldPos -> Int -> GameData -> GameData
increaseCell pos playerInd game
    = fromMaybe game $ increaseCellWithMax pos playerInd maxVal game
    where maxVal = min maxCellValue $ calcSumOwnedNearest game playerInd pos

increaseCellWithMax :: WorldPos -> Int -> Int -> GameData -> Maybe GameData
increaseCellWithMax pos playerInd maxVal game
      = updateGameCellWithCost pos playerInd game (increaseCellWithMax' maxVal)
      >>= decreaseGamePlayerFree playerInd
      >>== increasePlayerNum 1 playerInd

increaseCellWithMax' :: Int -> Int -> Cell -> Maybe (Int, Cell)
increaseCellWithMax' maxVal playerInd cell
       | cell ^. playerIndex == playerInd
       && cell ^. value < maxVal
       = Just . (,) 1 $ cell & value +~ 1
       | isFree cell
       && maxVal >= 1
       = Just . (,) 1 $ mkCell 1 playerInd
       | otherwise
       = Nothing

updateGameCellWithCost :: WorldPos-> Int -> GameData -> (Int -> Cell -> Maybe (Int, Cell))
      -> Maybe (Int, GameData)
updateGameCellWithCost pos playerInd game action
    = game ^? cellOfGame pos >>= action playerInd
      >>= (\(val, cell) -> Just . (,) val $ game & cellOfGame pos .~ cell)
