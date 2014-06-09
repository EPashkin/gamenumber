module GameLogic.Action.Defend
    ( increaseCell
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Util
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Action.ModifyPlayer


increaseCell :: WorldPos -> Int -> Game -> Game
increaseCell pos playerInd game
    = fromMaybe game $ increaseCellWithMax pos playerInd maxVal game
    where maxVal = min 9 $ calcSumOwnedNearest game playerInd pos

increaseCellWithMax :: WorldPos -> Int -> Int -> Game -> Maybe Game
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

updateGameCellWithCost :: WorldPos-> Int -> Game -> (Int -> Cell -> Maybe (Int, Cell))
      -> Maybe (Int, Game) 
updateGameCellWithCost pos playerInd game action
    = game ^? cellOfGame pos >>= action playerInd
      >>= (\(val, cell) -> Just . (,) val $ game & cellOfGame pos .~ cell)
