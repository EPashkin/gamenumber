module GameLogic.Action.Defend
    ( increaseCell
    ) where

import Control.Lens
import GameLogic.Util
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players


increaseCell :: WorldPos -> Int -> Game -> Game
increaseCell pos playerInd game
    = increaseCellWithMax pos playerInd maxVal game
    where maxVal = min 9 $ calcSumOwnedNearest game playerInd pos

increaseCellWithMax :: WorldPos -> Int -> Int -> Game -> Game
increaseCellWithMax pos playerInd maxVal game
    = case mcell of
           Just cell -> increasePlayerNum 1 playerInd game'
                        where game' = game & cellOfGame pos .~ cell
           Nothing   -> game
    where mcell = game ^? cellOfGame pos >>= increaseCellWithMax' playerInd maxVal

increaseCellWithMax' :: Int -> Int -> Cell -> Maybe Cell
increaseCellWithMax' playerInd maxVal cell
       | cell ^. playerIndex == playerInd
       && cell ^. value < maxVal
       = Just $ cell & value %~ (+1)
       | isFree cell
       && maxVal >= 1
       = Just $ mkCell 1 playerInd
       | otherwise
       = Nothing

increasePlayerNum :: Int -> Int -> Game -> Game
increasePlayerNum inc playerInd
    = players . ix playerInd . num %~ (+inc)
