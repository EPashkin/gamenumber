module GameLogic.Action.Defend
    ( increaseCell
    ) where

import Control.Lens
import Control.Conditional
import GameLogic.Util
import GameLogic.GameState
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Action.ModifyPlayer


increaseCell :: WorldPos -> Int -> GameState ()
increaseCell pos playerInd = do
    strength <- gets $ calcSumOwnedNearest playerInd pos
    let maxVal = min maxCellValue strength
    fromMaybeState $ increaseCellWithMax pos playerInd maxVal

increaseCellWithMax :: WorldPos -> Int -> Int -> MaybeGameState ()
increaseCellWithMax pos playerInd maxVal = do
      cost <- zoom (cellOfGame pos) $ increaseCellWithMax' maxVal playerInd
      decreaseGamePlayerFree playerInd cost
      playerOfGame playerInd . num += 1

increaseCellWithMax' :: Int -> Int -> StateT Cell Maybe Int
increaseCellWithMax' maxVal playerInd
   = condPlusM
       [(gets (isOwnedBy playerInd) <&&> uses value (< maxVal)
           , value += 1 >> return 1)
       ,(gets isFree <&&> return (maxVal >= 1)
           , put (mkCell 1 playerInd) >> return 1)
       ]
