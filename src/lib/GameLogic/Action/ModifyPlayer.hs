module GameLogic.Action.ModifyPlayer where

import Control.Lens
import GameLogic.Data.Facade


decreaseGamePlayerFree :: Int -> Int -> MaybeGameState ()
decreaseGamePlayerFree playerInd cost = do
    curFree <- use $ playerOfGame playerInd . free
    when (cost > curFree) $ fail "Not enough free"
    playerOfGame playerInd . free -= cost

helpPlayer :: Int -> GameState ()
helpPlayer playerInd = fromMaybeState $ decreaseGamePlayerFree playerInd (-10)
