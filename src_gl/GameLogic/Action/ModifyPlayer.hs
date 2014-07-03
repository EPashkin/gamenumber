module GameLogic.Action.ModifyPlayer where

import Control.Lens
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.GameState


increasePlayerNum :: Int -> Int -> GameData -> GameData
increasePlayerNum inc playerInd
    = playerOfGame playerInd . num +~ inc

--TODO: remove when unused
decreaseGamePlayerFree' :: Int -> (Int, GameData) -> Maybe GameData
decreaseGamePlayerFree' playerInd (cost, game)
    | cost <= curFree
    = Just $ game & playerOfGame playerInd . free -~ cost
    | otherwise
    = Nothing
    where curFree = game ^. playerOfGame playerInd . free

decreaseGamePlayerFree :: Int -> Int -> MaybeGameState ()
decreaseGamePlayerFree playerInd cost = do
    curFree <- use $ playerOfGame playerInd . free
    when (cost > curFree) $ fail "Not enough free"
    playerOfGame playerInd . free -= cost

helpPlayer :: Int -> GameState ()
helpPlayer playerInd = fromMaybeState $ decreaseGamePlayerFree playerInd (-10)
