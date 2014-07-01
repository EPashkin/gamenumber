module GameLogic.Action.ModifyPlayer where

import Control.Lens
import GameLogic.Data.Game
import GameLogic.Data.Players


increasePlayerNum :: Int -> Int -> GameData -> GameData
increasePlayerNum inc playerInd
    = playerOfGame playerInd . num +~ inc

decreaseGamePlayerFree :: Int -> (Int, GameData) -> Maybe GameData
decreaseGamePlayerFree playerInd (cost, game)
    | cost <= curFree
    = Just $ game & playerOfGame playerInd . free -~ cost
    | otherwise
    = Nothing
    where curFree = game ^. playerOfGame playerInd . free
