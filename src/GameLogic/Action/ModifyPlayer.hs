module GameLogic.Action.ModifyPlayer where

import Control.Lens
import GameLogic.Data.Game
import GameLogic.Data.Players


increasePlayerNum :: Int -> Int -> Game -> Game
increasePlayerNum inc playerInd
    = players . ix playerInd . num +~ inc

decreaseGamePlayerFree :: Int -> (Int, Game) -> Maybe Game
decreaseGamePlayerFree playerInd (cost, game)
    | cost <= curFree
    = Just $ game & players . ix playerInd . free -~ cost
    | otherwise
    = Nothing
    where Just curFree = game ^? players . ix playerInd . free
