module GameLogic.Action.ModifyPlayer where

import Control.Lens
import GameLogic.Data.Game
import GameLogic.Data.Players


increasePlayerNum :: Int -> Int -> Game -> Game
increasePlayerNum inc playerInd
    = players . ix playerInd . num %~ (+inc)

decreaseGamePlayerFree :: Int -> (Int, Game) -> Maybe Game
decreaseGamePlayerFree playerInd (inc, game)
    | inc <= curFree
    = Just $ game & (players . ix playerInd . free) -~ inc
    | otherwise
    = Nothing
    where Just curFree = game & preview (players . ix playerInd . free)
