module GameLogic.Util where

import Data.List
import Data.Function (on)
import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players


setSelectedPos :: WorldPos -> Int -> Game -> Game
setSelectedPos pos playerInd game
    | not $ isPosInGame game pos
    = game 
    | otherwise
    = game & players . ix playerInd . selectedPos .~ pos
    
isPosInGame :: Game -> WorldPos -> Bool
isPosInGame = isPosInWorld . view world

limitPosToWorld :: WorldPos -> Game -> WorldPos
limitPosToWorld pos = limitPosToWorld' pos . getWorldSize . view world

limitPosToWorld' (x, y) max
    | x < 1
    = limitPosToWorld' (1, y) max
    | x > max
    = limitPosToWorld' (max, y) max
    | y < 1
    = limitPosToWorld' (x, 1) max
    | y > max
    = limitPosToWorld' (x, max) max
    | otherwise
    = (x,y)

calcSumOwnedNearest :: Game -> Int -> WorldPos -> Int
calcSumOwnedNearest game playerInd
    = foldl p 0 . getNearestOwnedCells playerInd (view world game)
    where p acc val = (+) acc $ (view value . snd) val

calcSumAllNearest :: Game -> WorldPos -> [Cell]
calcSumAllNearest game
    = foldl p [] . getNearestCells (view world game)
    where p acc = updateCellList acc . snd

updateCellList :: [Cell] -> Cell -> [Cell]
updateCellList cells cell
    | isFree cell
    = cells
    | null cells
    = [cell]
    | otherwise
    = same' : others
    where plInd = cell ^. playerIndex
          (same, others) = partition (isOwnedBy plInd) cells
          same' = case same of
                  [] -> cell
                  [cell'] -> cell' & value %~ (+ (cell ^. value))

type StrengthsEx = (Cell, [Cell], Int, Int)

calcStrengthsForPlayerEx :: Game -> Int -> WorldPos -> StrengthsEx
calcStrengthsForPlayerEx game playerInd pos
    = (same, others, sameStrength, deltaStrength)
    where (same, others) = calcStrengthsForPlayer game playerInd pos
          sameStrength = same ^. value
          deltaStrength = getDeltaOtherStrength sameStrength others

calcStrengthsForPlayer :: Game -> Int -> WorldPos -> (Cell, [Cell])
calcStrengthsForPlayer game playerInd pos
    = (same', sortBy p others)
    where cells = calcSumAllNearest game pos
          (same, others) = partition (isOwnedBy playerInd) cells
          same' = case same of
                  [] -> mkCell 0 playerInd
                  [cell] -> cell
          p = flip compare `on` view value

getDeltaOtherStrength :: Int -> [Cell] -> Int
getDeltaOtherStrength sameStrength cells
    = sameStrength - getOthersStrength cells ^. value

getOthersStrength :: [Cell] -> Cell
getOthersStrength [] = mkCell 0 0
getOthersStrength (c:cs) = c

getOtherStrength :: Int -> [Cell] -> Int
getOtherStrength _ [] = 0
getOtherStrength playerInd (c:cs)
    | isOwnedBy playerInd c
    = c ^. value
    | otherwise
    = getOtherStrength playerInd cs

toRange :: (Int, Int) -> Int -> Int
toRange (minval, maxval) = max minval . min maxval
