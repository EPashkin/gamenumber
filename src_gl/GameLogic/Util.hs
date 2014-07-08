module GameLogic.Util
    ( module GameLogic.Util
    , module M
    ) where

import Data.List
import Data.Monoid as M ((<>))
import Data.Function (on)
import Control.Conditional
import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.GameState


setSelectedPos :: WorldPos -> Int -> GameState()
setSelectedPos pos playerInd = whenM (isPosInGame pos)
    $ playerOfGame playerInd . selectedPos .= pos

isPosInGame :: WorldPos -> GameState Bool
isPosInGame pos = uses world (`isPosInWorld` pos)

limitPosToWorld :: WorldPos -> GameData -> WorldPos
limitPosToWorld pos = limitPosToWorld' pos . getWorldSize . view world

limitPosToWorld' :: WorldPos -> Int -> WorldPos
limitPosToWorld' (x, y) size
    | x < 1
    = limitPosToWorld' (1, y) size
    | x > size
    = limitPosToWorld' (size, y) size
    | y < 1
    = limitPosToWorld' (x, 1) size
    | y > size
    = limitPosToWorld' (x, size) size
    | otherwise
    = (x,y)

calcSumOwnedNearest :: Int -> WorldPos -> GameData -> Int
calcSumOwnedNearest playerInd pos
    = foldl p 0 . getNearestOwnedCells playerInd pos . view world
    where p acc val = (+) acc $ (view value . snd) val

calcSumAllNearest :: WorldPos -> GameData -> [Cell]
calcSumAllNearest pos
    = foldl p [] . getNearestCells pos . view world
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
                  _ -> error "Wrong list in GameLogic.Util.updateCellList"

type Strengths = (Cell, [Cell], Int, Int)

calcStrengthsForPlayer :: Int -> WorldPos -> GameData -> Strengths
calcStrengthsForPlayer playerInd pos game
    = (same, others, sameStrength, deltaStrength)
    where (same, others) = calcStrengthsForPlayer' playerInd pos game
          sameStrength = same ^. value
          deltaStrength = getDeltaOtherStrength sameStrength others

calcStrengthsForPlayer' :: Int -> WorldPos -> GameData -> (Cell, [Cell])
calcStrengthsForPlayer' playerInd pos game
    = (same', sortBy p others)
    where cells = calcSumAllNearest pos game
          (same, others) = partition (isOwnedBy playerInd) cells
          same' = case same of
                  [] -> mkCell 0 playerInd
                  [cell] -> cell
                  _ -> error "Wrong list in GameLogic.Util.calcStrengthsForPlayer"
          p = flip compare `on` view value

getDeltaOtherStrength :: Int -> [Cell] -> Int
getDeltaOtherStrength sameStrength cells
    = sameStrength - getOthersStrength cells ^. value

getOthersStrength :: [Cell] -> Cell
getOthersStrength [] = mkCell 0 0
getOthersStrength (c:_) = c

getOtherStrength :: Int -> [Cell] -> Int
getOtherStrength _ [] = 0
getOtherStrength playerInd (c:cs)
    | isOwnedBy playerInd c
    = c ^. value
    | otherwise
    = getOtherStrength playerInd cs

toRange :: (Int, Int) -> Int -> Int
toRange (minval, maxval) = max minval . min maxval
