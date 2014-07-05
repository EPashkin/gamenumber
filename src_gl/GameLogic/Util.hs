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


infixl 1 >>==
(>>==) :: Functor m => m a -> (a -> b) -> m b
(>>==) ma f = fmap f ma 

--TODO: remove when unused
iif :: (a -> Bool) -> (a -> a) -> a -> a
iif bf f val = if bf val then f val else val

setSelectedPos :: WorldPos -> Int -> GameState()
setSelectedPos pos playerInd = whenM (isPosInGame pos)
    $ playerOfGame playerInd . selectedPos .= pos

--TODO: remove when unused
setSelectedPos' :: WorldPos -> Int -> GameData -> GameData
setSelectedPos' pos playerInd = iif (isPosInGame' pos)
    $ playerOfGame playerInd . selectedPos .~ pos

--TODO: remove when unused
isPosInGame' :: WorldPos -> GameData -> Bool
isPosInGame' pos = (`isPosInWorld` pos) . view world

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

calcSumOwnedNearest :: GameData -> Int -> WorldPos -> Int
calcSumOwnedNearest game playerInd
    = foldl p 0 . getNearestOwnedCells playerInd (view world game)
    where p acc val = (+) acc $ (view value . snd) val

calcSumAllNearest :: GameData -> WorldPos -> [Cell]
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
                  _ -> error "Wrong list in GameLogic.Util.updateCellList"

type StrengthsEx = (Cell, [Cell], Int, Int)

calcStrengthsForPlayerEx :: GameData -> Int -> WorldPos -> StrengthsEx
calcStrengthsForPlayerEx game playerInd pos
    = (same, others, sameStrength, deltaStrength)
    where (same, others) = calcStrengthsForPlayer game playerInd pos
          sameStrength = same ^. value
          deltaStrength = getDeltaOtherStrength sameStrength others

calcStrengthsForPlayer :: GameData -> Int -> WorldPos -> (Cell, [Cell])
calcStrengthsForPlayer game playerInd pos
    = (same', sortBy p others)
    where cells = calcSumAllNearest game pos
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
