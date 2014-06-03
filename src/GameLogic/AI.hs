module GameLogic.AI where

import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util

data PossibleAction = NoAction WorldPos Cell
                    | Increase WorldPos Cell
                    | Unknown  WorldPos Cell
                    deriving (Show)

isNoAction :: PossibleAction -> Bool
isNoAction (NoAction _ _) = True
isNoAction (Unknown _ _) = True
isNoAction _ = False

calcPossibleActions :: Game -> Int -> [PossibleAction]
calcPossibleActions game playerInd
    = filter (not.isNoAction) actions
    where ((minX, minY), (maxX, maxY)) = aggroRect game playerInd
          poses = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
          actions = map (calcPossibleAction game playerInd) poses

aggroRect :: Game -> Int -> (WorldPos, WorldPos)
aggroRect game playerInd = ((minX, minY), (maxX, maxY))
    where Just pl = game ^? players. ix playerInd
          (spX, spY) = pl ^. selectedPos
          aggro = pl ^. aggr
          size = getWorldSize $ view world game
          rng = (1, size)
          minX = toRange rng $ spX - aggro
          maxX = toRange rng $ spX + aggro
          minY = toRange rng $ spY - aggro
          maxY = toRange rng $ spY + aggro

calcPossibleAction :: Game -> Int -> WorldPos -> PossibleAction
calcPossibleAction game playerInd pos
    = calcPossibleAction' pos cell strengths
    where strengths = calcStrengthsForPlayer game playerInd pos
          Just cell = game ^? cellOfGame pos

calcPossibleAction' :: WorldPos -> Cell -> (Cell, [Cell]) -> PossibleAction
calcPossibleAction' pos cell (same, others)
    | sameStrength == 0
    = NoAction pos cell
    | sameStrength >= otherStrength
    && (isFree cell || isOwnedBy samePlayerIndex cell)
    = Increase pos cell
    | otherwise
    = Unknown pos cell
    where sameStrength = same ^. value
          samePlayerIndex = same ^. playerIndex
          otherStrengthInfo = getOthersStrength others
          otherStrength = otherStrengthInfo ^. value

getOthersStrength :: [Cell] -> Cell
getOthersStrength [] = mkCell 0 0
getOthersStrength (c:cs) = c
