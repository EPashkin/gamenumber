module GameLogic.AI where

import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Util

data PossibleAction = NoAction WorldPos Cell
                    | Increase WorldPos Cell
                    | Unknown  WorldPos Cell
                    deriving (Show)

isNoAction :: PossibleAction -> Bool
isNoAction (NoAction _ _) = True
isNoAction _ = False

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
