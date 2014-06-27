module GameLogic.AI.PossibleAction
    (PossibleAction (..)
    , calcPossibleActions
    , actionWeight
    , calcPossibleAction  --TODO: temp
    ) where

import Control.Lens
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.Action.Shield


--TODO: remove poses in defence
--TODO: change to enum and tupple
data PossibleAction = NoAction WorldPos Cell
                    | FreeCapture WorldPos Cell
                    | Increase WorldPos Cell
                    | NeedDefend WorldPos Cell [WorldPos]
                    | ParanoidNeedDefend WorldPos Cell [WorldPos]
                    | BorderNeedDefend WorldPos Cell [WorldPos]
                    | Conquer WorldPos Cell
                    | ReduceDefence WorldPos Cell Bool [WorldPos]
                    | Attack WorldPos Cell
                    | ShieldCharge Player
                    | ShieldActivate
                    | Unknown  WorldPos Cell
                    deriving (Show)

isNoAction :: PossibleAction -> Bool
isNoAction NoAction{} = True
isNoAction Unknown{} = True
isNoAction _ = False

actionWeight :: PossibleAction -> Int
actionWeight FreeCapture{} = 100
actionWeight Increase{} = 20
actionWeight NeedDefend{} = 1000
actionWeight ParanoidNeedDefend{} = 50
actionWeight BorderNeedDefend{} = 40
actionWeight Conquer{} = 800
actionWeight (ReduceDefence _ _ True _) = 1
actionWeight ReduceDefence{} = 5
actionWeight Attack{} = 1
actionWeight (ShieldCharge pl) = 100 + 2 * pl ^. shieldStrength
actionWeight ShieldActivate = 10000
actionWeight _ = error "Wrong PossibleAction in GameLogic.AI.PossibleAction.actionWeight" 

calcPossibleActions :: GameData -> Int -> [PossibleAction]
calcPossibleActions game playerInd
    = filter (not.isNoAction) actions <> calcPossibleShieldAction game playerInd
    where ((minX, minY), (maxX, maxY)) = aggroRect game playerInd
          poses = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
          actions = fmap (calcPossibleAction game playerInd free') poses
          free' = game ^?! playerOfGame playerInd . free

aggroRect :: GameData -> Int -> (WorldPos, WorldPos)
aggroRect game playerInd
    | pl ^. free > superAggroFree
    = (startWorldPos, (size, size))
    | otherwise
    = ((minX, minY), (maxX, maxY))
    where pl = game ^?! playerOfGame playerInd
          (spX, spY) = pl ^. selectedPos
          aggro = pl ^. aggr `div` 2
          size = getWorldSize $ view world game
          rng = (1, size)
          minX = toRange rng $ spX - aggro
          maxX = toRange rng $ spX + aggro
          minY = toRange rng $ spY - aggro
          maxY = toRange rng $ spY + aggro

calcPossibleAction :: GameData -> Int -> Int -> WorldPos -> PossibleAction
calcPossibleAction game playerInd free' pos
    = calcPossibleAction' pos cell strengths free'
        ownerPl defencePositions reduceDefencePositions
    where strengths = calcStrengthsForPlayerEx game playerInd pos
          cell = game ^?! cellOfGame pos
          defencePositions = getDefencePositions game playerInd pos
          reduceDefencePositions = getReduceDefencePositions game pos
          ownerPl = game ^?! playerOfGame (cell ^. playerIndex)

calcPossibleAction' :: WorldPos -> Cell -> StrengthsEx -> Int -> Player
    -> [WorldPos] -> [WorldPos] -> PossibleAction
calcPossibleAction' pos cell (same, others, sameStrength, deltaStrength) free'
  ownerPl defencePositions reduceDefencePositions
    | sameStrength == 0
    = NoAction pos cell
    -- for unowned cell
    | isFree cell
    && deltaStrength >= 0
    = FreeCapture pos cell
    | isFree cell
    = NoAction pos cell
    -- for owned cell
    | isOwnedBy samePlayerIndex cell
    && deltaStrength == -1
    && not (null defencePositions)
    = NeedDefend pos cell defencePositions
    | isOwnedBy samePlayerIndex cell
    && deltaStrength == 0
    && not (null defencePositions)
    = ParanoidNeedDefend pos cell defencePositions
    | isOwnedBy samePlayerIndex cell
    && deltaStrength > 0
    && not (null others)
    && not (null defencePositions)
    = BorderNeedDefend pos cell defencePositions
    | isOwnedBy samePlayerIndex cell
    && deltaStrength >= 0
    && cell ^. value < min maxCellValue sameStrength
    = Increase pos cell
    | isOwnedBy samePlayerIndex cell
    = NoAction pos cell
    -- for enemy cell
    | not (isOwnedBy samePlayerIndex cell)
    && (deltaStrength > 0 || (deltaStrength == 0 && sameStrength > ownerStrength))
    = Conquer pos cell
    | not (isOwnedBy samePlayerIndex cell)
    && free' >= 2
    && deltaStrength == 0
    && not (null reduceDefencePositions)
    = ReduceDefence pos cell (isShieldWorking ownerPl) reduceDefencePositions
    | not (isOwnedBy samePlayerIndex cell)
    && free' >= 2
    = Attack pos cell
    | otherwise
    = Unknown pos cell
    where samePlayerIndex = same ^. playerIndex
          ownerStrength = getOtherStrength (cell ^. playerIndex) others

getDefencePositions :: GameData -> Int-> WorldPos -> [WorldPos]
getDefencePositions game playerInd pos
    = filter (canBeSafeIncreased game playerInd) $ getNearestWorldPoses w pos
    where w = game ^. world

getReduceDefencePositions :: GameData -> WorldPos -> [WorldPos]
getReduceDefencePositions game pos
    = filter p $ getNearestWorldPoses w pos
    where w = game ^. world
          playerInd = w ^?! ix pos . playerIndex
          p pos' = isOwnedBy playerInd cell
                   where cell = w ^?! ix pos'

canBeSafeIncreased :: GameData -> Int -> WorldPos -> Bool
canBeSafeIncreased game playerInd pos
    | isFree cell
    && deltaStrength >= 0
    = True
    | not (isOwnedBy playerInd cell)
    = False
    | deltaStrength >= 0
    && cell ^. value < min maxCellValue sameStrength
    = True
    | otherwise
    = False
    where cell = game ^?! cellOfGame pos
          (_, _, sameStrength, deltaStrength)
              = calcStrengthsForPlayerEx game playerInd pos

calcPossibleShieldAction :: GameData -> Int -> [PossibleAction]
calcPossibleShieldAction game playerInd
    | pl ^. shieldActive
    = []
    | shieldStr >= shieldActivationStrength
    = return ShieldActivate
    | worldArea * maxCellValue < shieldAINumMultiplier * (pl ^. num)
    = return $ ShieldCharge pl
    | otherwise
    = []
    where pl = game ^?! playerOfGame playerInd
          shieldStr = pl ^. shieldStrength
          worldArea = getWorldSize (game ^. world) ^ (2 :: Int)
