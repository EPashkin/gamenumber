module GameLogic.AI
    ( doAIsGameStep
    , calcPossibleActions --TODO: temp
    , calcPossibleAction  --TODO: temp
    ) where

import System.Random
import Data.List (find)
import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.Action.Attack
import GameLogic.Action.Defend

--TODO: remove poses in defence
--TODO: change to enum and tupple
data PossibleAction = NoAction WorldPos Cell
                    | FreeCapture WorldPos Cell
                    | Increase WorldPos Cell
                    | NeedDefend WorldPos Cell [WorldPos]
                    | ParanoidNeedDefend WorldPos Cell [WorldPos]
                    | Conquer WorldPos Cell
                    | Unknown  WorldPos Cell
                    deriving (Show)

isNoAction :: PossibleAction -> Bool
isNoAction NoAction{} = True
isNoAction Unknown{} = True
isNoAction _ = False

actionWeight :: PossibleAction -> Int
actionWeight FreeCapture{} = 100
actionWeight Increase{} = 10
actionWeight NeedDefend{} = 1000
actionWeight ParanoidNeedDefend{} = 50
actionWeight Conquer{} = 800

doAIsGameStep :: Game -> Game
doAIsGameStep game = foldl p game plInds
   where pls = game ^. players
         --TODO: use all player indexes
         plInds =  take 1 $ fmap fst $ filter f $ mapP id pls
         f (ind, pl) = 0 < pl ^. aggr
         p g plInd = doAIGameStep plInd g

doAIGameStep :: Int -> Game -> Game
doAIGameStep playerInd game
    | free' < 1
    = game
    | otherwise
    = doAIActions actions playerInd game
    where Just pl = game ^? players . ix playerInd
          free' = pl ^. free
          actions = calcPossibleActions game playerInd

doAIActions :: [PossibleAction] -> Int -> Game -> Game
doAIActions [] _ game = game
doAIActions actions playerInd game
    = doAIAction action playerInd $ game & rndGen .~ gen'
    where (sumWeight, weightedActionsTmp) = foldl wf (0,[]) actions
          wf (w, acc) act = (weight, (weight, act):acc)
               where weight = w + actionWeight act
          weightedActions = reverse weightedActionsTmp
          (weight, gen') = randomR (0, sumWeight - 1) $ game ^. rndGen
          Just (_, action) = find (\(w, _) -> w > weight) weightedActions

doAIAction :: PossibleAction -> Int -> Game -> Game
doAIAction (FreeCapture pos _) playerIndex
    = increaseCellAction pos playerIndex
doAIAction (Increase pos _) playerIndex
    = increaseCellAction pos playerIndex
doAIAction (NeedDefend pos _ poses) playerIndex
    = defenceCellAction poses playerIndex
doAIAction (ParanoidNeedDefend pos _ poses) playerIndex
    = defenceCellAction poses playerIndex
doAIAction (Conquer pos _) playerIndex
    = attackCell pos playerIndex

increaseCellAction pos playerIndex
    = increaseCell pos playerIndex . setSelectedPos pos playerIndex

defenceCellAction :: [WorldPos] -> Int -> Game -> Game
defenceCellAction poses playerIndex game
    = increaseCellAction pos playerIndex $ game & rndGen .~ gen'
    where (ind, gen') = randomR (0, length poses - 1) $ game ^. rndGen
          pos = poses !! ind

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
    = calcPossibleAction' pos cell strengths defencePositions
    where strengths = calcStrengthsForPlayerEx game playerInd pos
          Just cell = game ^? cellOfGame pos
          defencePositions = getDefencePositions game playerInd pos

calcPossibleAction' :: WorldPos -> Cell -> StrengthsEx -> [WorldPos] -> PossibleAction
calcPossibleAction' pos cell (same, _, sameStrength, deltaStrength) defencePositions
    | sameStrength == 0
    = NoAction pos cell
    | deltaStrength >= 0
    && isFree cell
    = FreeCapture pos cell
    | isOwnedBy samePlayerIndex cell
    && deltaStrength == -1
    && not (null defencePositions)
    = NeedDefend pos cell defencePositions
    | isOwnedBy samePlayerIndex cell
    && deltaStrength == 0
    && not (null defencePositions)
    = ParanoidNeedDefend pos cell defencePositions
    | isOwnedBy samePlayerIndex cell
    && deltaStrength >= 0
    && cell ^. value < min 9 sameStrength
    = Increase pos cell
    | not (isOwnedBy samePlayerIndex cell)
    && deltaStrength > 0
    = Conquer pos cell
    | otherwise
    = Unknown pos cell
    where samePlayerIndex = same ^. playerIndex

getDefencePositions :: Game -> Int-> WorldPos -> [WorldPos]
getDefencePositions game playerInd pos
    = filter (canBeSafeIncreased game playerInd) $ getNearestWorldPoses w pos
    where w = game ^. world

canBeSafeIncreased :: Game -> Int -> WorldPos -> Bool
canBeSafeIncreased game playerInd pos
    | isFree cell
    && deltaStrength > 0
    = True
    | not (isOwnedBy playerInd cell)
    = False
    | deltaStrength > 0
    && cell ^. value < min 9 sameStrength
    = True
    | otherwise
    = False
    where Just cell = game ^? cellOfGame pos
          (_, _, sameStrength, deltaStrength)
              = calcStrengthsForPlayerEx game playerInd pos
