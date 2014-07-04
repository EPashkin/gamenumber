module GameLogic.AI.Actions
    ( doAIsGameStep
    ) where

import Data.List (find)
import Control.Lens
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.Util.RandomState
import GameLogic.GameState
import GameLogic.AI.PossibleAction
import GameLogic.Action.Attack
import GameLogic.Action.Defend
import GameLogic.Action.Shield


doAIsGameStep :: GameState ()
doAIsGameStep = do
   plInds <- uses players $ {-take 1 $ -}fmap fst . filter (isAI . snd) . mapP id
   mapM_ doAIGameStep plInds

doAIGameStep :: Int -> GameState ()
doAIGameStep playerInd = do
    free' <- use $ playerOfGame playerInd . free
    actions <- gets $ flip calcPossibleActions playerInd
    when (free' >= 1) $ doAIActions actions playerInd

doAIActions :: [PossibleAction] -> Int -> GameState ()
doAIActions [] _ = return ()
doAIActions actions playerInd = do
    let (sumWeight, weightedActionsTmp) = foldl wf (0,[]) actions
    let weightedActions = reverse weightedActionsTmp
    weight <- zoom rndGen $ randomRSt (0, sumWeight - 1)
    let Just (_, action) = find (\(w, _) -> w > weight) weightedActions
    doAIAction action playerInd
    where wf (w, acc) action' = (weight', (weight', action'):acc)
            where weight' = w + actionWeight action'

doAIAction :: PossibleAction -> Int -> GameState ()
doAIAction (FreeCapture pos _) playerInd
    = increaseCellAction pos playerInd
doAIAction (Increase pos _) playerInd
    = increaseCellAction pos playerInd
doAIAction (NeedDefend _pos _ poses) playerInd
    = defendCellAction poses playerInd
doAIAction (ParanoidNeedDefend _pos _ poses) playerInd
    = defendCellAction poses playerInd
doAIAction (BorderNeedDefend _pos _ poses) playerInd
    = defendCellAction poses playerInd
doAIAction (Conquer pos _) playerInd
    = attackCellAction pos playerInd
doAIAction (ReduceDefence _pos _ _ poses) playerInd
    = reduceDefenceCellAction poses playerInd
doAIAction (Attack pos _) playerInd
    = attackCellAction pos playerInd
doAIAction (ShieldCharge _) playerInd
    = shieldAction playerInd
doAIAction ShieldActivate playerInd
    = shieldAction playerInd
doAIAction _ _ = error "Wrong PossibleAction in GameLogic.AI.Actions.doAIAction"

increaseCellAction :: WorldPos -> Int -> GameState ()
increaseCellAction pos playerInd
    = increaseCell pos playerInd >> setSelectedPos pos playerInd

defendCellAction :: [WorldPos] -> Int -> GameState ()
defendCellAction poses playerInd = do
    ind <- zoom rndGen $ randomRSt (0, length poses - 1)
    let pos = poses !! ind
    increaseCellAction pos playerInd

reduceDefenceCellAction :: [WorldPos] -> Int -> GameState ()
reduceDefenceCellAction poses playerInd = do
    ind <- zoom rndGen $ randomRSt (0, length poses - 1)
    let pos = poses !! ind
    attackCellAction pos playerInd

attackCellAction :: WorldPos -> Int -> GameState ()
attackCellAction pos playerInd
    = modify (attackCell pos playerInd) >> setSelectedPos pos playerInd
