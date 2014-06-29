module GameLogic.AI.Actions
    ( doAIsGameStep
    ) where

import System.Random
import Data.List (find)
import Control.Lens
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.GameState
import GameLogic.AI.PossibleAction
import GameLogic.Action.Attack
import GameLogic.Action.Defend
import GameLogic.Action.Shield


doAIsGameStep :: GameState ()
doAIsGameStep = do
   plInds <- uses players $ {-take 1 $ -}fmap fst . filter (isAI . snd) . mapP id
   modify $ flip (foldl doAIGameStep) plInds

doAIGameStep :: GameData -> Int -> GameData
doAIGameStep game playerInd =
    if free' < 1 then game
    else doAIActions actions playerInd game
    where pl = game ^?! playerOfGame playerInd
          free' = pl ^. free
          actions = calcPossibleActions game playerInd

doAIActions :: [PossibleAction] -> Int -> GameData -> GameData
doAIActions [] _ game = game
doAIActions actions playerInd game
    = doAIAction action playerInd $ game & rndGen .~ gen'
    where (sumWeight, weightedActionsTmp) = foldl wf (0,[]) actions
          wf (w, acc) action' = (weight', (weight', action'):acc)
               where weight' = w + actionWeight action'
          weightedActions = reverse weightedActionsTmp
          (weight, gen') = randomR (0, sumWeight - 1) $ game ^. rndGen
          Just (_, action) = find (\(w, _) -> w > weight) weightedActions

doAIAction :: PossibleAction -> Int -> GameData -> GameData
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

increaseCellAction :: WorldPos -> Int -> GameData -> GameData
increaseCellAction pos playerInd
    = increaseCell pos playerInd . setSelectedPos' pos playerInd

defendCellAction :: [WorldPos] -> Int -> GameData -> GameData
defendCellAction poses playerInd game
    = increaseCellAction pos playerInd $ game & rndGen .~ gen'
    where (ind, gen') = randomR (0, length poses - 1) $ game ^. rndGen
          pos = poses !! ind

reduceDefenceCellAction :: [WorldPos] -> Int -> GameData -> GameData
reduceDefenceCellAction poses playerInd game
    = attackCellAction pos playerInd $ game & rndGen .~ gen'
    where (ind, gen') = randomR (0, length poses - 1) $ game ^. rndGen
          pos = poses !! ind

attackCellAction :: WorldPos -> Int -> GameData -> GameData
attackCellAction pos playerInd
    = attackCell pos playerInd . setSelectedPos' pos playerInd
