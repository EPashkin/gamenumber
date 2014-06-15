module GameLogic.AI.Actions
    ( doAIsGameStep
    ) where

import System.Random
import Data.List (find)
import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.AI.PossibleAction
import GameLogic.Action.Attack
import GameLogic.Action.Defend
import GameLogic.Action.Shield


doAIsGameStep :: GameData -> GameData
doAIsGameStep game = foldl p game plInds
   where pls = game ^. players
         --TODO: use all player indexes
         plInds =  {-take 1 $ -}fmap fst $ filter (isAI . snd) $ mapP id pls
         p g plInd = doAIGameStep plInd g

doAIGameStep :: Int -> GameData -> GameData
doAIGameStep playerInd game
    | free' < 1
    = game
    | otherwise
    = doAIActions actions playerInd game
    where Just pl = game ^? players . ix playerInd
          free' = pl ^. free
          actions = calcPossibleActions game playerInd

doAIActions :: [PossibleAction] -> Int -> GameData -> GameData
doAIActions [] _ game = game
doAIActions actions playerInd game
    = doAIAction action playerInd $ game & rndGen .~ gen'
    where (sumWeight, weightedActionsTmp) = foldl wf (0,[]) actions
          wf (w, acc) act = (weight, (weight, act):acc)
               where weight = w + actionWeight act
          weightedActions = reverse weightedActionsTmp
          (weight, gen') = randomR (0, sumWeight - 1) $ game ^. rndGen
          Just (_, action) = find (\(w, _) -> w > weight) weightedActions

doAIAction :: PossibleAction -> Int -> GameData -> GameData
doAIAction (FreeCapture pos _) playerIndex
    = increaseCellAction pos playerIndex
doAIAction (Increase pos _) playerIndex
    = increaseCellAction pos playerIndex
doAIAction (NeedDefend pos _ poses) playerIndex
    = defendCellAction poses playerIndex
doAIAction (ParanoidNeedDefend pos _ poses) playerIndex
    = defendCellAction poses playerIndex
doAIAction (BorderNeedDefend pos _ poses) playerIndex
    = defendCellAction poses playerIndex
doAIAction (Conquer pos _) playerIndex
    = attackCellAction pos playerIndex
doAIAction (ReduceDefence pos _ _ poses) playerIndex
    = reduceDefenceCellAction poses playerIndex
doAIAction (Attack pos _) playerIndex
    = attackCellAction pos playerIndex
doAIAction (ShieldCharge _) playerIndex
    = shieldAction playerIndex
doAIAction ShieldActivate playerIndex
    = shieldAction playerIndex

increaseCellAction pos playerIndex
    = increaseCell pos playerIndex . setSelectedPos pos playerIndex

defendCellAction :: [WorldPos] -> Int -> GameData -> GameData
defendCellAction poses playerIndex game
    = increaseCellAction pos playerIndex $ game & rndGen .~ gen'
    where (ind, gen') = randomR (0, length poses - 1) $ game ^. rndGen
          pos = poses !! ind

reduceDefenceCellAction poses playerIndex game
    | null poses
    = game
    | otherwise 
    = reduceDefenceCellAction' poses playerIndex game

reduceDefenceCellAction' :: [WorldPos] -> Int -> GameData -> GameData
reduceDefenceCellAction' poses playerIndex game
    = attackCellAction pos playerIndex $ game & rndGen .~ gen'
    where (ind, gen') = randomR (0, length poses - 1) $ game ^. rndGen
          pos = poses !! ind

attackCellAction pos playerIndex
    = attackCell pos playerIndex . setSelectedPos pos playerIndex
