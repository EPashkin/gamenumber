module GameLogic.AI
    ( doAIsGameStep
    , calcPossibleActions --TODO: temp
    , calcPossibleAction  --TODO: temp
    ) where

import System.Random
import Control.Lens
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Util
import GameLogic.Action.Defend

data PossibleAction = NoAction WorldPos Cell
                    | FreeCapture WorldPos Cell
                    | Increase WorldPos Cell
                    | Unknown  WorldPos Cell
                    deriving (Show)

isNoAction :: PossibleAction -> Bool
isNoAction (NoAction _ _) = True
isNoAction (Unknown _ _) = True
isNoAction _ = False

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
    where (ind, gen') = randomR (0, length actions - 1) $ game ^. rndGen
          action = actions !! ind

doAIAction :: PossibleAction -> Int -> Game -> Game
doAIAction (FreeCapture pos _) playerIndex
    = increaseCellAction pos playerIndex
doAIAction (Increase pos _) playerIndex
    = increaseCellAction pos playerIndex

increaseCellAction pos playerIndex
    = increaseCell pos playerIndex . setSelectedPos pos playerIndex

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
    && isFree cell
    = FreeCapture pos cell
    | sameStrength >= otherStrength
    && isOwnedBy samePlayerIndex cell
    && cell ^. value < min 9 sameStrength
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
