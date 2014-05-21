{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.Gloss.Facade (Picture)
import GameLogic.Data.Facade
import GameLogic.Logic
import GameLogic.StartLogic
import View.View
import View.Convert

data State = State { _game :: Game
                   }
  deriving (Show)
  
makeLenses ''State

newState :: IO State
newState = do
    runStartupTest
    game <- newGame
    return $ State game   

runStartupTest = do
   g1 <- newGame
   traceIO $ show g1
   let players = [1..defNumPlayers]
       w2 = getWorld g1
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList defWorldSize defNumPlayers
   traceIO $ show positions
   traceIO $ show xList
   traceIO $ show $ getGameCell (2,2) g1
   traceIO $ show $ getNearestPoses (2,3)


drawState :: State -> IO Picture
drawState = drawGame . view game

runGameStep :: Float -> State -> IO State
runGameStep _ = return . over game doGameStep

startPlacement :: (Float, Float) -> State -> State
startPlacement pos = over game p
    where p game = set placementMode True $ doSelectCellAction pos' game
                   where pos' = worldPosOfWindowPos game pos

stopPlacement :: State -> State
stopPlacement = over game p
    where p = set placementMode False

inPlacementMode :: State -> Bool
inPlacementMode = view placementMode . view game

centering :: (Float, Float) -> State -> State
centering pos = over game p
    where p game = setCenterPosLimited pos' game
            where pos' = worldPosOfWindowPos game pos

drawing :: (Float, Float) -> State -> State
drawing pos = over game p
    where p game = doSelectCellAction pos' game 
            where pos' = worldPosOfWindowPos game pos
