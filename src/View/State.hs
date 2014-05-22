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
       w2 = g1 ^. world
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList defWorldSize defNumPlayers
   traceIO $ show positions
   traceIO $ show xList
   traceIO $ show $ g1 ^. cellOfGame (2,2) 
   traceIO $ show $ getNearestPoses (2,3)


drawState :: State -> IO Picture
drawState = drawGame . view game

runGameStep :: Float -> State -> IO State
runGameStep _ = return . over game doGameStep

startPlacement :: (Float, Float) -> State -> State
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: State -> State
stopPlacement = set placementModeOfGame False

inPlacementMode :: State -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' State Bool
placementModeOfGame = game . placementMode

centering :: (Float, Float) -> State -> State
centering = doWithWindowPos setCenterPosLimited

drawing :: (Float, Float) -> State -> State
drawing = doWithWindowPos doSelectCellAction 

doWithWindowPosOnGame :: (WorldPos -> Game -> Game) -> (Float, Float) -> Game-> Game
doWithWindowPosOnGame action pos game = action pos' game
            where pos' = worldPosOfWindowPos game pos

doWithWindowPos :: (WorldPos -> Game -> Game) -> (Float, Float) -> State-> State
doWithWindowPos action pos = game %~ doWithWindowPosOnGame action pos
