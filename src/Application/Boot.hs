module Application.Boot where

import Debug.Trace
import GameLogic.Data.Facade
import GameLogic.StartLogic
import Application.Game.Engine

boot = do
  runStartupTest
  game <- newGame
  trace "Starting" $ runEngine game

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
