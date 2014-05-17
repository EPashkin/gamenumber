module Application.Boot where

import Debug.Trace
import GameLogic.Data.Facade
import Application.Game.Engine
import System.Random

boot = do
  runStartupTest
  game <- newGame
  trace "Starting" $ runEngine game

runStartupTest = do
   gen1 <- newStdGen
--   let gen1 = mkStdGen 100
   g1 <- newGame
   traceIO $ show g1
   let players = [1..defNumPlayers]
       w2 = getWorld g1
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList defWorldSize defNumPlayers
   traceIO $ show positions
   traceIO $ show xList
   traceIO $ show $ getGameCell g1 (3,3)
