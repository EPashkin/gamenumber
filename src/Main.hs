module Main where

import Application.Boot

import GameLogic.Data.Facade
import System.Random
import Debug.Trace

main = do
   runStartupTest
   boot

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
