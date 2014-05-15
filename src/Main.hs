module Main where

import Application.Boot

import GameLogic.Data.Facade
import System.Random

main = do
   gen1 <- newStdGen
--   let gen1 = mkStdGen 100
   g1 <- newGame
   print $ show g1
   let players = [1..defNumPlayers]
       w2 = getWorld g1
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList defWorldSize defNumPlayers
   print $ show positions
   print $ show xList
   print $ show $ getGameCell g1 (3,3)

--   boot
