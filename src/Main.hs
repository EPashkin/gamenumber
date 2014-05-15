module Main where

import Application.Boot

import GameLogic.Data.Facade
import System.Random

main = do
   gen1 <- newStdGen
--   let gen1 = mkStdGen 100
   let (w2, gen2) = mkStartWorld 8 4 gen1
--   print $ show w2
   let players = [1..4]
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList (getWorldSize w2) (length players)
   print $ show positions
   print $ show xList
   print $ show $ getWorldCell w2 (3,3)

--   boot
