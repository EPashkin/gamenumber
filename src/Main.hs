module Main where

import Application.Boot

import GameLogic.Data.Facade
import System.Random

main = do
   gen1 <- newStdGen
--   let gen1 = mkStdGen 100
   let (w2, gen2) = mkStartWorld 8 4 gen1
   print $ show w2
--   boot
