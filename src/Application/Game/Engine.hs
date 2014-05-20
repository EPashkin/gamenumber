module Application.Game.Engine where

import GameLogic.Data.Facade (Game)
import Application.Game.Logic (eventHandler)
import GameLogic.Logic (doGameStep)
import Middleware.Gloss.Environment (runEnvironment)
import View.View (drawGame)

ticksPerSecond = 10

runEngine game = runEnvironment ticksPerSecond game drawGame eventHandler runGameStep


runGameStep :: Float -> Game -> IO Game
runGameStep _ = return . doGameStep
