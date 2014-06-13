module Application.Game.Engine where

--import Application.Game.Logic (eventHandler)
import Middleware.FreeGame.Environment (runEnvironment)
import View.State (runGameStep)
import View.View (drawState)

ticksPerSecond = 10

--runEngine state = runEnvironment ticksPerSecond state drawState eventHandler runGameStep
runEngine state = runEnvironment ticksPerSecond state drawState --runGameStep 
