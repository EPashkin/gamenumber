module Application.Game.Engine where

import Application.Game.Logic (eventHandler)
import Middleware.FreeGame.Environment (runEnvironment, EnvironmentInfo(..))
import View.Logic (runGameStep)
import View.View (drawState)

ticksPerSecond = 10

runEngine state = runEnvironment ticksPerSecond info
    where info = EnvironmentInfo drawState runGameStep eventHandler state
