module Application.Game.Engine where

import Application.Game.Logic
import Middleware.Gloss.Facade
import Middleware.Gloss.Environment
import View.View

runEngine game = runEnvironment game drawGame eventHandler doGameStep

doGameStep :: Float -> game -> IO game
doGameStep _ = return
