module Application.Game.Engine where

import Application.Game.Logic
import GameLogic.Data.Facade
import Middleware.Gloss.Facade
import Middleware.Gloss.Environment
import View.View

runEngine game = runEnvironment game drawGame eventHandler doGameStep

doGameStep :: Float -> Game -> IO Game
doGameStep _ game = 
   return game
