module Application.Boot where

import Debug.Trace (trace)
import View.ViewState (newState)
import Application.Game.Engine (runEngine)
import Middleware.FreeGame.Facade (loadFont)

boot = do
  font <- loadFont "./Data/VL-PGothic-Regular.ttf"
  state <- newState font
  trace "Starting" $ runEngine state
