module Application.Boot where

import Debug.Trace (trace)
import View.ViewState (newState)
import Application.Game.Engine (runEngine)
import Middleware.FreeGame.Facade (loadFont)
import Paths_gamenumber

boot = do
  fontFile <- getDataFileName "VL-PGothic-Regular.ttf"
  font <- loadFont fontFile
  state <- newState font
  trace "Starting" $ runEngine state
