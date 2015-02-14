module Application.Boot where

import Debug.Trace (trace)
import View.State (newState)
import Application.Game.Engine (runEngine)

boot = do
  state <- newState
  trace "Starting" $ runEngine state
