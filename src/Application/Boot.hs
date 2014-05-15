module Application.Boot where

import Debug.Trace
import GameLogic.Data.Facade
import Application.Game.Engine

boot = do
           let game = initialGame 100
           trace "Starting" $ runEngine game
