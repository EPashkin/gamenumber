module Application.Boot where

import Debug.Trace
import GameLogic.Data.Facade
import Application.Game.Engine

boot = do
           game <- newGame
           trace "Starting" $ runEngine game
