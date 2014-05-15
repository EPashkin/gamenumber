module Middleware.Gloss.Environment where

import Graphics.Gloss.Interface.IO.Game

runEnvironment game drawGame eventHandler doGameStep = playIO (InWindow "GameNumber" (800, 550) (10, 10)) black 1 game drawGame eventHandler doGameStep
