module Middleware.Gloss.Environment where

import Graphics.Gloss.Interface.IO.Game

runEnvironment:: Int -> game -> (game -> IO Picture) -> (Event -> game -> IO game)
    -> (Float -> game -> IO game) -> IO ()
runEnvironment = playIO (InWindow "GameNumber on gloss" (800, 550) (10, 10)) black
