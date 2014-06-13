module Middleware.FreeGame.Environment where

import FreeGame
import Control.Monad.Reader (unless)
import Control.Monad.Trans.Class (lift)
import Control.Lens
import View.State

{-
runEnvironment:: Int -> game -> (game -> IO Picture) -> (Event -> game -> IO game)
    -> (Float -> game -> IO game) -> IO ()
runEnvironment = playIO (InWindow "GameNumber" (800, 550) (10, 10)) black
-}

runEnvironment :: Int -> StateData -> (StateData -> Frame()) -> {- (Float -> State -> Frame State) -> -} IO (Maybe ())
runEnvironment ticksPerSecond state drawState {-runStep-} = runGame Resizable (Box (V2 0 0) (V2 800 550)) $ do
    setTitle "GameNumber"
    clearColor black
    setFPS ticksPerSecond
    subLoop drawState state

subLoop :: (StateData -> Frame()) -> StateData -> Game ()
subLoop drawState state = do
    let state' = state & counter +~ 1

    Box _ (V2 w h) <- getBoundingBox
    let state'' = state'& windowSize .~ (w, h)

    lift $ drawState state''

    key <- keyPress KeyEscape
    unless key $ tick >> subLoop drawState state''
