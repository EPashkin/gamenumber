module Middleware.FreeGame.Environment
    ( runEnvironment
    , EnvironmentInfo(..)
    ) where

import FreeGame
import View.ViewState

data EnvironmentInfo = EnvironmentInfo
     { _drawState :: ViewState ()
     , _runGameStep :: ViewState ()
     , _eventHandler :: ViewState ()
     , _state :: ViewData
     }

runEnvironment :: Int -> EnvironmentInfo -> IO (Maybe ())
runEnvironment ticksPerSecond info
    = runGame Resizable (Box (V2 0 0) (V2 800 550)) $ do
        setTitle "GameNumber on free-game"
        clearColor black
        setFPS $ fromIntegral ticksPerSecond
        gameLoop info

gameLoop :: EnvironmentInfo -> Game ()
gameLoop info = do
    state' <- lift $ doFrame info
    unlessM (keyPress KeyEscape)
        $ tick >> gameLoop info{_state = state'}

doFrame :: EnvironmentInfo -> Frame ViewData
doFrame info = execStateT (eventHandler >> runGameStep >> drawState) state
    where state = _state info
          drawState = _drawState info
          runGameStep = _runGameStep info
          eventHandler = _eventHandler info
