module Middleware.FreeGame.Environment
    ( runEnvironment
    , EnvironmentInfo(..)
    ) where

import FreeGame
import Control.Monad.State.Lazy
import View.State
import Middleware.FreeGame.GameState

data EnvironmentInfo = EnvironmentInfo
     { _drawState :: StateData -> Frame()
     , _runGameStep :: StateData -> StateData
     , _eventHandler :: GameState
     , _state :: StateData
     }

runEnvironment :: Int -> EnvironmentInfo -> IO (Maybe ())
runEnvironment ticksPerSecond info
    = runGame Resizable (Box (V2 0 0) (V2 800 550)) $ do
        setTitle "GameNumber"
        clearColor black
        setFPS ticksPerSecond
        gameLoop info

gameLoop :: EnvironmentInfo -> Game ()
gameLoop info = do
    state'' <- lift $ doFrame info
    unlessM (keyPress KeyEscape)
        $ tick >> gameLoop info{_state = state''}

doFrame :: EnvironmentInfo -> Frame StateData
doFrame info = do
    state' <- execStateT (eventHandler >> modify runGameStep) state
    drawState state'
    return state'
    where state = _state info
          drawState = _drawState info
          runGameStep = _runGameStep info
          eventHandler = _eventHandler info
