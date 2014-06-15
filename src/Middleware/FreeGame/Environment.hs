module Middleware.FreeGame.Environment where

import FreeGame
import Control.Monad.State.Lazy
import View.State
import Middleware.FreeGame.GameState


runEnvironment :: Int -> StateData
    -> (StateData -> Frame()) -> (StateData -> StateData) -> GameState
    -> IO (Maybe ())
runEnvironment ticksPerSecond state drawState runStep eventHandler
    = runGame Resizable (Box (V2 0 0) (V2 800 550)) $ do
        setTitle "GameNumber"
        clearColor black
        setFPS ticksPerSecond
        subLoop drawState runStep eventHandler state

subLoop :: (StateData -> Frame()) -> (StateData -> StateData) -> GameState -> StateData -> Game ()
subLoop drawState runStep eventHandler state = do
    state'' <- lift $ do
       state' <- runGameState (eventHandler >> overGameState runStep) state
       drawState state'
       return state'
    unlessM (keyPress KeyEscape) $ tick >> subLoop drawState runStep eventHandler state''
