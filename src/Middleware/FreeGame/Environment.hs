module Middleware.FreeGame.Environment
    ( runEnvironment
    ) where

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
        gameLoop drawState runStep eventHandler state

gameLoop :: (StateData -> Frame()) -> (StateData -> StateData)
    -> GameState -> StateData -> Game ()
gameLoop drawState runStep eventHandler state = do
    state'' <- lift $ doFrame drawState runStep eventHandler state
    unlessM (keyPress KeyEscape)
        $ tick >> gameLoop drawState runStep eventHandler state''

doFrame :: (StateData -> Frame()) -> (StateData -> StateData)
    -> GameState -> StateData -> Frame StateData
doFrame drawState runStep eventHandler state = do
    state' <- runGameState (eventHandler >> overGameState runStep) state
    drawState state'
    return state'
