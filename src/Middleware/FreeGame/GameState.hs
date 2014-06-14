module Middleware.FreeGame.GameState where

import FreeGame
import Control.Monad.State.Lazy
import View.State


--TODO: Include in Middleware.FreeGame.Facade
type GameStateA a = StateT StateData Game a

type GameState = GameStateA StateData

runGameState :: GameState -> StateData-> Game StateData
runGameState = evalStateT

overGameState :: (StateData -> StateData) -> GameState
overGameState f = modify f >> get 

whenGameState :: GameStateA Bool -> GameState -> GameState
whenGameState mp m = mp >>= bool get m
