module Middleware.FreeGame.GameState where

import FreeGame
import Control.Monad.State.Lazy
import View.State


--TODO: Include in Middleware.FreeGame.Facade
type GameStateA a = StateT StateData Frame a

type GameState = GameStateA ()

overIOGameState :: (StateData -> IO StateData) -> GameState
overIOGameState f = get >>= (liftIO . f) >>= put
