module GameLogic.GameState
    ( module ST
    , module GameLogic.GameState
    ) where

import Control.Monad.State.Lazy as ST hiding (state)
import GameLogic.Data.Game


type GameState m a = Monad m => StateT GameData m a
