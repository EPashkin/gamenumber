module GameLogic.GameState
    ( module ST
    , module GameLogic.GameState
    ) where

import Control.Monad.State.Lazy as ST hiding (state)
import GameLogic.Data.World
import GameLogic.Data.Game


type GameState a = State GameData a

type WorldAction = WorldPos -> GameState ()
