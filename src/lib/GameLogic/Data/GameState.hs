module GameLogic.Data.GameState
    ( module ST
    , module GameLogic.Data.GameState
    ) where

import Data.Maybe
import Data.Monoid
import Control.Monad.State.Lazy as ST hiding (state)
import qualified Control.Monad.State.Lazy as ST' (state)
import Control.Monad.Identity
import GameLogic.Data.World
import GameLogic.Data.Game


type GameState = State GameData

type MaybeGameState = StateT GameData Maybe

type WorldAction = WorldPos -> GameState ()

{-# INLINE mkState #-}
mkState :: MonadState s m => (s -> (a, s)) -> m a
mkState = ST'.state

--fromMaybeState :: Monoid a => StateT s Maybe a -> StateT s Identity a
fromMaybeState :: Monoid a => MaybeGameState a -> GameState a
fromMaybeState mst = do
   s <- get
   mapStateT (Identity . fromMaybe (mempty, s)) mst
