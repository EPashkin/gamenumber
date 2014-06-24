{-# LANGUAGE TemplateHaskell #-}
module View.GameState
    ( module ST
    , GameState()
    , GameState'()
    , StateData(_game, _windowSize, _font)
    , newState
    -- lens
    , game
    , windowSize
    , font
    ) where

import Debug.Trace
import Control.Lens
import FreeGame
import Control.Monad.State.Lazy as ST hiding (state)
import GameLogic
import View.Convert


--TODO: Rename to ViewState and GameState
--TODO: Generalize GameState by monad, move to GameLogic
type GameState a = StateT StateData Frame a
type GameState' a = StateT GameData Frame a

instance (Show Font) where
  show _ = "Font"

data StateData = StateData { _game :: GameData
                   , _windowSize :: (Coord, Coord) -- current window size
                   , _font :: Font
                   }
  deriving (Show)

makeLenses ''StateData

newState :: Font -> IO StateData
newState fnt = do
    --runStartupTest
    game' <- newGame
    return $ StateData game' (100, 100) fnt

_runStartupTest :: IO ()
_runStartupTest = do
    traceIO "Testing"
    traceIO . show $ getNearestPoses (2,3)
