{-# LANGUAGE TemplateHaskell #-}
module View.ViewState
    ( module ST
    , ViewState()
    , GameStateF()
    , ViewData(_game, _windowSize, _font)
    , newState
    -- lens
    , game
    , windowSize
    , font
    , Frame()
    ) where

import Debug.Trace
import Control.Lens
import FreeGame
import Control.Monad.State.Lazy as ST hiding (state)
import GameLogic
import View.Convert


type ViewState a = StateT ViewData Frame a
type GameStateF a = GameState Frame a

instance (Show Font) where
  show _ = "Font"

data ViewData = ViewData { _game :: GameData
                   , _windowSize :: (Coord, Coord) -- current window size
                   , _font :: Font
                   }
  deriving (Show)

makeLenses ''ViewData

newState :: Font -> IO ViewData
newState fnt = do
    --runStartupTest
    game' <- newGame
    return $ ViewData game' (100, 100) fnt

_runStartupTest :: IO ()
_runStartupTest = do
    traceIO "Testing"
    traceIO . show $ getNearestPoses (2,3)
