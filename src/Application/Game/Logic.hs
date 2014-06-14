module Application.Game.Logic where

import Middleware.FreeGame.Facade
import Middleware.FreeGame.GameState
import Control.Monad.State.Lazy
import View.State


eventHandler :: GameState
eventHandler = do
    updateWindowSize'
    mouseEvents
    whenGameState (keyDown KeyP) $ overGameState doChangePaused
    whenGameState (keyDown KeyS) $ overGameState doShieldAction
    whenGameState (keyDown KeyF1) $ overGameState doHelpPlayer
    whenGameState (keyDown KeyF2) $ overIOGameState doSave
    whenGameState (keyDown KeyF3) $ overIOGameState doLoad

updateWindowSize' :: GameState
updateWindowSize' = do
    Box _ (V2 w h) <- getBoundingBox
    overGameState $ updateWindowSize (w, h)

mouseEvents :: GameState
mouseEvents = do
    V2 x y <- mousePosition
    let pos = (x, y)
    l <- mouseButtonL
    state <- get 
    
    whenGameState mouseButtonR . overGameState $ centering pos
    case (l, inPlacementMode state) of
        (True, False) -> overGameState $ startPlacement pos
        (True, True)  -> overGameState $ drawing pos
        (False, True) -> overGameState stopPlacement
        _             -> get
