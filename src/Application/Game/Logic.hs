module Application.Game.Logic where

import qualified Data.Boolean as B
import Middleware.FreeGame.Facade
import View.GameState
import View.Logic


eventHandler :: GameState
eventHandler = do
    Box _ (V2 w h) <- getBoundingBox
    updateWindowSize (w, h)

    mouseEvents
    whenM (keyDown KeyP) doChangePaused
    whenM (keyDown KeyS) doShieldAction
    whenM (keyDown KeyF1) doHelpPlayer
    whenM (keyDown KeyF2) doSave
    whenM (keyDown KeyF3) doLoad
    whenM (keyDown KeyPadAdd B.|| keyDown KeyEqual) increaseSpeed
    whenM (keyDown KeyPadSubtract B.|| keyDown KeyMinus) decreaseSpeed

mouseEvents :: GameState
mouseEvents = do
    V2 x y <- mousePosition
    let pos = (x, y)
    l <- mouseButtonL
    curL <- inPlacementMode
    
    whenM mouseButtonR $ centering pos
    case (l, curL) of
        (True, False) -> startPlacement pos
        (True, True)  -> drawing pos
        (False, True) -> stopPlacement
        _             -> return ()
