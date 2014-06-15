module Application.Game.Logic where

import qualified Data.Boolean as B
import Middleware.FreeGame.Facade
import Middleware.FreeGame.GameState
import Control.Monad.State.Lazy
import View.State


eventHandler :: GameState
eventHandler = do
    updateWindowSize'
    mouseEvents
    whenM (keyDown KeyP) $ modify doChangePaused
    whenM (keyDown KeyS) $ modify doShieldAction
    whenM (keyDown KeyF1) $ modify doHelpPlayer
    whenM (keyDown KeyF2) $ overIOGameState doSave
    whenM (keyDown KeyF3) $ overIOGameState doLoad
    whenM (keyDown KeyPadAdd B.|| keyDown KeyEqual)
        $ modify increaseSpeed
    whenM (keyDown KeyPadSubtract B.|| keyDown KeyMinus)
        $ modify decreaseSpeed

updateWindowSize' :: GameState
updateWindowSize' = do
    Box _ (V2 w h) <- getBoundingBox
    modify $ updateWindowSize (w, h)

mouseEvents :: GameState
mouseEvents = do
    V2 x y <- mousePosition
    let pos = (x, y)
    l <- mouseButtonL
    state <- get 
    
    whenM mouseButtonR . modify $ centering pos
    case (l, inPlacementMode state) of
        (True, False) -> modify $ startPlacement pos
        (True, True)  -> modify $ drawing pos
        (False, True) -> modify stopPlacement
        _             -> return ()
