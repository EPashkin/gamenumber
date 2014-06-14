module Application.Game.Logic where

import Control.Lens
import Middleware.FreeGame.Facade
import Middleware.FreeGame.GameState
import Control.Monad.State.Lazy
import View.State


eventHandler :: GameState
eventHandler = do
    updateWindowSize
    overGameState incCounter
    onKeyA
    onKeyB
    
    mouseEvents
    whenGameState (keyDown KeyP) $ overGameState doChangePaused
    whenGameState (keyDown KeyS) $ overGameState doShieldAction
    whenGameState (keyDown KeyF1) $ overGameState doHelpPlayer
    whenGameState (keyDown KeyF2) $ overIOGameState doSave
    whenGameState (keyDown KeyF3) $ overIOGameState doLoad

updateWindowSize :: GameState
updateWindowSize = do
    Box _ (V2 w h) <- getBoundingBox
    overGameState $ windowSize .~ (w, h)

incCounter100 :: StateData -> StateData
incCounter100 = counter +~ 100

incCounter :: StateData -> StateData
incCounter = counter +~ 1

onKeyA :: GameState
onKeyA = do
    whenM (keyDown KeyA) $ color green $ polygon [V2 20 0, V2 100 20, V2 90 60, V2 30 70]
    get

onKeyB :: GameState
onKeyB = do
    whenGameState (keyDown KeyB) $ overGameState incCounter100
    color green $ polygon [V2 100 0, V2 100 20, V2 90 60, V2 30 70]
    get

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
