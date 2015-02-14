module Application.Game.Logic where

import Middleware.Gloss.Facade
import View.State

eventHandler :: Event -> ViewData -> IO ViewData
eventHandler (EventKey key keyState mods pos) state
    | MouseButton LeftButton == key
    , Down                   == keyState
    = return $ startPlacement pos state 
    | MouseButton LeftButton == key
    , Up                     == keyState
    = return $ stopPlacement state 
    | MouseButton RightButton == key
    , Down                    == keyState
    = return $ centering pos state

eventHandler (EventKey key keyState mods pos) state
    | SpecialKey KeyF1 == key
    , Down == keyState
    = return $ doHelpPlayer state
    | SpecialKey KeyF2 == key
    , Down == keyState
    = doSave state
    | SpecialKey KeyF3 == key
    , Down == keyState
    = doLoad state
    | SpecialKey KeyPadAdd == key || Char '=' == key
    , Down == keyState
    = return $ increaseSpeed state
    | SpecialKey KeyPadSubtract == key || Char '-' == key
    , Down == keyState
    = return $ decreaseSpeed state
    | Char 'p' == key
    , Down == keyState
    = return $ doChangePaused state
    | Char 's' == key
    , Down == keyState
    = return $ doShieldAction state

eventHandler (EventMotion pos) state
    | inPlacementMode state 
    = return $ drawing pos state
    | otherwise
    = return state

eventHandler (EventResize size) state
    = return $ updateWindowSize size state

eventHandler _ state
    = return state
