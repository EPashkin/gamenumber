module Application.Game.Logic where

import Middleware.Gloss.Facade
import View.State

eventHandler :: Event -> State -> IO State
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

eventHandler (EventKey (SpecialKey specKey) keyState mods pos) state
    | KeyF2 == specKey
    , Down == keyState
    = doSave state
    | KeyF3 == specKey
    , Down == keyState
    = doLoad state

eventHandler (EventMotion pos) state
    | inPlacementMode state 
    = return $ drawing pos state
    | otherwise
    = return state

eventHandler (EventResize size) state
    = return $ updateWindowSize size state

eventHandler _ state
    = return state
