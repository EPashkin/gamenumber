module Application.Game.Logic where

import Middleware.FreeGame.Facade
import View.State

{-
eventHandler :: Event -> StateData -> IO State
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
-}
