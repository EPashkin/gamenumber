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
    
eventHandler (EventMotion pos) state
    | inPlacementMode state 
    = return $ drawing pos state
    | otherwise
    = return state

eventHandler _ state
    = return state
