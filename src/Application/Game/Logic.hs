module Application.Game.Logic where

import Control.Lens (set, view)
import GameLogic.Data.Facade (Game, leftClickDown)
import GameLogic.Logic (doSelectCellAction, setCenterPosLimited)
import Middleware.Gloss.Facade
import View.View (worldPosOfWindowPos)

eventHandler :: Event -> Game -> IO Game
eventHandler (EventKey key keyState mods pos) game
    | MouseButton LeftButton == key
    , Down                   == keyState
    = return $ set leftClickDown True $ doSelectCellAction pos' game 
    | MouseButton LeftButton == key
    , Up                     == keyState
    = return $ set leftClickDown False game 
    | MouseButton RightButton == key
    , Down                    == keyState
    = return $ setCenterPosLimited pos' game
    where pos' = worldPosOfWindowPos game pos
    
eventHandler (EventMotion pos) game
    | view leftClickDown game
    =return $ doSelectCellAction pos' game 
    where pos' = worldPosOfWindowPos game pos

eventHandler _ game
    = return game
