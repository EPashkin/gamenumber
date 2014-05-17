module Application.Game.Logic where

import GameLogic.Data.Facade
import GameLogic.Logic
import Middleware.Gloss.Facade
import View.View

eventHandler :: Event -> Game -> IO Game
eventHandler (EventKey key keyState mods pos) game
    | MouseButton LeftButton == key
    , Down                   == keyState
    = return $ doCellAction game $ worldPosOfWindowPos game pos
    | MouseButton RightButton == key
    , Down                    == keyState
    = return $ setCenterPos game $ worldPosOfWindowPos game pos

eventHandler _ game
    = return game
