module Application.Game.Logic where

import GameLogic.Data.Facade
import Middleware.Gloss.Facade

eventHandler :: Event -> Game -> IO Game
eventHandler (EventKey key keyState mods pos) game
	| MouseButton LeftButton	<- key
	, Down				<- keyState
	= return game

eventHandler _ game
	= return game
