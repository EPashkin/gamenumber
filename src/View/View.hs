module View.View where

import GameLogic.Data.Facade
import Middleware.Gloss.Facade

drawGame :: Game -> IO Picture
drawGame game = 
   return $ Pictures [ Color red $ Line [(0,0), (700,700)] ]
