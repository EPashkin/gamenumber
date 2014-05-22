module View.View where

import Control.Lens
import View.Convert
import GameLogic.Data.Facade
import Middleware.Gloss.Facade


drawGame :: Game -> IO Picture
drawGame game = do
   let cells = mapW drawCell $ game ^. world
   let halfScale = drawScale / 2

   let centerPos' = game ^. centerPos
   let firstPlayerColor = playerColor activePlayerIndex
   let selected = drawSelectedCellBox (game ^. selectedPos) firstPlayerColor

   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   let worldPicts = Translate shiftX shiftY $ Pictures $ selected : cells

   return $ Pictures [worldPicts]


drawCell :: (WorldPos, Cell) -> Picture
drawCell (pos, cell)
   | isFree cell
      = translateCell pos $ Pictures [ Color emptyCellColor $ rectangleWire 30 30 ]
   | otherwise
      = let rect = rectangleWire 30 30
            color = playerColor $ cell ^. playerIndex
            shiftX = - drawScale / 3
            shiftY = - drawScale / 2.5
            txt = Translate shiftX shiftY $ Scale textScale textScale 
                $ Text $ show $ cell ^. value
      in translateCell pos $ Color color $ Pictures $ rect : [ txt ]

drawSelectedCellBox pos color =
    translateCell pos $ Color color $ Pictures [
        line [(-radius, 0), (delta-radius, 0)],
        line [(radius, 0), (radius-delta, 0)],
        rectangleWire diametr diametr
    ] where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4 

translateCell :: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict
