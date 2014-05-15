module View.View where

import GameLogic.Data.Facade
import Middleware.Gloss.Facade

--TODO: config
drawScale = 8 * 4 :: Float
textScale = 8 / drawScale

drawGame :: Game -> IO Picture
drawGame game = do
   let world = getWorld game
   let cells = mapW drawCell world
   let temp = Color red $ Line [(0,0), (700,700)]
   let list  = temp : cells

   return $ Pictures list


drawCell :: (WorldPos, Cell) -> Picture
drawCell (pos, cell)
   | isFree cell
      = translateCell pos $ Pictures [ Color emptyCellColor $ rectangleWire 30 30 ]
   | otherwise
      = let rect = rectangleWire 30 30
            shiftX = - drawScale / 3
            shiftY = - drawScale / 2.5
            txt = Translate shiftX shiftY $ Scale textScale textScale $ Text $ show $ getCellValue cell
      in translateCell pos $ Color green $ Pictures $ rect : [ txt ]

translateCell:: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict
