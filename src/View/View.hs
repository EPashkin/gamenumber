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
   let halfScale = drawScale / 2
   let temp = Color red $ Pictures
           [ Line [(-halfScale, 0), (halfScale, 0)]
           , Line [(0, -halfScale), (0, halfScale)] ]

   let centerPos = gCenterPos game
   let centered = translateCell centerPos $ Color yellow $ Pictures 
           [ Line [(-halfScale, -halfScale), (halfScale,  halfScale)]
           , Line [(-halfScale,  halfScale), (halfScale, -halfScale)] ]

   let worldSize = getWorldSize world
   let shiftX = - (fromIntegral (fst centerPos) - 0.5) * drawScale
   let shiftY = - (fromIntegral (snd centerPos) - 0.5) * drawScale
   let world  = Translate shiftX shiftY $ Pictures $ centered : cells

   return $ Pictures $ [temp, world]


drawCell :: (WorldPos, Cell) -> Picture
drawCell (pos, cell)
   | isFree cell
      = translateCell pos $ Pictures [ Color emptyCellColor $ rectangleWire 30 30 ]
   | otherwise
      = let rect = rectangleWire 30 30
            color = playerColor $ playerIndex cell
            shiftX = - drawScale / 3
            shiftY = - drawScale / 2.5
            txt = Translate shiftX shiftY $ Scale textScale textScale $ Text $ show $ getCellValue cell
      in translateCell pos $ Color color $ Pictures $ rect : [ txt ]

translateCell :: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict

worldPosOfWindowPos :: Game -> (Float, Float) -> WorldPos
worldPosOfWindowPos game (x, y) =
   let centerPos = gCenterPos game
       xi = floor (x / drawScale + 0.5) + fst centerPos :: Int
       yi = floor (y / drawScale + 0.5) + snd centerPos :: Int
   in (xi, yi)