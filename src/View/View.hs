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

   let centerPos = gCenterPos game
   let firstPlayerColor = playerColor 1
   --TODO: replace centered to last
   let centered = translateCell centerPos $ Color firstPlayerColor $ rectangleWire 26 26

   let worldSize = getWorldSize world
   let shiftX = - (fromIntegral (fst centerPos) - 0.5) * drawScale
   let shiftY = - (fromIntegral (snd centerPos) - 0.5) * drawScale
   let world  = Translate shiftX shiftY $ Pictures $ centered : cells

   return $ Pictures $ [world]


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