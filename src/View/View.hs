module View.View
    ( drawState
    ) where

import Control.Lens
import View.State
import View.Panel
import View.Convert
import GameLogic.Data.Facade
import GameLogic.Util
import Middleware.FreeGame.Facade

drawState :: StateData -> Frame ()
drawState state = do
    let fnt = view font state
    let (w,h) = state ^. windowSize
    let worldShift = V2 (w/2 + worldShiftX) (h/2)
        shiftX = w - panelWidth

    let cnt = view counter state
    translate (V2 240 300) . color green . text fnt 15 $ show cnt
    translate (V2 240 480) . rotateD (fromIntegral cnt) . color red $ text fnt 70 "Test"

    let visibleR = visibleRange state
    translate worldShift . drawGame fnt visibleR $ state ^. game
    translate (V2 shiftX 0) $ drawPanel state
--    let panel = drawPanel state 
--  return . Pictures $ world' : [panel]

drawGame :: Font -> (WorldPos, WorldPos) -> GameData -> Frame ()
drawGame fnt visibleR game = do
   let cells = mapWR (drawCell fnt) visibleR $ game ^. world
   let halfScale = drawScale / 2
   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   translate (V2 shiftX shiftY) $ sequence_ cells
{-
   let selecteds = if True --TODO: game field :: showEnemySelected
                   then reverse $ drawSelecteds game
                   else [drawSelected game activePlayerIndex]

   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   let worldPicture = Translate shiftX shiftY . Pictures $ cells ++ selecteds

   return $ Pictures [worldPicture]
-}

drawCell :: Font -> (WorldPos, Cell) -> Frame()
drawCell fnt (pos, cell)
   | isFree cell
      = translateCell pos . color emptyCellColor $ rectangleWire 30 30
   | otherwise
      = let rect = rectangleWire 30 30
            clr = playerColor $ cell ^. playerIndex
            shiftX = - drawScale / 3.2
            shiftY = drawScale / 2.8
            txt = translate (V2 shiftX shiftY) 
                   . text fnt cellFontSize . show $ cell ^. value
      in translateCell pos . color clr $ rect >> txt

{-
drawSelecteds :: GameData -> [Picture]
drawSelecteds game = mapPIndices (drawSelected game) $ game ^. players

drawSelected :: GameData -> Int -> Picture
drawSelected game playerIndex
   | num' > 0
   || playerIndex == activePlayerIndex
   = drawSelectedCellBox pos color
   | otherwise
   = Blank
   where color = playerColor playerIndex
         Just pl = game ^? players . ix playerIndex
         pos = pl ^. selectedPos
         num' = pl ^. num

drawSelectedCellBox :: WorldPos -> Color -> Picture
drawSelectedCellBox pos color =
    translateCell pos . Color color $ Pictures [
        line [(-radius, 0), (delta-radius, 0)],
        line [(radius, 0), (radius-delta, 0)],
        rectangleWire diametr diametr
    ] where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4
-}
translateCell :: WorldPos -> Frame () -> Frame ()
translateCell (x,y) =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in translate (V2 xx yy)

visibleRange :: StateData -> (WorldPos, WorldPos)
visibleRange state = ((minX, minY), (maxX, maxY))
    where game' = state ^. game
          Just pl = game' ^? players. ix activePlayerIndex
          (cX, cY) = game' ^. centerPos
          (width, height) = state ^. windowSize
          dX = floor $ ((width - panelWidth ) / 2 / drawScale) + 0.5
          dY = floor $ (height / 2 / drawScale) + 0.5
          size = getWorldSize $ game' ^. world
          rng = (1, size)
          minX = toRange rng $ cX - dX
          maxX = toRange rng $ cX + dX
          minY = toRange rng $ cY - dY
          maxY = toRange rng $ cY + dY
