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

    let visibleR = visibleRange state
    translate worldShift . drawGame fnt visibleR $ state ^. game
    translate (V2 shiftX 0) $ drawPanel state

drawGame :: Font -> (WorldPos, WorldPos) -> GameData -> Frame ()
drawGame fnt visibleR game = do
   let cells = mapWR (drawCell fnt) visibleR $ game ^. world
   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   translate (V2 shiftX shiftY) $ sequence_ cells

   translate (V2 shiftX shiftY) $ if True --TODO: game field :: showEnemySelected
   then drawSelecteds game
   else drawSelected game activePlayerIndex

drawCell :: Font -> (WorldPos, Cell) -> Frame ()
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

drawSelecteds :: GameData -> Frame ()
drawSelecteds game = sequence_ . mapPIndices (drawSelected game) $ game ^. players

drawSelected :: GameData -> Int -> Frame ()
drawSelected game playerIndex
   = when (num' > 0 || playerIndex == activePlayerIndex)
       $ drawSelectedCellBox pos color
   where color = playerColor playerIndex
         Just pl = game ^? players . ix playerIndex
         pos = pl ^. selectedPos
         num' = pl ^. num

--TODO: better selected box
drawSelectedCellBox :: WorldPos -> Color -> Frame ()
drawSelectedCellBox pos clr =
    translateCell pos . color clr $ do
        line [V2 (-radius) 0, V2 (delta - radius) 0]
        line [V2 radius 0, V2 (radius - delta) 0]
        rectangleWire diametr diametr
    where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4

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
