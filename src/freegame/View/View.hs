module View.View
    ( drawState
    ) where

import Control.Lens
import View.ViewState
import View.Panel
import View.Convert
import GameLogic
import Middleware.FreeGame.Facade

drawState :: ViewState ()
drawState = do
    fnt <- use font
    (w,h) <- use windowSize
    let worldShift = V2 (w/2 + worldShiftX) (h/2)
        shiftX = w - panelWidth

    visibleR <- gets visibleRange
    zoom game . translate worldShift $ drawGame fnt visibleR
    translate (V2 shiftX 0) drawPanel

drawGame :: Font -> (WorldPos, WorldPos) -> GameStateF ()
drawGame fnt visibleR = do
   cells <- mapWR (drawCell fnt) visibleR <$> use world
   (shiftX, shiftY) <- gets $ windowPosOfWorldPos startWorldPos
   lift . translate (V2 shiftX shiftY) $ sequence_ cells

   translate (V2 shiftX shiftY) $ if True --TODO: game field :: showEnemySelected
   then drawSelecteds
   else drawSelected activePlayerIndex

drawCell :: Font -> (WorldPos, Cell) -> Frame ()
drawCell fnt (pos, cell)
   | isFree cell
   = translateCell pos . color emptyCellColor $ rect
   | otherwise
   = translateCell pos . color clr $ rect >> txt
   where rect = rectangleWire 30 30
         txt = translate (V2 shiftX shiftY)
               . text fnt cellFontSize . show $ cell ^. value
         clr = playerColor $ cell ^. playerIndex
         shiftX = drawScale * 0.17
         shiftY = drawScale * 0.80

drawSelecteds :: GameStateF ()
drawSelecteds = use players >>= sequence_ . reverse . mapPIndices drawSelected

drawSelected :: Int -> GameStateF ()
drawSelected playerInd = do
   Just pl <- gets . preview $ playerOfGame playerInd
   let clr = playerColor playerInd
       pos = pl ^. selectedPos
       num' = pl ^. num
   when (num' > 0 || playerInd == activePlayerIndex)
       . lift $ drawSelectedCellBox pos clr

drawSelectedCellBox :: WorldPos -> Color Float -> Frame ()
drawSelectedCellBox pos clr =
    draw $ translateCell pos . translate (V2 start start)
        . thickness 3 . color clr $ do
        line [V2 0 radius, V2 delta radius]
        line [V2 diametr radius, V2 (diametr - delta) radius]
        rectangleWire diametr diametr
    where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4
        start = 2

translateCell :: Affine f => WorldPos -> f () -> f ()
translateCell (x,y) =
   let xx = (fromIntegral x - 1) * drawScale
       yy = (fromIntegral y - 1) * drawScale
   in translate (V2 xx yy)

visibleRange :: ViewData -> (WorldPos, WorldPos)
visibleRange state = ((minX, minY), (maxX, maxY))
    where game' = state ^. game
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
