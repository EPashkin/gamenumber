module View.View
    ( drawState
    ) where

import Control.Lens
import View.State
import View.Panel
import View.Convert
import GameLogic
import Middleware.Gloss.Facade

drawState :: ViewData -> IO Picture
drawState state = do 
  let visibleR = visibleRange state
  world <- drawGame visibleR $ state ^. game
  let world' = Translate worldShiftX 0 world
  let panel = drawPanel state 
  return . Pictures $ world' : [panel]

drawGame :: (WorldPos, WorldPos) -> GameData -> IO Picture
drawGame visibleR game = do
   let cells = mapWR drawCell visibleR $ game ^. world
   let halfScale = drawScale / 2

   let selecteds = if True --TODO: game field :: showEnemySelected
                   then reverse $ drawSelecteds game
                   else [drawSelected game activePlayerIndex]

   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   let worldPicture = Translate shiftX shiftY . Pictures $ cells ++ selecteds

   return $ Pictures [worldPicture]


drawCell :: (WorldPos, Cell) -> Picture
drawCell (pos, cell)
   | isFree cell
      = translateCell pos $ Pictures [ Color emptyCellColor $ rectangleWire 30 30 ]
   | otherwise
      = let rect = rectangleWire 30 30
            color = playerColor $ cell ^. playerIndex
            shiftX = - drawScale / 3
            shiftY = - drawScale / 2.5
            txt = Translate shiftX shiftY . Scale textScale textScale 
                . Text . show $ cell ^. value
      in translateCell pos . Color color . Pictures $ rect : [ txt ]

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

translateCell :: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict

visibleRange :: ViewData -> (WorldPos, WorldPos)
visibleRange state = ((minX, minY), (maxX, maxY))
    where game' = state ^. game
          Just pl = game' ^? players. ix activePlayerIndex
          (cX, cY) = game' ^. centerPos
          wSize = state ^. windowSize
          width = fromIntegral (fst wSize) - panelWidth
          height = fromIntegral (snd wSize)
          dX = floor $ (width / 2 / drawScale) + 0.5
          dY = floor $ (height / 2 / drawScale) + 0.5
          size = getWorldSize $ game' ^. world
          rng = (1, size)
          minX = toRange rng $ cX - dX
          maxX = toRange rng $ cX + dX
          minY = toRange rng $ cY - dY
          maxY = toRange rng $ cY + dY
