module View.View where

import Control.Lens
import View.State
import View.Panel
import View.Convert
import GameLogic.Data.Facade
import Middleware.Gloss.Facade

drawState :: State -> IO Picture
drawState state = do 
  world <- state & drawGame . view game
  let world' = Translate worldShiftX 0 world
  let panel = drawPanel state 
  return . Pictures $ world' : [panel]

drawGame :: Game -> IO Picture
drawGame game = do
   let cells = mapW drawCell $ game ^. world
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

drawSelecteds :: Game -> [Picture]
drawSelecteds game = mapPIndices (drawSelected game) $ game ^. players

drawSelected :: Game -> Int -> Picture
drawSelected game playerIndex =
   let color = playerColor playerIndex
       Just pos = game ^? players . ix playerIndex . selectedPos
   in drawSelectedCellBox pos color

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
