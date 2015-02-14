module View.Convert where

import Control.Lens
import GameLogic

--TODO: config
drawScale = 8 * 4 :: Float
textScale = 8 / drawScale
panelTextScale = 1 / 8 :: Float
panelWidth = 200 :: Float
playerInfoWidth = panelWidth*0.9
playerInfoHeight = 20 :: Float
mapSize = 64 :: Float

type Coord = Float

windowPosOfWorldPos :: GameData -> WorldPos -> (Coord, Coord)
windowPosOfWorldPos game pos =
   let centerPos' = view centerPos game
       xf = (fromIntegral (fst (pos - centerPos')) - 0.5) * drawScale
       yf = (fromIntegral (snd (pos - centerPos')) - 0.5) * drawScale
   in (xf, yf)

worldPosOfWindowPos :: (Coord, Coord) -> GameData -> WorldPos
worldPosOfWindowPos (x, y) game =
   let centerPos' = view centerPos game
       xi = floor (x / drawScale + 0.5) + fst centerPos'
       yi = floor (y / drawScale + 0.5) + snd centerPos'
   in (xi, yi)

--position of minimap in panel
shiftMiniMap :: Coord -> (Coord, Coord)
shiftMiniMap height = (8, 15 - halfHeight)
    where halfHeight = height / 2
