module View.Convert where

import Control.Lens
import GameLogic.Data.Facade


type Coord = Double

--TODO: config
drawScale = 8 * 4 :: Coord
textScale = 8 / drawScale
cellFontSize = 30 :: Double
panelFontSize = 15 :: Double
panelTextScale = 1 / 8 :: Coord
panelWidth = 200 :: Coord
playerInfoWidth = panelWidth*0.9
playerInfoHeight = 20 :: Coord
mapSize = 64 :: Coord

windowPosOfWorldPos :: GameData -> WorldPos -> (Coord, Coord)
windowPosOfWorldPos game pos =
   let centerPos' = view centerPos game
       xf = (fromIntegral (fst (pos - centerPos')) - 0.5) * drawScale
       yf = (fromIntegral (snd (pos - centerPos')) - 0.5) * drawScale
   in (xf, yf)

worldPosOfWindowPos :: GameData -> (Coord, Coord) -> WorldPos
worldPosOfWindowPos game (x, y) =
   let centerPos' = view centerPos game
       xi = floor (x / drawScale + 0.5) + fst centerPos'
       yi = floor (y / drawScale + 0.5) + snd centerPos'
   in (xi, yi)
