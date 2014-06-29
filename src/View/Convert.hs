{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module View.Convert where

import Control.Lens
import GameLogic
import Middleware.FreeGame.Facade


type Coord = Double

--TODO: config
drawScale = 8 * 4 :: Coord
cellFontSize = 30 :: Double
panelFontSize = 15 :: Double
panelWidth = 200 :: Coord
playerInfoWidth = panelWidth*0.9
playerInfoHeight = 20 :: Coord
mapSize = 64 :: Coord

windowPosOfWorldPos :: WorldPos -> GameData -> (Coord, Coord)
windowPosOfWorldPos pos game =
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

worldShiftX = - panelWidth / 2 :: Coord

--position of minimap in panel
shiftMiniMap :: Coord -> V2 Coord
shiftMiniMap height = V2 10 (height - 10 - mapSize)
