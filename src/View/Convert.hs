module View.Convert where

import Control.Lens
import GameLogic.Data.Facade

--TODO: config
drawScale = 8 * 4 :: Float
textScale = 8 / drawScale
panelTextScale = 1 / 8 :: Float
panelWidth = 200 :: Float
playerInfoWidth = panelWidth*0.9
playerInfoHeight = 20 :: Float
mapSize = 64 :: Float

windowPosOfWorldPos :: Game -> WorldPos -> (Float, Float)
windowPosOfWorldPos game pos =
   let centerPos' = view centerPos game
       xf = (fromIntegral (fst (pos - centerPos')) - 0.5) * drawScale
       yf = (fromIntegral (snd (pos - centerPos')) - 0.5) * drawScale
   in (xf, yf)

worldPosOfWindowPos :: Game -> (Float, Float) -> WorldPos
worldPosOfWindowPos game (x, y) =
   let centerPos' = view centerPos game
       xi = floor (x / drawScale + 0.5) + fst centerPos'
       yi = floor (y / drawScale + 0.5) + snd centerPos'
   in (xi, yi)
