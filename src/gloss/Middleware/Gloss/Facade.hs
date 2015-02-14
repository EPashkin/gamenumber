module Middleware.Gloss.Facade (module G
    , emptyCellColor
    , panelBkColor
    , playerColor
    , gray
    ) where

import Graphics.Gloss.Interface.IO.Game as G

emptyCellColor = makeColorI 100 100 100 255

panelBkColor = makeColorI 212 208 200 255

gray = greyN 0.5

playerColor playerInd = 
    let (r, g, b) = playerColor' playerInd
    in makeColorI (r*4) (g*4) (b*4) 255


playerColor' (-1) = (0, 0,42)        -- possible background color

playerColor' 1 = ( 0,42, 0)
playerColor' 2 = (39,17,25)
playerColor' 3 = (60,24,27)
playerColor' 4 = (60,52,17)
playerColor' 5 = (20,60,10)
playerColor' 6 = (16,24,57)
playerColor' 7 = (31,10,31)
playerColor' 8 = (41, 6, 3)
playerColor' 9 = (36,42,48)
playerColor' 10 = (52,33,25)
playerColor' 11 = (16,50, 8)
playerColor' 12 = (42,34,46)
playerColor' 13 = (21,12,22)
playerColor' 14 = (27,35,58)
playerColor' 15 = (40,30, 1)
playerColor' 16 = (10,52,51)
