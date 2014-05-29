module View.Panel
    ( drawPanel
    ) where

import Control.Lens
import View.State
import View.Convert
import GameLogic.Data.Facade
import Middleware.Gloss.Facade

--TODO: Show position
--TODO: Show minimap
--TODO: Pause checbox
--TODO: Colapsible panel
drawPanel :: State -> Picture
drawPanel state =
    let size = state ^. windowSize
        width = fromIntegral $ fst size
        height = fromIntegral $ snd size
        halfWidth = width / 2
        halfHeight = height / 2
        shiftX = halfWidth - (panelWidth/2)
        rect = Color panelBkColor $ rectangleSolid panelWidth height
        playerPicts = mapP drawPlayer $ state ^. game . players
        playersPict = Translate 0 (halfHeight - 30) $ Pictures playerPicts
        pausedPict  = Translate 0 (20 - halfHeight) $ drawPaused state
    in Translate shiftX 0 $ Pictures [rect, playersPict, pausedPict]

drawPlayer :: (Int, Player) -> Picture
drawPlayer (index, player)
    = Translate 0 shiftY . Color color . Pictures $ drawPlayerInfoParts player
    where color = playerColor index
          shiftY = (0.5 - fromIntegral index) * playerInfoHeight

drawPlayerInfoParts :: Player -> [Picture]
drawPlayerInfoParts player = fmap p playerInfoParts
    where p (shift, p) = Translate (playerInfoWidth*shift) 0 . drawInfoText $ p player

playerInfoParts :: [(Float, Player -> String)]
playerInfoParts = [(-0.50, show.view num)
                  ,(-0.23, show.view free)
                  ,(-0.01, remainText)
                  ,( 0.22, shieldText)
                  ,( 0.40, aggrText)
                  ]

drawInfoText :: String -> Picture
drawInfoText = Translate 0 (-playerInfoHeight/2)
               . Scale panelTextScale panelTextScale . Text

remainText :: Player -> String
remainText player = show $ view remain player `div` remainDivMult

shieldText :: Player -> String
shieldText player
    | strength < 128
    = show strength
    | active
    = "+1"
    | otherwise 
    = "+0"
    where strength = player ^. shieldStrength
          active = player ^. shieldActive

aggrText :: Player -> String
aggrText player
    | aggro > 0
    = show aggro
    | otherwise 
    = ""
    where aggro = player ^. aggr

drawPaused :: State -> Picture
drawPaused state
    | state ^. game . paused
    = Color black $ drawInfoText "PAUSED"
    | otherwise
    = Blank
