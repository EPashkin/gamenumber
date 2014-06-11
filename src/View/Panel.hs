module View.Panel
    ( drawPanel
    ) where

import Control.Lens
import View.State
import View.Convert
import GameLogic.Data.Facade
import Middleware.Gloss.Facade

--TODO: Pause checkbox
--TODO: Collapsible panel
drawPanel :: State -> Picture
drawPanel state =
    let size = state ^. windowSize
        width = fromIntegral $ fst size
        height = fromIntegral $ snd size
        halfWidth = width / 2
        halfHeight = height / 2
        shiftX = halfWidth - (panelWidth/2)
        rect = Color panelBkColor $ rectangleSolid panelWidth height
        positionPic = Translate (-panelWidth/2.2) (halfHeight - 20) $ drawPosition state
        playerPicts = mapP drawPlayer $ state ^. game . players
        playersPict = Translate 0 (halfHeight - 30) $ Pictures playerPicts
        pausedPict  = Translate 0 (20 - halfHeight) $ drawPaused state
        miniMapPict = Translate (-panelWidth/2.2) (35 - halfHeight)
                      $ drawMiniMap $ state ^. game
    in Translate shiftX 0 $ Pictures [rect, positionPic, playersPict, miniMapPict
                                     , pausedPict]

drawPosition :: State -> Picture
drawPosition state
    = Color black $ Scale panelTextScale panelTextScale $ Text str
    where str = "Position: " ++ show x ++ "x" ++ show y
          Just (x,y) = state ^? game . players . ix activePlayerIndex . selectedPos

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
                  ,(-0.22, show.view free)
                  --,(-0.01, remainText)
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

drawMiniMap :: Game -> Picture
drawMiniMap game = Pictures cells
    where cells = mapW (drawMiniMapCell mapCellScale) w
          w = game ^. world
          mapCellScale = mapSize / fromIntegral (getWorldSize w)

drawMiniMapCell :: Float -> (WorldPos, Cell) -> Picture
drawMiniMapCell mapCellScale (pos, cell)
   | isFree cell
      = translateCell pos $ Color emptyCellColor rect
   | otherwise
      = translateCell pos $ Color color rect
    where translateCell (x,y) pict =
             let xx = (fromIntegral x - 0.5) * mapCellScale
                 yy = (fromIntegral y - 0.5) * mapCellScale
             in Translate xx yy pict
          rect = rectangleSolid mapCellScale mapCellScale
          color = playerColor $ cell ^. playerIndex
