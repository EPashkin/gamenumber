module View.Panel
    ( drawPanel
    ) where

import Control.Lens
import View.State
import View.Convert
import GameLogic
import Middleware.Gloss.Facade

--TODO: Pause checkbox
--TODO: Collapsible panel
drawPanel :: ViewData -> Picture
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
        pausedPict  = Translate 5 (20 - halfHeight) $ drawPaused state
        speedPict  = Translate (-5) (40 - halfHeight) $ drawGameSpeed state
        miniMapPict = Translate (-panelWidth/2.2) (15 - halfHeight)
                      $ drawMiniMap $ state ^. game
    in Translate shiftX 0 $ Pictures [rect, positionPic, playersPict, miniMapPict
                                     , pausedPict, speedPict]

drawPosition :: ViewData -> Picture
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

--remainText :: Player -> String
--remainText player = show $ (view remain player) `div` remainDivMult

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

drawPaused :: ViewData -> Picture
drawPaused state
    | state ^. game . paused
    = Color black $ drawInfoText "PAUSED"
    | otherwise
    = Blank

drawMiniMap :: GameData -> Picture
drawMiniMap game = Pictures cells
    where cells = mapW (drawMiniMapCell mapCellScale) w
          --swap for testing drawing speed degradation
          --cells = fmap (drawMiniMapCell mapCellScale) [((x,y), mkCell 1 1) | x<-[1..wSize], y<-[1..wSize]]
          w = game ^. world
          wSize = getWorldSize w
          mapCellScale = mapSize / fromIntegral wSize

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

drawGameSpeed :: ViewData -> Picture
drawGameSpeed state = do
    let gs = state ^. game . gameSpeed
        panelFontSize = 15
        gaudgeLeft = panelWidth * 0.065
        gaudgeWidth = panelWidth * 0.3
        gaudgeStep = gaudgeWidth / fromIntegral (fromEnum (maxBound :: GameSpeed))
        gaudgeTop = panelFontSize * 1.5 
        gaudgeHeight = panelFontSize
        gaudgePos sp = gaudgeLeft + gaudgeStep * fromIntegral (fromEnum sp)
        pText = Translate (-10) (gaudgeHeight + panelFontSize) . Color black
            $ drawInfoText "Game speed"
        pHLine = translate 0 (gaudgeHeight / 2) . Color black 
            $ Line [(gaudgeLeft, 0), (gaudgeLeft + gaudgeWidth, 0)]
        pVLines = Color black $ Pictures
            [Line [(x, 0), (x, gaudgeHeight)]
            | sp <- enumFrom (minBound :: GameSpeed)
            , let x = gaudgePos sp]
        pMarker = translate (gaudgePos gs) 0 $ drawGameSpeedMarker gaudgeHeight
    Pictures [pText, pHLine, pVLines, pMarker]

drawGameSpeedMarker :: Float -> Picture
drawGameSpeedMarker gaudgeHeight
    = Color gray $ Polygon [ (0, gaudgeHeight)
                           , (-hw, gaudgeHeight-hw)
                           , (-hw, 0)
                           , (hw, 0)
                           , (hw, gaudgeHeight-hw)
                           ]
    where hw = 5
