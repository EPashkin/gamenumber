module View.Panel
    ( drawPanel
    ) where

import Control.Lens
import View.State
import View.Convert
import GameLogic.Data.Facade
import Middleware.FreeGame.Facade

--TODO: Pause checkbox
--TODO: Collapsible panel
--TODO: Show owned player of selected cell
--TODO: Hide dead player info
--TODO: Center by minimap
--TODO: Move game speed down
drawPanel :: StateData -> Frame ()
drawPanel state = do 
    let (width, height) = state ^. windowSize
        fnt = state ^. font
        g = state ^. game
    color panelBkColor $ rectangleSolid panelWidth height
    translate (V2 10 20) $ drawPosition state
    fps <- getFPS
    let fpsText = "FPS:" ++ show fps
    translate (V2 (panelWidth - 60) 20) . color black $ text fnt 15 fpsText
    translate (V2 0 30) . sequence_ . mapP (drawPlayer fnt) $ g ^. players
    translate (V2 10 (height - 10 - mapSize)) $ drawMiniMap g
    translate (V2 (panelWidth/2) (height - 10)) $ drawPaused state
    translate (V2 (panelWidth/2) (height - 100)) $ drawGameSpeed state

drawPosition :: StateData -> Frame ()
drawPosition state
    = color black $ text (state ^. font) panelFontSize str
    where str = "Position: " ++ show x ++ "x" ++ show y
          Just (x,y) = state ^? game . players . ix activePlayerIndex . selectedPos

drawPlayer :: Font -> (Int, Player) -> Frame ()
drawPlayer font (index, player)
    = translate (V2 (panelWidth/2) shiftY) . color clr . drawPlayerInfoParts font $ player
    where clr = playerColor index
          shiftY = fromIntegral index * playerInfoHeight

drawPlayerInfoParts :: Font -> Player -> Frame ()
drawPlayerInfoParts fnt player = mapM_ p playerInfoParts
    where p (shift, p) = translate (V2 (playerInfoWidth*shift) 0)
                             . text fnt panelFontSize $ p player

playerInfoParts :: [(Double, Player -> String)]
playerInfoParts = [(-0.50, show.view num)
                  ,(-0.22, show.view free)
                  --,(-0.01, remainText)
                  ,( 0.22, shieldText)
                  ,( 0.40, aggrText)
                  ]

--remainText :: Player -> String
--remainText player = show $ view remain player `div` remainDivMult

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

drawPaused :: StateData -> Frame ()
drawPaused state
    = when (state ^. game . paused)
           . color black $ text (state ^. font) panelFontSize "PAUSED"

drawMiniMap :: GameData -> Frame ()
drawMiniMap game = sequence_ cells
    where cells = mapW (drawMiniMapCell mapCellScale) w
          --swap for testing drawing speed degradation
          --cells = fmap (drawMiniMapCell mapCellScale) [((x,y), mkCell 1 1) | x<-[1..wSize], y<-[1..wSize]]
          w = game ^. world
          wSize = getWorldSize w
          mapCellScale = mapSize / fromIntegral wSize

drawMiniMapCell :: Double -> (WorldPos, Cell) -> Frame ()
drawMiniMapCell mapCellScale (pos, cell)
   | isFree cell
      = translateCell pos $ color emptyCellColor rect
   | otherwise
      = translateCell pos $ color clr rect
    where translateCell (x,y) pict =
             let xx = (fromIntegral x - 1) * mapCellScale
                 yy = (fromIntegral y - 1) * mapCellScale
             in translate (V2 xx yy) pict
          rect = rectangleSolid mapCellScale mapCellScale
          clr = playerColor $ cell ^. playerIndex

drawGameSpeed :: StateData -> Frame ()
drawGameSpeed state = do
    let gs = state ^. game . gameSpeed
        gaudgeLeft = panelWidth * 0.05
        gaudgeWidth = panelWidth * 0.3
        gaudgeStep = gaudgeWidth / fromIntegral (fromEnum (maxBound :: GameSpeed))
        gaudgeTop = panelFontSize * 1.5 
        gaudgeHeight = panelFontSize
        gaudgePos sp = gaudgeLeft + gaudgeStep * fromIntegral (fromEnum sp)
    translate (V2 0 panelFontSize) . color black
        $ text (state ^. font) panelFontSize "Game speed"
    translate (V2 0 (panelFontSize * 2)) . color black 
        $ line [V2 gaudgeLeft 0, V2 (gaudgeLeft + gaudgeWidth) 0]
    translate (V2 0 gaudgeTop) . color black $ sequence_
        [line [V2 x 0, V2 x gaudgeHeight]
        | sp <- enumFrom (minBound :: GameSpeed)
        , let x = gaudgePos sp]
    translate (V2 (gaudgePos gs) gaudgeTop) $ drawGameSpeedMarker gaudgeHeight

drawGameSpeedMarker :: Coord -> Frame ()
drawGameSpeedMarker gaudgeHeight
    = color gray $ polygon [ V2 0 0
                           , V2 hw hw
                           , V2 hw gaudgeHeight
                           , V2 (-hw) gaudgeHeight
                           , V2 (-hw) hw
                           ]
    where hw = 5
