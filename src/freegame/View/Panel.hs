module View.Panel
    ( drawPanel
    ) where

import Control.Lens hiding (index)
import View.ViewState
import View.Convert
import GameLogic
import Middleware.FreeGame.Facade

--TODO: Pause checkbox
--TODO: Collapsible panel
drawPanel :: ViewState ()
drawPanel = do
    state <- get
    let (_width, height) = state ^. windowSize
        fnt = state ^. font
        g = state ^. game
        activePlayerPosition = g ^. playerOfGame activePlayerIndex . selectedPos
        ownerPlayerInd = g ^. cellOfGame activePlayerPosition . playerIndex
    lift $ do
      color panelBkColor $ rectangleSolid panelWidth height
      translate (V2 10 20) $ drawPosition state
      fps <- getFPS
      let fpsText = "FPS:" <> show fps
      translate (V2 (panelWidth - 60) 20) . color black $ text fnt 15 fpsText
      translate (V2 0 30) . drawPlayers fnt ownerPlayerInd $ g ^. players
      translate (shiftMiniMap height) $ drawMiniMap g
      translate (V2 110 (height - 10)) $ drawPaused state
      translate (V2 95 (height - 70)) $ drawGameSpeed state

drawPosition :: ViewData -> Frame ()
drawPosition state
    = color black $ text (state ^. font) panelFontSize str
    where str = "Position: " <> show x <> "x" <> show y
          (x,y) = state ^. game . playerOfGame activePlayerIndex . selectedPos

drawPlayers :: Font -> Int -> Players -> Frame()
drawPlayers fnt ownerPlayerInd
   = (`evalStateT` 1) . sequence_ . mapP (drawPlayer fnt ownerPlayerInd)

drawPlayer :: Font -> Int -> (Int, Player) -> StateT Int Frame ()
drawPlayer fnt ownerPlayerInd (index, player)
    | not ( isAlive player)
    = return ()
    | otherwise
    = do
    ind <- get
    let shiftY = fromIntegral ind * playerInfoHeight
        clr = playerColor index
    translate (V2 (panelWidth/2) shiftY) . color clr
        . lift . drawPlayerInfoParts fnt $ player
    when (ownerPlayerInd == index)
        . translate (V2 0 shiftY) . color clr $ lift drawOwnerFlag
    put (ind+1)

drawOwnerFlag :: Frame ()
drawOwnerFlag = line [V2 0 (-5), V2 10 (-5)] >> line [V2 5 0, V2 5 (-10)]

drawPlayerInfoParts :: Font -> Player -> Frame ()
drawPlayerInfoParts fnt player = mapM_ p playerInfoParts
    where p (shift, f) = translate (V2 (playerInfoWidth*shift) 0)
                             . text fnt panelFontSize $ f player

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
    | isAI player
    = show $ player ^. aggr
    | otherwise 
    = ""

drawPaused :: ViewData -> Frame ()
drawPaused state
    = when (state ^. game . paused)
           . color black $ text (state ^. font) panelFontSize "PAUSED"

drawMiniMap :: GameData -> Frame ()
drawMiniMap game' = draw $ sequence_ cells
    where cells :: Drawable f => [f ()]
          cells = mapW (drawMiniMapCell mapCellScale) w 
          --swap for testing drawing speed degradation
          --cells = fmap (drawMiniMapCell mapCellScale) [((x,y), mkCell 1 1) | x<-[1..wSize], y<-[1..wSize]]
          w = game' ^. world
          wSize = getWorldSize w
          mapCellScale = mapSize / fromIntegral wSize

drawMiniMapCell :: Drawable f => Double -> (WorldPos, Cell) -> f ()
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

drawGameSpeed :: ViewData -> Frame ()
drawGameSpeed state = do
    let gs = state ^. game . gameSpeed
        gaudgeLeft = panelWidth * 0.065
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
