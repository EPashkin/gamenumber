module View.View where

import Control.Lens
import View.State
import View.Convert
import GameLogic.Data.Facade
import Middleware.Gloss.Facade

drawState :: State -> IO Picture
drawState state = do 
  world <- state & drawGame . view game
  let world' = Translate worldShiftX 0 world
  let panel = drawPanel state 
  return . Pictures $ world' : [panel]

drawGame :: Game -> IO Picture
drawGame game = do
   let cells = mapW drawCell $ game ^. world
   let halfScale = drawScale / 2

   let centerPos' = game ^. centerPos
   let firstPlayerColor = playerColor activePlayerIndex
   let selected = drawSelectedCellBox (game ^. selectedPos) firstPlayerColor

   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   let worldPicture = Translate shiftX shiftY . Pictures $ selected : cells

   return $ Pictures [worldPicture]


drawCell :: (WorldPos, Cell) -> Picture
drawCell (pos, cell)
   | isFree cell
      = translateCell pos $ Pictures [ Color emptyCellColor $ rectangleWire 30 30 ]
   | otherwise
      = let rect = rectangleWire 30 30
            color = playerColor $ cell ^. playerIndex
            shiftX = - drawScale / 3
            shiftY = - drawScale / 2.5
            txt = Translate shiftX shiftY . Scale textScale textScale 
                . Text . show $ cell ^. value
      in translateCell pos . Color color . Pictures $ rect : [ txt ]

drawSelectedCellBox pos color =
    translateCell pos . Color color $ Pictures [
        line [(-radius, 0), (delta-radius, 0)],
        line [(radius, 0), (radius-delta, 0)],
        rectangleWire diametr diametr
    ] where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4

--TODO: move to ViewPanel.hs
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
    in Translate shiftX 0 $ Pictures [rect, playersPict]

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
                  ,(-0.01, show.view remain)
                  ,( 0.22, shieldText)
                  ,( 0.40, aggrText)
                  ]

drawInfoText :: String -> Picture
drawInfoText = Translate 0 (-playerInfoHeight/2)
               . Scale panelTextScale panelTextScale . Text

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
    
translateCell :: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict
