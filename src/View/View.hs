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
  return $ Pictures $ world' : [panel]

drawGame :: Game -> IO Picture
drawGame game = do
   let cells = mapW drawCell $ game ^. world
   let halfScale = drawScale / 2

   let centerPos' = game ^. centerPos
   let firstPlayerColor = playerColor activePlayerIndex
   let selected = drawSelectedCellBox (game ^. selectedPos) firstPlayerColor

   let (shiftX, shiftY) = windowPosOfWorldPos game startWorldPos
   let worldPicture = Translate shiftX shiftY $ Pictures $ selected : cells

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
            txt = Translate shiftX shiftY $ Scale textScale textScale 
                $ Text $ show $ cell ^. value
      in translateCell pos $ Color color $ Pictures $ rect : [ txt ]

drawSelectedCellBox pos color =
    translateCell pos $ Color color $ Pictures [
        line [(-radius, 0), (delta-radius, 0)],
        line [(radius, 0), (radius-delta, 0)],
        rectangleWire diametr diametr
    ] where
        diametr = 26
        radius = diametr / 2
        delta = radius / 4

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
drawPlayer (index, player) =
    let infoWidth = panelWidth*0.9
        textNum = Translate (infoWidth*textNumShift) 0 $ drawInfoText $ player ^. num
        textFree = Translate (infoWidth*textFreeShift) 0 $ drawInfoText $ player ^. free
        textRemain = Translate (infoWidth*textRemainShift) 0 $ drawInfoText $ player ^. remain
        textShield = Translate (infoWidth*textShieldShift) 0 $ drawInfoText $ shieldText player
        textAggr = Translate (infoWidth*textAggrShift) 0 $ drawInfoText $ aggrText player
        color = playerColor index
        texts = Color color $ Pictures [textNum, textFree, textRemain, textShield, textAggr]
        shiftY = playerInfoHeight/2 - fromIntegral index * playerInfoHeight
    in Translate 0 shiftY texts

drawInfoText :: Show a => a -> Picture
drawInfoText = Translate 0 (-playerInfoHeight/2) . Scale panelTextScale panelTextScale
    . Text . show

newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

shieldText :: Player -> PlainString
shieldText player
    | strength < 128
    = PlainString $ show strength
    | active
    = PlainString "+1"
    | otherwise 
    = PlainString "+0"
    where strength = player ^. shieldStrength
          active = player ^. shieldActive

aggrText :: Player -> PlainString
aggrText player
    | aggro > 0
    = PlainString $ show aggro
    | otherwise 
    = PlainString ""
    where aggro = player ^. aggr
    
translateCell :: WorldPos -> Picture -> Picture
translateCell (x,y) pict =
   let xx = (fromIntegral x - 0.5) * drawScale
       yy = (fromIntegral y - 0.5) * drawScale
   in Translate xx yy pict
