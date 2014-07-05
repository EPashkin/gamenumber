module View.Util
    ( WindowAction
    , WindowGameAction
    , PanelAction
    , placementModeOfGame
    , doWithWindowPos
    , doWithWindowPos2
    ) where

import Control.Lens
import Control.Conditional
import GameLogic
import View.Convert
import View.ViewState


type WindowAction a = (Coord, Coord) -> ViewState a
type WindowGameAction a = (Coord, Coord) -> GameStateF a
type PanelAction a = Coord -> WindowGameAction a

placementModeOfGame :: Lens' ViewData Bool
placementModeOfGame = game . placementMode

doWithWindowPos :: WorldAction -> WindowAction ()
doWithWindowPos = (`doWithWindowPos2` emptyPanelAction)

doWithWindowPos2 :: WorldAction -> PanelAction () -> WindowAction ()
doWithWindowPos2 action panelAction pos@(x, y) = do
    (w, h) <- use windowSize
    let pos' = (x - worldShiftX - w/2, y - h/2)
    doWithWindowPosInPanel panelAction pos 
        <<| inPanel pos |>>
        doWithWindowPosInField action pos'

doWithWindowPosOnGame :: WorldAction -> WindowGameAction ()
doWithWindowPosOnGame action pos
    = framed $ gets (worldPosOfWindowPos pos) >>= action

doWithWindowPosInField :: WorldAction -> WindowAction ()
doWithWindowPosInField action pos
    = zoom game $ doWithWindowPosOnGame action pos

emptyPanelAction :: PanelAction ()
emptyPanelAction _ _ = return ()

doWithWindowPosInPanel :: PanelAction () -> WindowAction ()
doWithWindowPosInPanel panelAction (x, y) = do
    leftX <- panelLeftX
    let x' = x - leftX
    (_, h) <- use windowSize
    zoom game $ panelAction h (x', y)

inPanel :: WindowAction Bool
inPanel (x, _y) = panelLeftX >>= \leftX -> return $ x >= leftX

panelLeftX :: ViewState Coord
panelLeftX = use (windowSize . _1) >>= \w -> return $ w - panelWidth
