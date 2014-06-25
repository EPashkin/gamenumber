module View.Logic where

import Data.Maybe
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic
import View.Convert
import View.ViewState


type WindowAction a = (Coord, Coord) -> ViewState a
type WindowGameAction a = (Coord, Coord) -> GameState a
type PanelAction a = Coord -> WindowGameAction a


runGameStep :: ViewState ()
runGameStep = game %= doGameStep

startPlacement :: WindowAction ()
startPlacement pos = placementModeOfGame .= True
    >> doWithWindowPos doSelectCellAction pos

stopPlacement :: ViewState ()
stopPlacement = placementModeOfGame .= False

inPlacementMode :: ViewState Bool
inPlacementMode = use placementModeOfGame

placementModeOfGame :: Lens' ViewData Bool
placementModeOfGame = game . placementMode

centering :: WindowAction ()
centering = doWithWindowPos2 setCenterPosLimited setCenterPosByMiniMap

setCenterPosByMiniMap :: PanelAction ()
setCenterPosByMiniMap height (x,y)
    = setCenterPosOnMiniMap (x', y')
    where V2 x' y' = V2 x y - shiftMiniMap height

setCenterPosOnMiniMap :: WindowGameAction ()
setCenterPosOnMiniMap (x,y) = modify $ setCenterPos (floor x, floor y)

drawing :: WindowAction ()
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: WindowAction ()
updateWindowSize = (.=) windowSize

doSave :: ViewState ()
doSave = zoom game $ get >>= liftIO . doSaveGame "gamenumber.gn"

doLoad :: ViewState ()
doLoad = zoom game $ get >>= liftIO . doLoadGame "gamenumber.gn" >>= put

doHelpPlayer :: ViewState ()
doHelpPlayer = game %= p
    where p g = fromMaybe g $ decreaseGamePlayerFree activePlayerIndex (-10, g)

doChangePaused :: ViewState ()
doChangePaused = game . paused %= not

doShieldAction :: ViewState ()
doShieldAction = game %= shieldAction activePlayerIndex

increaseSpeed :: ViewState ()
increaseSpeed = game . gameSpeed %= succ'
    where succ' gs = if gs == maxBound then gs
                     else succ gs

decreaseSpeed :: ViewState ()
decreaseSpeed = game . gameSpeed %= pred'
    where pred' gs = if gs == minBound then gs
                     else pred gs

doWithWindowPosOnGame :: WorldAction -> WindowGameAction ()
doWithWindowPosOnGame action pos
    = get >>= modify . action . flip worldPosOfWindowPos pos

doWithWindowPosInField :: WorldAction -> WindowAction ()
doWithWindowPosInField action pos
    = zoom game $ doWithWindowPosOnGame action pos

emptyPanelAction :: PanelAction ()
emptyPanelAction _ _ = return ()

doWithWindowPosInPanel :: PanelAction () -> WindowAction ()
doWithWindowPosInPanel panelAction (x,y) = do
    leftX <- panelLeftX
    let x' = x - leftX
    (_,h) <- use windowSize
    zoom game $ panelAction h (x', y)

doWithWindowPos :: WorldAction -> WindowAction ()
doWithWindowPos = (`doWithWindowPos2` emptyPanelAction)

doWithWindowPos2 :: WorldAction -> PanelAction () -> WindowAction ()
doWithWindowPos2 action panelAction pos@(x, y) = do
    state <- get
    let (w,h) = state ^. windowSize
        pos' = (x - worldShiftX - w/2, y - h/2)
    b <- inPanel pos
    if b
    then doWithWindowPosInPanel panelAction pos
    else doWithWindowPosInField action pos'

inPanel :: WindowAction Bool
inPanel (x, _y) = panelLeftX >>= \leftX -> return $ x >= leftX

panelLeftX :: ViewState Coord
panelLeftX = use (windowSize . _1) >>= \w -> return $ w - panelWidth
