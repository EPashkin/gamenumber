module View.Logic where

import Data.Maybe
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic
import View.Convert
import View.GameState


type WindowAction a = (Coord, Coord) -> GameState a
type WindowGameAction a = (Coord, Coord) -> GameState' a
type PanelAction a = Coord -> WindowGameAction a


runGameStep :: GameState ()
runGameStep = game %= doGameStep

startPlacement :: WindowAction ()
startPlacement pos = placementModeOfGame .= True
    >> doWithWindowPos doSelectCellAction pos

stopPlacement :: GameState ()
stopPlacement = placementModeOfGame .= False

inPlacementMode :: GameState Bool
inPlacementMode = use placementModeOfGame

placementModeOfGame :: Lens' StateData Bool
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

doSave :: GameState ()
doSave = zoom game $ get >>= liftIO . doSaveGame "gamenumber.gn"

doLoad :: GameState ()
doLoad = zoom game $ get >>= liftIO . doLoadGame "gamenumber.gn" >>= put

doHelpPlayer :: GameState ()
doHelpPlayer = game %= p
    where p g = fromMaybe g $ decreaseGamePlayerFree activePlayerIndex (-10, g)

doChangePaused :: GameState ()
doChangePaused = game . paused %= not

doShieldAction :: GameState ()
doShieldAction = game %= shieldAction activePlayerIndex

increaseSpeed :: GameState ()
increaseSpeed = game . gameSpeed %= succ'
    where succ' gs = if gs == maxBound then gs
                     else succ gs

decreaseSpeed :: GameState ()
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

panelLeftX :: GameState Coord
panelLeftX = use (windowSize . _1) >>= \w -> return $ w - panelWidth
