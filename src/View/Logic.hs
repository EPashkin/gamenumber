module View.Logic where

import Data.Maybe
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic
import View.Convert
import View.ViewState
import View.Util


runGameStep :: ViewState ()
runGameStep = zoom game doGameStep

startPlacement :: WindowAction ()
startPlacement pos = placementModeOfGame .= True
    >> doWithWindowPos doSelectCellAction pos

stopPlacement :: ViewState ()
stopPlacement = placementModeOfGame .= False

inPlacementMode :: ViewState Bool
inPlacementMode = use placementModeOfGame

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
