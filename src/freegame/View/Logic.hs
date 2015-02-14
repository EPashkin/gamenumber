module View.Logic where

import Data.Maybe
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic
import View.Convert
import View.ViewState
import View.Util
import Paths_gamenumber


runGameStep :: ViewState ()
runGameStep = zoom game $ framed doGameStep

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
setCenterPosOnMiniMap (x,y) = framed $ setCenterPos (floor x, floor y)

drawing :: WindowAction ()
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: WindowAction ()
updateWindowSize = (.=) windowSize

saveFileName :: IO FilePath
saveFileName = getDataFileName "gamenumber.gn"

doSave :: ViewState ()
doSave = zoom game $ do
    g <- get
    liftIO $ do
        fileName <- saveFileName
        doSaveGame fileName g

doLoad :: ViewState ()
doLoad = zoom game $ do
    g <- get
    g' <- liftIO $ do
        fileName <- saveFileName
        doLoadGame fileName g
    put g'

doHelpPlayer :: ViewState ()
doHelpPlayer = zoom game . framed $ helpPlayer activePlayerIndex

doChangePaused :: ViewState ()
doChangePaused = game . paused %= not

doShieldAction :: ViewState ()
doShieldAction = zoom game . framed $ shieldAction activePlayerIndex

increaseSpeed :: ViewState ()
increaseSpeed = game . gameSpeed %= succ'
    where succ' gs = if gs == maxBound then gs
                     else succ gs

decreaseSpeed :: ViewState ()
decreaseSpeed = game . gameSpeed %= pred'
    where pred' gs = if gs == minBound then gs
                     else pred gs
