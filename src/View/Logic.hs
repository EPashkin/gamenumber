module View.Logic where

import Debug.Trace
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic.Data.Facade
import GameLogic.Logic
import GameLogic.StartLogic
import GameLogic.Action.ModifyPlayer
import GameLogic.Action.Shield
import View.Convert
import View.GameState


type WindowActionA a = (Coord, Coord) -> StateData -> a
type WindowAction = WindowActionA StateData
type WindowGameAction = (Coord, Coord) -> GameData -> GameData
type PanelAction = Coord -> WindowGameAction

runGameStep :: StateData -> StateData
runGameStep = game %~ doGameStep

startPlacement :: WindowAction
startPlacement pos = set placementModeOfGame True
    . doWithWindowPos doSelectCellAction pos

stopPlacement :: StateData -> StateData
stopPlacement = set placementModeOfGame False

inPlacementMode :: StateData -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' StateData Bool
placementModeOfGame = game . placementMode

centering :: WindowAction
centering = doWithWindowPos2 setCenterPosLimited setCenterPosByMiniMap

setCenterPosByMiniMap :: PanelAction
setCenterPosByMiniMap height (x,y)
    = setCenterPosOnMiniMap (x', y')
    where V2 x' y' = V2 x y - shiftMiniMap height

setCenterPosOnMiniMap :: WindowGameAction
setCenterPosOnMiniMap (x,y) = setCenterPos (floor x, floor y)

drawing :: WindowAction
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: WindowAction
updateWindowSize = set windowSize

doSave :: StateData -> IO StateData
doSave state = do 
    doSaveGame "gamenumber.gn" $ state ^. game
    return state

doLoad :: StateData -> IO StateData
doLoad state = do
    let g = state ^. game
    g' <- doLoadGame "gamenumber.gn" g
    return $ set game g' state

doHelpPlayer :: StateData -> StateData
doHelpPlayer state
    = state & game .~ g'
    where g = state ^. game
          Just g' = decreaseGamePlayerFree activePlayerIndex (-10, g)

doChangePaused :: StateData -> StateData
doChangePaused = game . paused %~ not

doShieldAction :: StateData -> StateData
doShieldAction = game %~ shieldAction activePlayerIndex

increaseSpeed :: StateData -> StateData
increaseSpeed = iif ((/=) maxBound . view (game . gameSpeed))
    $ game . gameSpeed %~ succ

decreaseSpeed :: StateData -> StateData
decreaseSpeed = iif ((/=) minBound . view (game . gameSpeed))
    $ game . gameSpeed %~ pred

doWithWindowPosOnGame :: WorldAction -> WindowGameAction
doWithWindowPosOnGame action pos game = action pos' game
    where pos' = worldPosOfWindowPos game pos

doWithWindowPosInField :: WorldAction -> WindowAction
doWithWindowPosInField action pos = game %~ doWithWindowPosOnGame action pos

emptyPanelAction _ _ = id

doWithWindowPosInPanel :: PanelAction -> WindowAction
doWithWindowPosInPanel panelAction (x,y) state
    = state & game %~ panelAction h (x', y)
    where x' = x - panelLeftX state
          (_,h) = state ^. windowSize

doWithWindowPos :: WorldAction -> WindowAction
doWithWindowPos = (`doWithWindowPos2` emptyPanelAction)

doWithWindowPos2 :: WorldAction -> PanelAction -> WindowAction
doWithWindowPos2 action panelAction pos@(x, y) state
    | inPanel pos state
    = doWithWindowPosInPanel panelAction pos state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX - w/2, y - h/2)
          (w,h) = state ^. windowSize

inPanel :: WindowActionA Bool
inPanel (x, y) = (>=) x . panelLeftX

panelLeftX :: StateData -> Coord
panelLeftX = flip (-) panelWidth . fst . view windowSize
