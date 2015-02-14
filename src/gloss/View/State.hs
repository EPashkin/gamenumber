{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.Gloss.Facade (Picture)
import GameLogic
import View.Convert
import Paths_gamenumber


data ViewData = ViewData { _game :: GameData
                   , _windowSize :: (Int, Int) -- current window size
                   }
  deriving (Show)
  
makeLenses ''ViewData

type ViewAction = ViewData -> ViewData
type ViewActionIO = ViewData -> IO ViewData
type WindowAction a = (Coord, Coord) -> ViewAction
type WindowGameAction a = (Coord, Coord) -> GameState a
type PanelAction a = Coord -> WindowGameAction a

newState :: IO ViewData
newState = do
    --runStartupTest
    game <- newGame
    return $ ViewData game (100, 100)

runStartupTest = do
    traceIO "Testing"
    traceIO . show $ getNearestPoses (2,3)

runGameStep :: Float -> ViewActionIO
runGameStep _ = return . over game (execState doGameStep)

startPlacement :: WindowAction ()
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: ViewAction
stopPlacement = set placementModeOfGame False

inPlacementMode :: ViewData -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' ViewData Bool
placementModeOfGame = game . placementMode

centering :: WindowAction ()
centering = doWithWindowPos2 setCenterPosLimited setCenterPosByMiniMap

setCenterPosByMiniMap :: PanelAction ()
setCenterPosByMiniMap height (x, y) =
    setCenterPosOnMiniMap (x - dx, y - dy)
    where (dx, dy) = shiftMiniMap height

setCenterPosOnMiniMap :: WindowGameAction ()
setCenterPosOnMiniMap (x, y) = setCenterPos (floor x, floor y)

drawing :: WindowAction ()
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: (Int, Int) -> ViewAction
updateWindowSize = set windowSize

saveFileName :: IO FilePath
saveFileName = getDataFileName "gamenumber.gn"

doSave :: ViewActionIO
doSave state = do 
    fileName <- saveFileName
    doSaveGame fileName $ state ^. game
    return state

doLoad :: ViewActionIO
doLoad state = do
    let g = state ^. game
    fileName <- saveFileName
    g' <- doLoadGame fileName g
    return $ set game g' state

doHelpPlayer :: ViewAction
doHelpPlayer state
    = state & game %~ execState (helpPlayer activePlayerIndex)

doChangePaused :: ViewAction
doChangePaused = game . paused %~ not

doShieldAction :: ViewAction
doShieldAction state = state & game %~ execState (shieldAction activePlayerIndex)

increaseSpeed :: ViewAction
increaseSpeed = game . gameSpeed %~ succ'
    where succ' gs = if gs == maxBound then gs
                     else succ gs

decreaseSpeed :: ViewAction
decreaseSpeed = game . gameSpeed %~ pred'
    where pred' gs = if gs == minBound then gs
                     else pred gs

doWithWindowPosOnGame :: WorldAction -> WindowGameAction ()
doWithWindowPosOnGame action pos = gets (worldPosOfWindowPos pos) >>= action

doWithWindowPosInField :: WorldAction -> WindowAction ()
doWithWindowPosInField action pos =
    game %~ execState (doWithWindowPosOnGame action pos)

doWithWindowPos :: WorldAction -> WindowAction ()
doWithWindowPos action pos@(x, y) state
    | inPanel pos state
    = state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX, y)

doWithWindowPos2 :: WorldAction -> PanelAction () -> WindowAction ()
doWithWindowPos2 action panelAction pos@(x, y) state
    | inPanel pos state
    = doWithWindowPosInPanel panelAction pos state
    | otherwise
    = doWithWindowPosInField action pos' state
    where (w, h) = view windowSize state
          pos' = (x - worldShiftX, y)

doWithWindowPosInPanel :: PanelAction () -> WindowAction ()
doWithWindowPosInPanel panelAction (x, y) state =
    state & game %~ f
    where x' = x - panelLeftX state
          (_, h) = state ^. windowSize
          f = execState $ panelAction (fromIntegral h) (x', y)

inPanel :: (Coord, Coord) -> ViewData -> Bool
inPanel (x, y) state = x >= panelLeftX state

panelLeftX :: ViewData -> Coord
panelLeftX state = width/2 - panelWidth
    where size = state ^. windowSize
          width = fromIntegral $ fst size

worldShiftX = - panelWidth / 2 :: Coord
