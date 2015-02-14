{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.Gloss.Facade (Picture)
import GameLogic
import View.Convert

data ViewData = ViewData { _game :: GameData
                   , _windowSize :: (Int, Int) -- current window size
                   }
  deriving (Show)
  
makeLenses ''ViewData

type WindowAction = (Coord, Coord) -> ViewData -> ViewData
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

runGameStep :: Float -> ViewData -> IO ViewData
runGameStep _ = return . over game (execState doGameStep)

startPlacement :: (Float, Float) -> ViewData -> ViewData
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: ViewData -> ViewData
stopPlacement = set placementModeOfGame False

inPlacementMode :: ViewData -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' ViewData Bool
placementModeOfGame = game . placementMode

centering :: (Float, Float) -> ViewData -> ViewData
centering = doWithWindowPos2 setCenterPosLimited setCenterPosByMiniMap

setCenterPosByMiniMap :: PanelAction ()
setCenterPosByMiniMap height (x, y) =
    setCenterPosOnMiniMap (x - dx, y - dy)
    where (dx, dy) = shiftMiniMap height

setCenterPosOnMiniMap :: WindowGameAction ()
setCenterPosOnMiniMap (x, y) = setCenterPos (floor x, floor y)

drawing :: (Float, Float) -> ViewData -> ViewData
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: (Int, Int) -> ViewData -> ViewData
updateWindowSize = set windowSize

doSave :: ViewData -> IO ViewData
doSave state = do 
    doSaveGame "gamenumber.gn" $ state ^. game
    return state

doLoad :: ViewData -> IO ViewData
doLoad state = do
    let g = state ^. game
    g' <- doLoadGame "gamenumber.gn" g
    return $ set game g' state

doHelpPlayer :: ViewData -> ViewData
doHelpPlayer state
    = state & game %~ execState (helpPlayer activePlayerIndex)

doChangePaused :: ViewData -> ViewData
doChangePaused = game . paused %~ not

doShieldAction :: ViewData -> ViewData
doShieldAction state = state & game %~ execState (shieldAction activePlayerIndex)

increaseSpeed :: ViewData -> ViewData
increaseSpeed = game . gameSpeed %~ succ'
    where succ' gs = if gs == maxBound then gs
                     else succ gs

decreaseSpeed :: ViewData -> ViewData
decreaseSpeed = game . gameSpeed %~ pred'
    where pred' gs = if gs == minBound then gs
                     else pred gs

doWithWindowPosOnGame :: WorldAction -> (Float, Float) -> GameData-> GameData
doWithWindowPosOnGame action pos game = execState (action pos') game
    where pos' = worldPosOfWindowPos game pos

doWithWindowPosInField :: WorldAction -> WindowAction
doWithWindowPosInField action pos = game %~ doWithWindowPosOnGame action pos

doWithWindowPos :: WorldAction -> WindowAction
doWithWindowPos action pos@(x, y) state
    | inPanel pos state
    = state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX, y)

doWithWindowPos2 :: WorldAction -> PanelAction () -> WindowAction
doWithWindowPos2 action panelAction pos@(x, y) state
    | inPanel pos state
    = doWithWindowPosInPanel panelAction pos state
    | otherwise
    = doWithWindowPosInField action pos' state
    where (w, h) = view windowSize state
          pos' = (x - worldShiftX, y)

doWithWindowPosInPanel :: PanelAction () -> WindowAction
doWithWindowPosInPanel panelAction (x, y) state =
    state & game %~ f
    where x' = x - panelLeftX state
          (_, h) = state ^. windowSize
          f = execState $ panelAction (fromIntegral h) (x', y)

inPanel :: (Float, Float) -> ViewData -> Bool
inPanel (x, y) state = x >= panelLeftX state

panelLeftX :: ViewData -> Float
panelLeftX state = width/2 - panelWidth
    where size = state ^. windowSize
          width = fromIntegral $ fst size

worldShiftX = - panelWidth / 2 :: Float
