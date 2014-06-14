{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.FreeGame.Facade
import GameLogic.Data.Facade
import GameLogic.Logic
import GameLogic.StartLogic
import GameLogic.Action.ModifyPlayer
import GameLogic.Action.Shield
import View.Convert


instance (Show Font) where
  show _ = "Font"

data StateData = StateData { _game :: GameData
                   , _windowSize :: (Coord, Coord) -- current window size
                   , _font :: Font
                   }
  deriving (Show)

makeLenses ''StateData

newState :: Font -> IO StateData
newState font = do
    --runStartupTest
    game <- newGame
    return $ StateData game (100, 100) font

runStartupTest = do
    traceIO "Testing"
    traceIO . show $ getNearestPoses (2,3)

runGameStep :: StateData -> StateData
runGameStep = over game doGameStep

startPlacement :: (Coord, Coord) -> StateData -> StateData
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: StateData -> StateData
stopPlacement = set placementModeOfGame False

inPlacementMode :: StateData -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' StateData Bool
placementModeOfGame = game . placementMode

centering :: (Coord, Coord) -> StateData -> StateData
centering = doWithWindowPos setCenterPosLimited

drawing :: (Coord, Coord) -> StateData -> StateData
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: (Coord, Coord) -> StateData -> StateData
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
doShieldAction state = state & game %~ shieldAction activePlayerIndex

doWithWindowPosOnGame :: (WorldPos -> GameData -> GameData) -> (Coord, Coord)
  -> GameData -> GameData
doWithWindowPosOnGame action pos game = action pos' game
    where pos' = worldPosOfWindowPos game pos

doWithWindowPosInField :: (WorldPos -> GameData -> GameData) -> (Coord, Coord) -> StateData -> StateData
doWithWindowPosInField action pos = game %~ doWithWindowPosOnGame action pos

doWithWindowPos :: (WorldPos -> GameData -> GameData) -> (Coord, Coord) -> StateData -> StateData
doWithWindowPos action pos@(x, y) state
    | inPanel pos state
    = state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX - w/2, y - h/2)
          (w,h) = state ^. windowSize

inPanel :: (Coord, Coord) -> StateData -> Bool
inPanel (x, y) state = x >= panelLeftX state

panelLeftX :: StateData -> Coord
panelLeftX state = width - panelWidth
    where (width, _) = state ^. windowSize

worldShiftX = - panelWidth / 2 :: Coord
