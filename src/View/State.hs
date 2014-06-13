{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.Gloss.Facade (Picture)
import GameLogic.Data.Facade
import GameLogic.Logic
import GameLogic.StartLogic
import GameLogic.Action.ModifyPlayer
import GameLogic.Action.Shield
import View.Convert

data StateData = StateData { _game :: GameData
                   , _windowSize :: (Int, Int) -- current window size
                   }
  deriving (Show)
  
makeLenses ''StateData

newState :: IO StateData
newState = do
    --runStartupTest
    game <- newGame
    return $ StateData game (100, 100)

runStartupTest = do
    traceIO "Testing"
    traceIO . show $ getNearestPoses (2,3)

runGameStep :: Float -> StateData -> IO StateData
runGameStep _ = return . over game doGameStep

startPlacement :: (Float, Float) -> StateData -> StateData
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: StateData -> StateData
stopPlacement = set placementModeOfGame False

inPlacementMode :: StateData -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' StateData Bool
placementModeOfGame = game . placementMode

centering :: (Float, Float) -> StateData -> StateData
centering = doWithWindowPos setCenterPosLimited

drawing :: (Float, Float) -> StateData -> StateData
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: (Int, Int) -> StateData -> StateData
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

doWithWindowPosOnGame :: (WorldPos -> GameData -> GameData) -> (Float, Float)
  -> GameData -> GameData
doWithWindowPosOnGame action pos game = action pos' game
    where pos' = worldPosOfWindowPos game pos

doWithWindowPosInField :: (WorldPos -> GameData -> GameData) -> (Float, Float) -> StateData -> StateData
doWithWindowPosInField action pos = game %~ doWithWindowPosOnGame action pos

doWithWindowPos :: (WorldPos -> GameData -> GameData) -> (Float, Float) -> StateData -> StateData
doWithWindowPos action pos@(x, y) state
    | inPanel pos state
    = state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX, y)

inPanel :: (Float, Float) -> StateData -> Bool
inPanel (x, y) state = x >= panelLeftX state

panelLeftX :: StateData -> Float
panelLeftX state = width/2 - panelWidth
    where size = state ^. windowSize
          width = fromIntegral $ fst size

worldShiftX = - panelWidth / 2 :: Float
