{-# LANGUAGE TemplateHaskell #-}
module View.State where

import Debug.Trace
import Control.Lens
import Middleware.Gloss.Facade (Picture)
import GameLogic.Data.Facade
import GameLogic.Logic
import GameLogic.StartLogic
import View.Convert

data State = State { _game :: Game
                   , _windowSize :: (Int, Int) -- current window size
                   }
  deriving (Show)
  
makeLenses ''State

newState :: IO State
newState = do
    runStartupTest
    game <- newGame
    return $ State game (100, 100)

runStartupTest = do
   g1 <- newGame
   traceIO $ show g1
   let players = [1..defNumPlayers]
       w2 = g1 ^. world
       positions = [calcStartPos w2 (length players) pl | pl <- players]
       xList = playersStartPosXList defWorldSize defNumPlayers
   traceIO . show $ positions
   traceIO . show $ xList
   traceIO . show $ g1 ^? cellOfGame (2,2)
   traceIO . show $ getNearestPoses (2,3)
   traceIO . show $ g1 ^. GameLogic.Data.Facade.players


runGameStep :: Float -> State -> IO State
runGameStep _ = return . over game doGameStep

startPlacement :: (Float, Float) -> State -> State
startPlacement pos = set placementModeOfGame True . doWithWindowPos doSelectCellAction pos

stopPlacement :: State -> State
stopPlacement = set placementModeOfGame False

inPlacementMode :: State -> Bool
inPlacementMode = view placementModeOfGame 

placementModeOfGame :: Lens' State Bool
placementModeOfGame = game . placementMode

centering :: (Float, Float) -> State -> State
centering = doWithWindowPos setCenterPosLimited

drawing :: (Float, Float) -> State -> State
drawing = doWithWindowPos doSelectCellAction

updateWindowSize :: (Int, Int) -> State -> State
updateWindowSize = set windowSize

doSave :: State -> IO State
doSave state = do 
    doSaveGame "gamenumber.gn" $ state ^. game
    return state

doLoad :: State -> IO State
doLoad state = do
    let g = state ^. game
    g' <- doLoadGame "gamenumber.gn" g
    return $ set game g' state

doWithWindowPosOnGame :: (WorldPos -> Game -> Game) -> (Float, Float) -> Game-> Game
doWithWindowPosOnGame action pos game = action pos' game
    where pos' = worldPosOfWindowPos game pos

doWithWindowPosInField :: (WorldPos -> Game -> Game) -> (Float, Float) -> State -> State
doWithWindowPosInField action pos = game %~ doWithWindowPosOnGame action pos

doWithWindowPos :: (WorldPos -> Game-> Game) -> (Float, Float) -> State -> State
doWithWindowPos action pos@(x, y) state
    | inPanel pos state
    = state
    | otherwise
    = doWithWindowPosInField action pos' state
    where pos' = (x - worldShiftX, y)

inPanel :: (Float, Float) -> State -> Bool
inPanel (x, y) state = x >= panelLeftX state

panelLeftX :: State -> Float
panelLeftX state = width/2 - panelWidth
    where size = state ^. windowSize
          width = fromIntegral $ fst size

worldShiftX = - panelWidth / 2 :: Float
