module Middleware.FreeGame.Environment where

import FreeGame
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Lens
import View.State

{-
runEnvironment:: Int -> game -> (game -> IO Picture) -> (Event -> game -> IO game)
    -> (Float -> game -> IO game) -> IO ()
runEnvironment = playIO (InWindow "GameNumber" (800, 550) (10, 10)) black
-}

runEnvironment :: Int -> StateData -> (StateData -> Frame()) -> {- (Float -> State -> Frame State) -> -} IO (Maybe ())
runEnvironment ticksPerSecond state drawState {-runStep-} = runGame Resizable (Box (V2 0 0) (V2 800 550)) $ do
    setTitle "GameNumber"
    clearColor black
    setFPS ticksPerSecond
    subLoop drawState state

subLoop :: (StateData -> Frame()) -> StateData -> Game ()
subLoop drawState state = do
    state' <- state & runGameState (updateWindowSize>>processEvents)
    lift $ drawState state'
    unlessM (keyPress KeyEscape) $ tick >> subLoop drawState state'

type GameStateA a = StateT StateData Game a

type GameState = GameStateA StateData

runGameState :: GameState -> StateData-> Game StateData
runGameState = evalStateT

overGameState :: (StateData -> StateData) -> GameState
overGameState f = modify f >> get 

whenGameState :: GameStateA Bool -> GameState -> GameState
whenGameState mp m = mp >>= bool get m

updateWindowSize :: GameState
updateWindowSize = do
    Box _ (V2 w h) <- getBoundingBox
    overGameState $ windowSize .~ (w, h)

processEvents :: GameState
processEvents = do
    overGameState incCounter
    onKeyA
    onKeyB

incCounter100 :: StateData -> StateData
incCounter100 = counter +~ 100

incCounter :: StateData -> StateData
incCounter = counter +~ 1

onKeyA :: GameState
onKeyA = do
    whenM (keyDown KeyA) $ color green $ polygon [V2 20 0, V2 100 20, V2 90 60, V2 30 70]
    get

onKeyB :: GameState
onKeyB = do
    whenGameState (keyDown KeyB) $ overGameState incCounter100
    color green $ polygon [V2 100 0, V2 100 20, V2 90 60, V2 30 70]
    get
