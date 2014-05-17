module GameLogic.Data.Game where

import System.Random
import GameLogic.Data.Cell
import GameLogic.Data.World

data Game = Game { gWorld :: World
                 , gCenterPos :: WorldPos -- position in center of screen
                 , gSelectedPos :: WorldPos -- current action position for active player
                 , gRndGen :: StdGen }
  deriving (Show)

--TODO: to config
defWorldSize = 8::Int
defNumPlayers = 16::Int
defSeed = 0::Int -- set not 0 for debug purposes
activePlayerIndex = 1::Int

mkGame :: World -> Int -> Game
mkGame world seed = mkGameDef world $ mkStdGen seed

mkGameDef :: World -> StdGen -> Game 
mkGameDef world gen 
    = Game { 
    gWorld = world
    , gRndGen = gen
    , gCenterPos = findPlayerPos world activePlayerIndex
    , gSelectedPos = findPlayerPos world activePlayerIndex
    }  

getWorld :: Game -> World
getWorld = gWorld

getGameCell :: Game -> WorldPos -> Cell
getGameCell game = getWorldCell world
    where world = getWorld game

setGameCell :: Game -> WorldPos -> Cell -> Game
setGameCell game pos cell = game {gWorld = world'}
    where world' = setWorldCell (getWorld game) pos cell
