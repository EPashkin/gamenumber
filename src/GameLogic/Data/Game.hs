{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Game where

import System.Random
import Control.Lens
import Data.Maybe
import GameLogic.Data.Cell
import GameLogic.Data.World

data Game = Game { _world :: World
                 , _centerPos :: WorldPos -- position in center of screen
                 , _selectedPos :: WorldPos -- current action position for active player
                 , _rndGen :: StdGen }
  deriving (Show)
  
makeLenses ''Game

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
    _world = world
    , _rndGen = gen
    , _centerPos = findPlayerPos world activePlayerIndex
    , _selectedPos = findPlayerPos world activePlayerIndex
    }

getWorld :: Game -> World
getWorld = view world

getCenterPos :: Game -> WorldPos
getCenterPos = view centerPos

setCenterPos :: Game -> WorldPos -> Game
setCenterPos game pos = set centerPos pos game

getSelectedPos :: Game -> WorldPos
getSelectedPos = _selectedPos

setSelectedPos :: Game -> WorldPos -> Game
setSelectedPos game pos = set selectedPos pos game

getGameCell :: Game -> WorldPos -> Cell
getGameCell game pos = fromMaybe undefined mcell
    where mcell = view world game ^? ix pos  

setGameCell :: Game -> WorldPos -> Cell -> Game
setGameCell game pos cell = (world `over` f) game
  where f world = setWorldCell world pos cell
