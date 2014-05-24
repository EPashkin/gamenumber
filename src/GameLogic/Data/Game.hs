{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.Game where

import System.Random
import Control.Lens
import GameLogic.Data.Config
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Players


data Game = Game { _world :: World
                 , _players :: Players
                 , _centerPos :: WorldPos -- position in center of screen
                 , _selectedPos :: WorldPos -- current action position for active player
                 , _placementMode :: Bool
                 , _rndGen :: StdGen }
  deriving (Show)
  
makeLenses ''Game

mkGameDef :: World -> Players -> StdGen -> Game
mkGameDef world players gen
    = Game { 
    _world = world
    , _players = players
    , _rndGen = gen
    , _centerPos = pos
    , _selectedPos = pos
    , _placementMode = False
    } where pos = findPlayerPos activePlayerIndex world

cellOfGame :: WorldPos -> Traversal' Game Cell 
cellOfGame pos = world . cellOfWorld pos
