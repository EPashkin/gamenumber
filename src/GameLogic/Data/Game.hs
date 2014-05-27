{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.Game where

import System.Random
import Control.Lens
import Data.Binary
import GameLogic.Data.Settings
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

doSaveGame :: FilePath -> Game-> IO ()
doSaveGame = encodeFile

--TODO: error prone version doLoadGame
doLoadGame :: FilePath -> Game -> IO Game
doLoadGame filePath game = decodeFile filePath

instance Binary Game where
    put g = do put $ g ^. rndGen
               put $ g ^. centerPos
               put $ g ^. selectedPos
               put $ g ^. players
               put $ g ^. world
    get = do gen <- get
             cp <- get
             sp <- get
             ps <- get
             world <- get
             return Game{ _world = world
                        , _players = ps
                        , _centerPos = cp
                        , _selectedPos = sp
                        , _rndGen = gen
                        , _placementMode = False
                        }

instance Binary StdGen where
    put gen = put $ show gen
    get = do str <- get :: Get String
             return $ read str
