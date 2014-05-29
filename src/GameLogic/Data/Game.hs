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
                 , _placementMode :: Bool
                 , _paused :: Bool
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
    , _placementMode = False
    , _paused = True
    } where Just pos = players ^? ix activePlayerIndex . selectedPos

cellOfGame :: WorldPos -> Traversal' Game Cell 
cellOfGame pos = world . ix pos

doSaveGame :: FilePath -> Game-> IO ()
doSaveGame = encodeFile

--TODO: error prone version doLoadGame
doLoadGame :: FilePath -> Game -> IO Game
doLoadGame filePath game = decodeFile filePath

instance Binary Game where
    put g = do put $ g ^. rndGen
               put $ g ^. centerPos
               put $ g ^. players
               put $ g ^. world
    get = do gen <- get
             cp <- get
             ps <- get
             world <- get
             return Game{ _world = world
                        , _players = ps
                        , _centerPos = cp
                        , _rndGen = gen
                        , _placementMode = False
                        , _paused = True
                        }

instance Binary StdGen where
    put gen = put $ show gen
    get = do str <- get :: Get String
             return $ read str
