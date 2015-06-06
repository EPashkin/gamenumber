{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module GameLogic.Data.Game where

import Debug.Trace
import System.Random
import Control.Lens
import Control.Exception
import Data.Binary
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Players


data GameData = GameData { _world :: World
                 , _players :: Players
                 , _centerPos :: WorldPos -- position in center of screen
                 , _placementMode :: Bool
                 , _paused :: Bool
                 , _gameSpeed :: GameSpeed
                 , _rndGen :: StdGen }
  deriving (Show)
  
makeLenses ''GameData

mkGameDef :: World -> Players -> StdGen -> GameData
mkGameDef world' players' gen
    = GameData {
    _world = world'
    , _players = players'
    , _rndGen = gen
    , _centerPos = pos
    , _placementMode = False
    , _paused = True
    , _gameSpeed = Normal
    } where pos = players' ^. toPlayer activePlayerIndex . selectedPos

{-# INLINE cellOfGame #-}
cellOfGame :: WorldPos -> Lens' GameData Cell 
cellOfGame pos = world . toCell pos

{-# INLINE playerOfGame #-}
playerOfGame :: Int -> Lens' GameData Player 
playerOfGame ind = players . toPlayer ind

doSaveGame :: FilePath -> GameData -> IO ()
doSaveGame filePath game = handle handler $ encodeFile filePath game
    where handler :: IOException -> IO ()
          handler ex = traceShow ex $ return ()

doLoadGame :: FilePath -> GameData -> IO GameData
doLoadGame filePath game = handle handler $ decodeFile filePath
    where handler :: IOException -> IO GameData
          handler ex = traceShow ex $ return game

instance Binary GameData where
    put g = do put $ g ^. rndGen
               put $ g ^. centerPos
               put $ g ^. players
               put $ g ^. world
    get = do gen <- get
             cp <- get
             ps <- get
             world' <- get
             return GameData{ _world = world'
                        , _players = ps
                        , _centerPos = cp
                        , _rndGen = gen
                        , _placementMode = False
                        , _paused = True
                        , _gameSpeed = Normal
                        }

instance Binary StdGen where
    put gen = put $ show gen
    get = do str <- get :: Get String
             return $ read str
