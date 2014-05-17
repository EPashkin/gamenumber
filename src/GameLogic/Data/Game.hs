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
defWorldSize = 8
defNumPlayers = 16
defSeed = 0::Int -- set not 0 for debug purposes
activePlayerIndex = 1

newGame::IO Game
newGame = newGame' defSeed

newGame':: Int -> IO Game
newGame' seed
    | 0 == seed
       = newStdGen  >>= \ gen ->
       return $ mkStartGameGen gen
    | otherwise
       = return $ mkStartGame seed

mkStartGame :: Int -> Game
mkStartGame seed = mkStartGameGen gen
       where gen = mkStdGen seed

mkStartGameGen :: StdGen -> Game
mkStartGameGen gen = mkGameDef world gen'
    where
    (world, gen') = mkStartWorld defWorldSize defNumPlayers gen

mkGame world seed = mkGameDef world $ mkStdGen seed

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

setCenterPos :: Game -> WorldPos -> Game
setCenterPos game pos = 
    let pos' = limitPosToWorld game pos
    in game { gCenterPos = pos' }
    
doCellAction :: Game -> WorldPos -> Game
doCellAction game pos
    | not $ isPosInWorld game pos
    = game
    | otherwise
    = game { gSelectedPos = pos }

isPosInWorld :: Game -> WorldPos -> Bool
isPosInWorld game (x, y)
    | x >= 1
    , x <= getWorldSize (getWorld game)
    , y >= 1
    , y <= getWorldSize (getWorld game)
    = True
    | otherwise
    = False

limitPosToWorld :: Game -> WorldPos -> WorldPos
limitPosToWorld game pos = limitPosToWorld' pos $ getWorldSize $ getWorld game

limitPosToWorld' (x, y) max
    | x < 1
    = limitPosToWorld' (1, y) max
    | x > max
    = limitPosToWorld' (max, y) max
    | y < 1
    = limitPosToWorld' (x, 1) max
    | y > max
    = limitPosToWorld' (x, max) max
    | otherwise
    = (x,y)
