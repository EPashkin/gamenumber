module GameLogic.Data.Game where

import System.Random
import GameLogic.Data.Cell
import GameLogic.Data.World

data Game = Game { gWorld :: World
                 , gCenterPos :: WorldPos -- position in center of screen
                 , gRndGen :: StdGen }
  deriving (Show)

--TODO: to config
defWorldSize = 8
defNumPlayers = 4
defSeed = 0::Int -- set not 0 for debug purposes

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
mkStartGameGen gen = Game { gWorld = world, gRndGen = gen', gCenterPos = findPlayerPos world 1 }
    where
    (world, gen') = mkStartWorld defWorldSize defNumPlayers gen

mkGame world seed = Game { gWorld = world, gRndGen = (mkStdGen seed), gCenterPos = findPlayerPos world 1 }

getWorld :: Game -> World
getWorld game = gWorld game

getGameCell :: Game -> WorldPos -> Cell
getGameCell game pos = getWorldCell world pos 
    where world = getWorld game

setCenterPos :: Game -> WorldPos -> Game
setCenterPos game pos = game { gCenterPos = pos }
