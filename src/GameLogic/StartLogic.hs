module GameLogic.StartLogic where

import System.Random
import Control.Lens
import GameLogic.Data.Settings
import GameLogic.Data.Cell
import GameLogic.Data.World
import GameLogic.Data.Players
import GameLogic.Data.Game
import Util.Shuffle


newGame::IO Game
newGame = newGame' defSeed

newGame':: Int -> IO Game
newGame' seed
    | 0 == seed
    = fmap mkStartGameGen newStdGen
    | otherwise
    = return $ mkStartGame seed

mkStartGame :: Int -> Game
mkStartGame seed = mkStartGameGen gen
    where gen = mkStdGen seed

mkStartGameGen :: StdGen -> Game
mkStartGameGen gen = mkGameDef world players gen''
    where (world, gen') = mkStartWorld defWorldSize defNumPlayers gen
          (players, gen'') = mkPlayers defNumPlayers gen'

{-# ANN mkStartWorld "HLint: ignore Eta reduce" #-}
mkStartWorld :: RandomGen g => Int ->Int -> g -> (World, g)
mkStartWorld size numPlayers gen = placeWorldPlayers (mkEmptyWorld size) numPlayers gen

placeWorldPlayers :: RandomGen g => World -> Int -> g -> (World, g)
placeWorldPlayers world numPlayers gen =
    let players = [1..numPlayers]
        positions = [calcStartPos world numPlayers pl | pl <- players]
        (positions', gen') = shuffle gen positions
        playersPosition = zip positions' players
        p w (pos, pl) = w & ix pos .~ mkCell 1 pl
        world' = foldl p world playersPosition
    in (world', gen')

calcStartPos :: World -> Int -> Int -> WorldPos
calcStartPos world numPlayers num = 
    let list = playersStartPosXList (getWorldSize world) numPlayers 
        cols = playersStartPosCols numPlayers
        xInd = ((num-1) `mod` cols)
        yInd = ((num-1) `div` cols)
    in (list !! xInd, list !! yInd)

-- return number of columns of players start positions by number of players
playersStartPosCols :: Int -> Int
playersStartPosCols 4 = 2
playersStartPosCols 16 = 4

-- return list x-coords start positions
playersStartPosXList :: Int -> Int -> [Int]
playersStartPosXList size numPlayers =
    let cols = playersStartPosCols numPlayers
        dist = size `div` cols
    in fmap (\i -> (i-1)*dist + (dist `div` 2) + 1 ) [1..cols]
