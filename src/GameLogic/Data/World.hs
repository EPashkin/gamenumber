module GameLogic.Data.World where

import Data.Array
import GameLogic.Data.Cell
import System.Random
import Util.Shuffle


type WorldPos = Int
startWorlPos = 1

type World = Array WorldPos Cell

mkEmptyWorld :: Int -> World
mkEmptyWorld size = array (startWorlPos, size) [(i, mkCell 0 0) | i <- [1..size]]

mkStartWorld :: Int ->Int -> StdGen -> (World, StdGen)
mkStartWorld size numPlayers gen = placeWorldPlayers (mkEmptyWorld size) numPlayers gen

setWorldCell :: World -> WorldPos -> Cell -> World
setWorldCell world pos cell = world // [(pos, cell)]

placeWorldPlayers :: World -> Int -> StdGen -> (World, StdGen)
placeWorldPlayers world numPlayers gen =
    let players = [1..numPlayers]
	positions = [pl * 2 | pl <- players]
        (positions', gen') = shuffle gen positions
        playersPosition = zipWith (\a b -> (a,b)) positions' players
        world' = foldl (\w (pos, pl) -> setWorldCell w pos (mkCell 1 pl)) world playersPosition :: World
    in (world', gen')
