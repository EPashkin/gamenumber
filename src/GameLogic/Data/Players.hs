{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.Players where

import System.Random
import Control.Monad.State
import Data.Array
import Control.Lens
import GameLogic.Data.Config


data Player = Player { _num :: Int    -- sum values of all owned cells
                     , _free :: Int   -- number of values can be placed (used)
                     , _remain :: Int -- counter for _free increase
                     , _aggr :: Int   -- aggro size for AI players
                     , _shield :: Int -- shield (active flag and activation steps)
                     }
    deriving (Show)

makeLenses ''Player

type Players = Array Int Player

player :: Int -> Traversal' Players Player
player = ix

mkPlayer aggr = Player {_num = 1
                  , _free = 0
                  , _remain = 0
                  , _aggr = aggr
                  , _shield = 0
                  }

mkPlayers :: Int -> StdGen -> (Players, StdGen)
mkPlayers num gen = (players, gen) 
    where players = array (1, num) $ (activePlayerIndex, mkPlayer 0) 
              : [(i, mkPlayer rnd) | (i, rnd) <- list]
          lPlayerNums = [2..num]
          (lRandoms, gen') = runState (getNRndAggros (num - 1)) gen
          list = zip lPlayerNums lRandoms

getRndAggro :: State StdGen Int
getRndAggro = do
  gen <- get
  let (value, gen') = randomR (aiAggroMin, aiAggroMax) gen
  put gen'
  return value

getNRndAggros :: Int -> State StdGen [Int]
getNRndAggros 0 = return []
getNRndAggros n = do
  value <- getRndAggro
  list <- getNRndAggros (n-1)
  return (value:list)
