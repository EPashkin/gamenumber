{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module GameLogic.Data.Player where

import Data.Array
import Control.Lens


data Player = Player { _num :: Int    -- sum values of all owned cells
                     , _free :: Int   -- number of values can be placed (used)
                     , _remain :: Int -- counter for _free increase
                     , _shield :: Int -- shield (active flag and activation steps)
                     }
    deriving (Show)

makeLenses ''Player

type Players = Array Int Player

player :: Int -> Traversal' Players Player
player = ix

mkPlayer = Player {_num = 1
                  , _free = 0
                  , _remain = 0
                  , _shield = 0
                  }

mkPlayers :: Int -> Players
mkPlayers num = array (1, num) [(i, mkPlayer) | i <- [1..num]]
