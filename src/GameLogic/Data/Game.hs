module GameLogic.Data.Game where

import System.Random

data Game = Game { gWorld :: Int
                 , gRndGen :: StdGen }
  deriving (Show)

instance Eq Game where
    (Game w1 g1) == (Game w2 g2) = (w1 == w2) && (show g1 == show g2)

initialGame seed = Game 1 (mkStdGen seed)
mkGame world seed = Game world (mkStdGen seed)
