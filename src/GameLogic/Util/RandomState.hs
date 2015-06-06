module GameLogic.Util.RandomState
   ( module GameLogic.Util.RandomState
   , R.RandomGen()
   ) where

import System.Random as R
import Control.Monad.State.Lazy

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt rng = state $ randomR rng
