module GameLogic.Action.Shield
    ( shieldAction
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Data.Settings
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Action.ModifyPlayer


shieldAction :: Int -> Game -> Game
shieldAction playerInd game
    = fromMaybe game $ maybeShieldAction playerInd game
    >>= decreaseGamePlayerFree playerInd

maybeShieldAction :: Int -> Game -> Maybe (Int, Game)
maybeShieldAction playerIndex game
    = playerShieldAction pl
    >>= (\(cost, pl') -> Just (cost, game & players . ix playerIndex .~ pl') )
    where Just pl = game ^? players . ix playerIndex


playerShieldAction :: Player -> Maybe (Int, Player)
playerShieldAction player
    | player ^. shieldActive
    = Just (0, player & shieldActive .~ False)
    | player ^. shieldStrength >= shieldActivationStrength
    = Just (0, player & shieldActive .~ True)
    | otherwise
    = Just (1, player & shieldStrength +~ 1)
