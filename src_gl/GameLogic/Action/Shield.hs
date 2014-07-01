module GameLogic.Action.Shield
    ( shieldAction
    , isShieldWorking
    ) where

import Control.Lens
import Data.Maybe
import GameLogic.Data.Settings
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Action.ModifyPlayer


shieldAction :: Int -> GameData -> GameData
shieldAction playerInd game
    = fromMaybe game $ maybeShieldAction playerInd game
    >>= decreaseGamePlayerFree playerInd

maybeShieldAction :: Int -> GameData -> Maybe (Int, GameData)
maybeShieldAction playerIndex game
    = playerShieldAction pl
    >>= (\(cost, pl') -> Just (cost, game & playerOfGame playerIndex .~ pl') )
    where pl = game ^. playerOfGame playerIndex


playerShieldAction :: Player -> Maybe (Int, Player)
playerShieldAction player
    | player ^. shieldActive
    = Just (0, player & shieldActive .~ False)
    | player ^. shieldStrength >= shieldActivationStrength
    = Just (0, player & shieldActive .~ True)
    | otherwise
    = Just (1, player & shieldStrength +~ 1)

isShieldWorking :: Player -> Bool
isShieldWorking player
    = player ^. shieldActive
    && player ^. free > shieldStopWorkingFree
