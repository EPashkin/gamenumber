module GameLogic.Action.Shield
    ( shieldAction
    , isShieldWorking
    ) where

import Control.Lens
import GameLogic.Data.Settings
import GameLogic.Data.Game
import GameLogic.Data.Players
import GameLogic.Action.ModifyPlayer
import GameLogic.GameState


shieldAction :: Int -> GameState ()
shieldAction playerInd = fromMaybeState $ maybeShieldAction playerInd

maybeShieldAction :: Int -> MaybeGameState ()
maybeShieldAction playerIndex =
    zoom (playerOfGame playerIndex) playerShieldAction
    >>= decreaseGamePlayerFree playerIndex

playerShieldAction :: StateT Player Maybe Int
playerShieldAction = mkState playerShieldAction'

playerShieldAction' :: Player -> (Int, Player)
playerShieldAction' player 
    | player ^. shieldActive
    = (0, player & shieldActive .~ False)
    | player ^. shieldStrength >= shieldActivationStrength
    = (0, player & shieldActive .~ True)
    | otherwise
    = (1, player & shieldStrength +~ 1)

isShieldWorking :: Player -> Bool
isShieldWorking player
    = player ^. shieldActive
    && player ^. free > shieldStopWorkingFree
