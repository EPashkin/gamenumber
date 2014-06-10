module GameLogic.Data.Settings where


--TODO: to config
defWorldSize = 8 :: Int
defNumPlayers = 16 :: Int
defSeed = 0 :: Int -- set not 0 for debug purposes
activePlayerIndex = 1 :: Int
aiAggroMax = defWorldSize * 3 `div` 4 :: Int
aiAggroMin = defWorldSize `div` 10 + 1 :: Int
remainDivMin = 4 :: Int    -- for slower start
remainDivMult = 20 :: Int   -- ~1 FREE per 2 sec for max valued player (see ticksPerSecond)
shieldActivationStrength = 128 :: Int
