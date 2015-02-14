module GameLogic.Data.Settings where


--TODO: to config
maxCellValue = 9 :: Int
defWorldSize = 64 :: Int
defNumPlayers = 16 :: Int
defSeed = 0 :: Int -- set not 0 for debug purposes
activePlayerIndex = 1 :: Int
aiAggroMax = defWorldSize * 3 `div` 4 :: Int
aiAggroMin = defWorldSize `div` 10 + 1 :: Int
remainDivMin = 4 :: Int    -- for slower start
-- 20 - ~1 FREE per 2 sec for max valued player (see ticksPerSecond)
remainDivMult = 2 :: Int    -- ~5 FREE per second for max valued player (see ticksPerSecond)
shieldActivationStrength = 128 :: Int
shieldStopWorkingFree = -10 :: Int -- shield does not protect if 'free' is too low
shieldAINumMultiplier = 9 :: Int
