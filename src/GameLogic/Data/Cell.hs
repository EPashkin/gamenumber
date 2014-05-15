module GameLogic.Data.Cell where

data Cell = Cell { value :: Int
                 , playerIndex :: Int }
  deriving (Show)

mkCell v plInd = Cell v plInd

cellChangeValue cell v = cell {value = v}