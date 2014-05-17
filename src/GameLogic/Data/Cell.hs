module GameLogic.Data.Cell where

data Cell = Cell { value :: Int
                 , playerIndex :: Int }
  deriving (Show)

mkCell v plInd = Cell { value = v, playerIndex = plInd }

cellChangeValue cell v = cell {value = v}

getCellValue Cell{value=v} = v

getCellPlayerIndex Cell{playerIndex=pi} = pi

isFree :: Cell -> Bool
isFree Cell{value=v, playerIndex=pi}
    | v == 0
        = True
    | pi == 0
        = True
    | otherwise
        = False
