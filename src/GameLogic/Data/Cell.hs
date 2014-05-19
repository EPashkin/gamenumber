{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Cell where

import Control.Lens

data Cell = Cell { _value :: Int
                 , _playerIndex :: Int }
  deriving (Show)

makeLenses ''Cell

mkCell v plInd = Cell { _value = v, _playerIndex = plInd }

setCellValue cell v = set value v cell

getCellValue :: Cell -> Int
getCellValue = view value

getCellPlayerIndex :: Cell -> Int
getCellPlayerIndex = view playerIndex

isFree :: Cell -> Bool
isFree Cell{_value=v, _playerIndex=pi}
    | v == 0
    = True
    | pi == 0
    = True
    | otherwise
    = False

isOwnedBy :: Int -> Cell -> Bool
isOwnedBy playerInd Cell{_playerIndex=pi} 
    | pi == playerInd
    = True
    | otherwise
    = False
