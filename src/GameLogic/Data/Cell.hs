{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Cell where

import Control.Lens
import Data.Binary
import Data.Int


data Cell = Cell { _value :: Int
                 , _playerIndex :: Int }
  deriving (Show)

makeLenses ''Cell

mkCell :: Int -> Int -> Cell
mkCell v plInd = Cell { _value = v, _playerIndex = plInd }

isFree :: Cell -> Bool
isFree Cell{ _value = v, _playerIndex = plInd }
    | v == 0
    = True
    | plInd == 0
    = True
    | otherwise
    = False

isOwnedBy :: Int -> Cell -> Bool
isOwnedBy playerInd Cell{ _playerIndex = plInd }
    = plInd == playerInd

instance Binary Cell where
    put c = do put (fromIntegral $ c ^. value :: Int8)
               put (fromIntegral $ c ^. playerIndex :: Int8)
    get = do v <- get :: Get Int8
             plInd <- get :: Get Int8
             return $ mkCell (fromIntegral v) (fromIntegral plInd)
