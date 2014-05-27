{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Data.Cell where

import Control.Lens
import Data.Binary
import Data.Int
import Data.Monoid


data Cell = Cell { _value :: Int
                 , _playerIndex :: Int }
  deriving (Show)

makeLenses ''Cell

-- needed for ix on array 
instance Monoid Cell where
  mempty = undefined
  mappend = undefined

mkCell v plInd = Cell { _value = v, _playerIndex = plInd }

isFree :: Cell -> Bool
isFree Cell{ _value = v, _playerIndex = pi }
    | v == 0
    = True
    | pi == 0
    = True
    | otherwise
    = False

isOwnedBy :: Int -> Cell -> Bool
isOwnedBy playerInd Cell{ _playerIndex = pi } 
    | pi == playerInd
    = True
    | otherwise
    = False

instance Binary Cell where
    put c = do put (fromIntegral $ c ^. value :: Int8)
               put (fromIntegral $ c ^. playerIndex :: Int8)
    get = do v <- get :: Get Int8
             pi <- get :: Get Int8
             return $ mkCell (fromIntegral v) (fromIntegral pi)
