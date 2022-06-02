module Deimos.Component.Position where

import Apecs
import qualified Linear as SDL

newtype Position = Position (SDL.V2 Double)
  deriving (Show, Eq, Generic)

instance Component Position where
  type Storage Position = Apecs.Map Position

position :: Double -> Double -> Position
position x y = Position $ SDL.V2 x y
