module Deimos.Component.Tiles where

import Apecs
import Linear
import Deimos.Utils

data Color = Color Int Int Int Int
  deriving (Eq, Show, Generic)

toV4 :: Color -> V4 Word8
toV4 (Color r g b a) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

newtype Tiles = Tiles [[Color]]
  deriving (Eq, Show, Generic)

instance Semigroup Tiles where
  (<>) = mappend

instance Monoid Tiles where
  mempty = Tiles mempty

instance Component Tiles where
  type Storage Tiles = Global Tiles

newtype TileSize = TileSize Int
  deriving (Eq, Show, Generic)

tileSize :: TileSize
tileSize = TileSize 40

tSize :: Int
tSize = let (TileSize t) = tileSize in t

toTilePosition :: V2 Double -> V2 Int
toTilePosition (V2 x y) = toIntV2 $ V2 (x / fromIntegral tSize) (y / fromIntegral tSize)