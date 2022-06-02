{-# LANGUAGE RecordWildCards #-}

module Deimos.Component.Tiles where

import Apecs
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Deimos.Component.Position
import Deimos.Component.ScreenSize
import Deimos.Utils
import Foreign.C
import Lens.Micro.Platform
import Linear
import qualified SDL

data Color = Color Int Int Int Int
  deriving (Eq, Show, Generic)

toV4 :: Color -> V4 Word8
toV4 (Color r g b a) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

data Tiles = Tiles
  { rowSize :: Int
  , tiles :: V.Vector Tile
  }
  deriving (Eq, Show, Generic)

data Tile = Tile
  { tColor :: Color
  , tPosition :: Position
  , tSize :: TileSize
  }
  deriving (Eq, Show, Generic)

toRectangle :: Tile -> SDL.Rectangle CInt
toRectangle Tile{..} = SDL.Rectangle (SDL.P $ toCIntV2 pos) (toCIntV2 size)
  where
    (Position pos) = tPosition
    ts = unSize tSize
    size = SDL.V2 ts ts

generateTiles :: (Monad m) => ScreenSize -> TileSize -> m Tiles
generateTiles ScreenSize{..} tSize = do
  Tiles maxWidth <$> tiles
  where
    color = Color 255 255 255 0
    tiles = V.generateM (maxWidth * maxHeight) $ \i ->
      pure $ Tile color (getPosition i) tSize
    maxHeight = height `div` unSize tSize
    maxWidth = width `div` unSize tSize
    getPosition i =
      let y = i `div` maxWidth
          x = i `rem` maxWidth
       in position (fromIntegral $ x * ts) (fromIntegral $ y * ts)
    ts = unSize tSize

newtype TileUpdates = TileUpdates [(Tile -> Tile, V2 Int)]

bulkUpdateTilesMutable :: Tiles -> TileUpdates -> IO Tiles
bulkUpdateTilesMutable (Tiles width ts) (TileUpdates updates) = do
  Tiles width <$> update
  where
    update :: IO (V.Vector Tile)
    update = do
      mut <- V.thaw ts
      for_ updates $ \(up, V2 x y) ->
        if x >= width || x < 0
           then pure ()
           else do
             let pos = max 0 (min (VM.length mut - 1) (y * width + x))
             VM.modify mut up pos
      V.freeze mut


bulkUpdateTiles :: Tiles -> TileUpdates -> Tiles
bulkUpdateTiles (Tiles width ts) (TileUpdates updates) = Tiles width updated
  where
    updates' = map (\(up, V2 x y) -> (normalizeIx $ calculate x y, up)) updates
    normalizeIx index = max 0 $ min (V.length ts - 1) index
    calculate x y
      | x >= width = y * width
      | x < 0 = 0
      | otherwise = y * width + x
    updated =
      V.accum (\tile update -> update tile) ts updates'

renderTiles :: Monad m => Tiles -> (Tile -> m ()) -> m ()
renderTiles (Tiles _ ts) = V.forM_ ts

maxTilesWidth :: ScreenSize -> TileSize -> Double
maxTilesWidth (ScreenSize (fromIntegral -> w) (fromIntegral -> h)) (TileSize (fromIntegral -> ts)) = w / ts

instance Semigroup Tiles where
  (<>) = mappend

instance Monoid Tiles where
  mempty = Tiles 0 mempty

instance Component Tiles where
  type Storage Tiles = Global Tiles

newtype TileSize = TileSize
  { unSize :: Int
  }
  deriving (Eq, Show, Generic)

defaultTileSize :: TileSize
defaultTileSize = TileSize 40

toTilePosition :: V2 Double -> V2 Int
toTilePosition (V2 x y) = toIntV2 $ V2 (x / fromIntegral ts) (y / fromIntegral ts)
  where
    ts = unSize defaultTileSize
