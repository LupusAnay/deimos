module Deimos.System.Graphic (
  draw,
  loadTextures,
  fillCircle,
  TileSize (..),
  tileSize,
) where

import Apecs (Has, SystemT, cmapM_, get, global)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Deimos.Component
import Deimos.Utils (toCIntV2)
import Foreign.C
import qualified SDL
import SDL.Image (loadTexture)
import System.FilePath
import Deimos.Component.Tiles

loadTextures :: SDL.Renderer -> [FilePath] -> IO Textures
loadTextures r = fmap (Textures . HM.fromList) . traverse getTex
  where
    getTex p = do
      tex <- loadTexture r p
      pure (toS $ dropExtension $ takeFileName p, tex)

draw :: (Has World IO Textures) => SDL.Renderer -> SystemT World IO ()
draw renderer = do
  texs <- Apecs.get global
  tiles <- Apecs.get global
  liftIO $ renderMapTiles renderer tileSize texs tiles
  cmapM_ (\(p@(Position _), n :: Name) -> liftIO $ renderCharacter renderer texs p n)
  cmapM_ (\(Player, n :: Name, p :: Position) -> liftIO $ renderPlayer renderer texs p n)
  pure ()

renderPlayer :: SDL.Renderer -> Textures -> Position -> Name -> IO ()
renderPlayer = renderCharacter

renderCharacter :: SDL.Renderer -> Textures -> Position -> Name -> IO ()
renderCharacter r (Textures tex) pos name = do
  let playerTexture = HM.lookup "pacman" tex
  case playerTexture of
    Just texture -> renderTexture r texture $ getCharacterRectangle pos
    Nothing -> renderRect r pos name

renderTexture :: SDL.Renderer -> SDL.Texture -> SDL.Rectangle CInt -> IO ()
renderTexture r texture rect =
    SDL.copy r texture Nothing (Just rect)

renderRect :: SDL.Renderer -> Position -> Name -> IO ()
renderRect r v _name = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0
  SDL.fillRect r (Just $ getCharacterRectangle v)

getCharacterRectangle :: Position -> SDL.Rectangle CInt
getCharacterRectangle (Position v) = SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 50 50))



renderMapTiles :: SDL.Renderer -> TileSize -> Textures -> Tiles -> IO ()
renderMapTiles renderer (TileSize tSize) (Textures tex) (Tiles ts) = do
  let tileTex = fromJust $ HM.lookup "lego" tex

  SDL.textureBlendMode tileTex SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend

  for_ (zip [0 ..] ts) $ \(x, row) ->
    for_ (zip [0 ..] row) $ \(y, tColor) -> do
      let pos = SDL.V2 (fromIntegral $ x * tSize) (fromIntegral $ y * tSize)
          size = SDL.V2 (fromIntegral tSize) (fromIntegral tSize)
          tileRect = SDL.Rectangle (SDL.P $ toCIntV2 pos) (toCIntV2 size)
      renderTexture renderer tileTex tileRect
      SDL.rendererDrawColor renderer SDL.$= toV4 tColor
      SDL.fillRect renderer (Just tileRect)

renderMapElement :: SDL.Renderer -> MapElement -> Position -> IO ()
renderMapElement r Rect (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 120 0 0 0
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 30 30))))
renderMapElement r Circle (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 120 0 0
  fillCircle r v 40

fillCircle :: SDL.Renderer -> SDL.V2 Double -> Double -> IO ()
fillCircle renderer center r
  | r <= 1 = pure ()
  | otherwise = do
    drawCircle renderer center r
    -- SDL.drawLine renderer (SDL.P $ toCIntV2 p2) (SDL.P $ toCIntV2 center)
    fillCircle renderer center (r - 1)

drawCircle :: SDL.Renderer -> SDL.V2 Double -> Double -> IO ()
drawCircle renderer (SDL.V2 x y) r = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 255 0
  let points = map (\t -> SDL.V2 (x + r * cos t) (y + r * sin t)) [0, 1 / r .. 2 * pi]
  for_ points $ \p2 -> do
    SDL.drawPoint renderer (SDL.P $ toCIntV2 p2)
