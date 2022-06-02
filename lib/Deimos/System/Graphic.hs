module Deimos.System.Graphic (
  draw,
  loadTextures,
  TileSize (..),
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
  liftIO $ renderMapTiles renderer texs tiles
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

renderMapTiles :: SDL.Renderer -> Textures -> Tiles -> IO ()
renderMapTiles renderer (Textures tex) tiles = do
  let tileTex = fromJust $ HM.lookup "lego" tex

  SDL.textureBlendMode tileTex SDL.$= SDL.BlendAlphaBlend
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend

  renderTiles tiles $ \tile@Tile{..} -> do
    let tileRect = toRectangle tile
    renderTexture renderer tileTex tileRect
    SDL.rendererDrawColor renderer SDL.$= toV4 tColor
    SDL.fillRect renderer (Just tileRect)
