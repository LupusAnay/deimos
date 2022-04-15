module Deimos.System.Graphic (
  draw,
  loadTextures,
) where

import Apecs (Has, SystemT, cmapM_, get, global)
import qualified Data.HashMap.Strict as HM
import Deimos.Component (MapElement (..), Name, Position (..), Textures (..), World, Player (Player))
import Deimos.Utils (toCIntV2)
import Foreign.C
import qualified SDL
import SDL.Image (loadTexture)
import System.FilePath

loadTextures :: SDL.Renderer -> [FilePath] -> IO Textures
loadTextures r = fmap (Textures . HM.fromList) . traverse getTex
  where
    getTex p = do
      tex <- loadTexture r p
      pure (toS $ dropExtension $ takeFileName p, tex)

draw :: (Has World IO Textures) => SDL.Renderer -> SystemT World IO ()
draw renderer = do
  texs <- Apecs.get global
  cmapM_ (\(p@(Position _), n :: Name) -> liftIO $ renderCharacter renderer texs p n)
  cmapM_ (\(Player, n :: Name, p :: Position) -> liftIO $ renderPlayer renderer texs p n)
  cmapM_ (\(p@(Position _), el :: MapElement) -> liftIO $ renderMapElement renderer el p)
  pure ()

renderPlayer = renderCharacter

renderCharacter :: SDL.Renderer -> Textures -> Position -> Name -> IO ()
renderCharacter r (Textures tex) pos name = do
  let playerTexture = HM.lookup "pacman" tex
  case playerTexture of
    Just texture -> renderTexture r texture pos
    Nothing -> renderRect r pos name

renderTexture :: SDL.Renderer -> SDL.Texture -> Position -> IO ()
renderTexture r texture v = do
  SDL.copy r texture Nothing (Just $ getCharacterRectangle v)

renderRect :: SDL.Renderer -> Position -> Name -> IO ()
renderRect r v _name = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0
  SDL.fillRect r (Just $ getCharacterRectangle v)

getCharacterRectangle :: Position -> SDL.Rectangle CInt
getCharacterRectangle (Position v) = SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 50 50))

renderMapElement :: SDL.Renderer -> MapElement -> Position -> IO ()
renderMapElement r Rect (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 120 0 0 0
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 30 30))))
renderMapElement r Circle (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 120 0 0
  fillCircle r v 40

fillCircle :: (MonadIO m) => SDL.Renderer -> SDL.V2 Double -> Double -> m ()
fillCircle renderer center@(SDL.V2 x y) r
  | r <= 1 = pure ()
  | otherwise = do
    let points = map (\t -> SDL.V2 (x + r * cos t) (y + r * sin t)) [0, 2 * pi / (2 * pi * r) .. 2 * pi]
    for_ points $ \p2 -> do
      SDL.drawPoint renderer (SDL.P $ toCIntV2 p2)
      SDL.drawLine renderer (SDL.P $ toCIntV2 p2) (SDL.P $ toCIntV2 center)
    fillCircle renderer center (r - 1)
