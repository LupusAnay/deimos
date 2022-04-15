module Deimos.System.Graphic (
  draw,
  loadTextures,
) where

import Apecs (Has, SystemT, cmapM_, get, global)
import qualified Data.HashMap.Strict as HM
import Deimos.Component (MapElement (..), Name, Position (..), Textures (..), World)
import Deimos.Utils (toCIntV2)
import qualified SDL
import SDL.Image (loadTexture)

loadTextures :: SDL.Renderer -> [FilePath] -> IO Textures
loadTextures r = fmap (Textures . HM.fromList) . traverse getTex
  where
    getTex p = do
      tex <- loadTexture r p
      pure (toS p, tex)

draw :: (Has World IO Textures) => SDL.Renderer -> SystemT World IO ()
draw renderer = do
  texs <- Apecs.get global
  cmapM_ (\(p@(Position _), n :: Name) -> liftIO $ renderCharacter renderer texs p n)
  cmapM_ (\(p@(Position _), el :: MapElement) -> liftIO $ renderMapElement renderer el p)
  pure ()

renderCharacter :: SDL.Renderer -> Textures -> Position -> Name -> IO ()
renderCharacter r tex (Position v) name = undefined

renderRect :: SDL.Renderer -> Textures -> Position -> Text -> IO ()
renderRect r _ts (Position v) name = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 50 50))))

-- putStrLn $ "Rendering " <> name <> " with position: " <> show v

renderMapElement :: SDL.Renderer -> MapElement -> Position -> IO ()
renderMapElement r Rect (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 120 0 0 0
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 (SDL.V2 30 30))))
renderMapElement r Circle (Position v) = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 120 0 0
  fillCircle r v 40

-- renderMapElement r Line (Position v) = do
--   SDL.renderLi

fillCircle :: (MonadIO m) => SDL.Renderer -> SDL.V2 Double -> Double -> m ()
fillCircle renderer center@(SDL.V2 x y) r
  | r <= 1 = pure ()
  | otherwise = do
    let points = map (\t -> SDL.V2 (x + r * cos t) (y + r * sin t)) [0, 2 * pi / (2 * pi * r) .. 2 * pi]
    for_ points $ \p2 -> do
      SDL.drawPoint renderer (SDL.P $ toCIntV2 p2)
      SDL.drawLine renderer (SDL.P $ toCIntV2 p2) (SDL.P $ toCIntV2 center)
    fillCircle renderer center (r - 1)
