module Graphic where

import Apecs hiding (($=))
import qualified Data.HashMap.Strict as HM
import qualified SDL as SDL
import SDL.Image hiding (load)
import World

loadTextures :: SDL.Renderer -> [FilePath] -> IO Textures
loadTextures r = (fmap (Textures . HM.fromList) . traverse getTex)
  where
    getTex p = do
      tex <- loadTexture r p
      pure (p, tex)

draw :: (Has World IO Textures) => SDL.Renderer -> Int -> SystemT World IO ()
draw renderer fps = do
  Textures texs <- get global
  Fonts fonts <- get global
  cmapM_ (\(p@(Position _), Name n) -> liftIO $ renderRect renderer texs p n)
  pure ()

drawComponents :: (Position -> IO ()) -> SystemT World IO ()
drawComponents f = cmapM_ (\p -> liftIO $ f p)

renderRect :: SDL.Renderer -> TextureMap -> Position -> String -> IO ()
renderRect r ts (Position v) name = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0
  SDL.fillRect r (Just (SDL.Rectangle (SDL.P $ toCIntV2 v) (toCIntV2 $ (SDL.V2 50 50))))
  putStrLn $ "Rendering " <> name <> " with position: " <> show v
  where
    toCIntV2 (SDL.V2 a b) = SDL.V2 (fromIntegral $ fromEnum a) (fromIntegral $ fromEnum b)
