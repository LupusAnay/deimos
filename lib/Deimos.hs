module Deimos
  ( app,
  )
where

import Apecs
  ( Not (..),
    System,
    cfold,
    cmap,
    global,
    newEntity,
    runSystem,
    set,
  )
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromJust)
import Data.Word (Word32)
import Deimos.Component
  ( MapElement (..),
    Name (Name),
    Player (..),
    Position (Position),
    Textures,
    Time (Time),
    World,
    initWorld,
    position,
  )
import Deimos.System.Event ( handleKeyEvent, handlePayload )
import Deimos.System.Graphic ( draw, loadTextures )
import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Framerate

step :: Double -> System World ()
step dT = do
  manager <- SDL.Framerate.manager
  framerate <- liftIO $ SDL.Framerate.get manager
  when (dT >= 10) $ liftIO $ putStrLn $ "FPS: " <> show framerate <> "; DT: " <> show dT
  everyoneChasePlayer dT
  pure ()

everyoneChasePlayer :: Double -> System World ()
everyoneChasePlayer dT = do
  (Player, Position p) <- fromJust <$> cfold (\_ p@(Player, Position _) -> Just p) Nothing
  cmap (\(Position p', Name _, Not :: Not Player) -> Position $ moveTowards dT p' p)

moveTowards :: Double -> SDL.V2 Double -> SDL.V2 Double -> SDL.V2 Double
moveTowards dT p1 p2 = p1 + (step * pure dT)
  where
    step = SDL.normalize (p2 - p1) / 20

initialize :: Textures -> System World ()
initialize texs = do
  set global $ Time 0
  set global texs

  newEntity (Player, Name "Lola", position 0 0)
  newEntity (Name "Harold", position 0 0)
  gameMap
  pure ()

gameMap :: System World ()
gameMap = do
  newEntity (Rect, position 10 10)
  newEntity (Circle, position 150 150)
  pure ()

loop :: World -> SDL.Renderer -> Word32 -> IO ()
loop world renderer prevTicks = do
  ticks <- SDL.ticks
  payload <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = SDL.QuitEvent `elem` payload
      dt = fromIntegral $ ticks - prevTicks

  runSystem (handlePayload payload) world
  runSystem (handleKeyEvent dt) world
  runSystem (step dt) world

  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 0
  SDL.clear renderer

  runSystem (draw renderer) world

  SDL.present renderer
  unless quit $ loop world renderer ticks

app :: IO ()
app = do
  world <- initWorld

  SDL.initialize [SDL.InitVideo]
  SDLF.initialize

  window <-
    SDL.createWindow "App" $
      SDL.defaultWindow
        { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        }

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer,
          SDL.rendererTargetTexture = False
        }

  texs <- liftIO $ loadTextures renderer []

  runSystem (initialize texs) world

  SDL.showWindow window

  loop world renderer 0
