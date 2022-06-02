module Deimos (
  app,
) where

import Apecs
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Deimos.Component
import Deimos.Component.ScreenSize
import Deimos.Component.Tiles
import Deimos.Component.Timer
import Deimos.System.Event (handleKeyEvent, handlePayload)
import Deimos.System.Graphic
import Deimos.Utils
import Lens.Micro ((.~))
import Linear hiding (trace)
import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Framerate

step :: Double -> System World ()
step dT = do
  manager <- SDL.Framerate.manager
  framerate <- liftIO $ SDL.Framerate.get manager
  when (dT >= 10) $ liftIO $ putText $ "FPS: " <> show framerate <> "; DT: " <> show dT
  everyoneChasePlayer dT
  updateWaves dT
  increaseTimer dT
  pure ()

updateWaves :: Double -> System World ()
updateWaves dT = do
  screenSize <- Apecs.get global
  (Timer t) <- Apecs.get global

  let maxR = maxTilesWidth screenSize defaultTileSize

  when (t > 1500) $ do
    cmapM (\(Player, pos :: Position) -> newEntity_ (Wave 1, pos))
    resetTimer

  -- Paint tiles
  cmapM (\(Wave r, tiles :: Tiles, Position pos) -> liftIO $ paintWavedTiles (toTilePosition pos) (round r) tiles)

  -- Increase radius and delete big waves
  cmap (\(Wave radius) -> if radius >= maxR then Nothing else Just $ Wave $ radius + 0.03 * dT)

paintWavedTiles :: V2 Int -> Int -> Tiles -> IO Tiles
paintWavedTiles center radius tiles =
  -- bulkUpdateTilesMutable tiles (TileUpdates circlePoints)
  pure $ bulkUpdateTiles tiles (TileUpdates circlePoints)
  where
    blue a = Color 0 0 255 a
    waveWidth = 3
    -- updates = TileUpdates $ map (\(pos, col) -> (#tColor .~ col, pos)) circlePoints
    circlePoints =
      mconcat
        [ zip (repeat (#tColor .~ blue ((50 * waveWidth) - abs (50 * aCoef)))) (generateCirclePoints' center (radius + aCoef))
        | aCoef <- [- waveWidth .. waveWidth]
        ]

everyoneChasePlayer :: Double -> System World ()
everyoneChasePlayer dT = do
  (Player, Position p) <- fromJust <$> cfold (\_ p@(Player, Position _) -> Just p) Nothing
  cmap (\(Position p', Name _, Not :: Not Player) -> Position $ moveTowards dT p' p)

moveTowards :: Double -> SDL.V2 Double -> SDL.V2 Double -> SDL.V2 Double
moveTowards dT p1 p2 = p1 + (delta * pure dT)
  where
    delta = SDL.normalize (p2 - p1) / 20

initialize :: Textures -> ScreenSize -> System World ()
initialize texs screenSize = do
  set global $ Time 0
  set global texs
  set global screenSize
  set global $ Timer 0

  _ <- newEntity (Player, Name "Lola", position 1700 1000)
  -- _ <- newEntity (Name "Harold", position 0 0)
  -- _ <- newEntity (Wave 1, position 20 20)
  gameMap
  pure ()

gameMap :: System World ()
gameMap = do
  screenSize <- Apecs.get global
  tiles <- generateTiles screenSize defaultTileSize
  set global tiles
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
        , SDL.windowHighDPI = True
        , SDL.windowInitialSize = SDL.V2 1700 1000
        , SDL.windowResizable = True
        }

  (SDL.V2 w h) <- SDL.glGetDrawableSize window
  let size = ScreenSize (fromIntegral w) (fromIntegral h)

  print size
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

  texs <- liftIO $ loadTextures renderer ["res/pacman.png", "res/lego.png"]

  runSystem (initialize texs size) world

  SDL.showWindow window

  loop world renderer 0
