import Apecs
import Control.Monad
import Deimos
import qualified SDL
import qualified SDL.Font

main :: IO ()
main = do
  world <- initWorld

  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  window <- SDL.createWindow "App" $ SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
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

  let loop prevTicks secondTick fpsAcc prevFps = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            dt = ticks - prevTicks
            calcFps = secondTick + dt > 1000
            newFps = if calcFps then fpsAcc + 1 else prevFps
            newFpsAcc = if calcFps then 1 else fpsAcc + 1
            newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt

        runSystem (handlePayload payload) world

        runSystem (step $ fromIntegral dt) world

        SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 0
        SDL.clear renderer

        runSystem (draw renderer newFps) world

        SDL.present renderer
        unless quit $ loop ticks newSecondTick newFpsAcc newFps

  loop 0 0 0 0
