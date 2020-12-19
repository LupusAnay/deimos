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

  let loop prevTicks = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            dt = ticks - prevTicks

        -- runSystem (handlePayload payload) world
        runSystem handleKeyEvent world
        runSystem (step $ fromIntegral dt) world

        SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 0
        SDL.clear renderer

        runSystem (draw renderer) world

        SDL.present renderer
        unless quit $ loop ticks

  loop 0