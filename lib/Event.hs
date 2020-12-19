module Event where

import Apecs
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import qualified SDL
import World

handlePayload :: [SDL.EventPayload] -> System World ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System World ()
handleEvent _ev = do
  pure ()

handleKeyEvent :: System World ()
handleKeyEvent = do
  (state :: GameState) <- get global
  keyboardState <- SDL.getKeyboardState

  let intents = mapMaybe (\(k, i) -> if keyboardState k then Just i else Nothing) defaultGameIntents
  gameAction intents

gameAction :: [GameIntent] -> System World ()
gameAction intents = do
  for_ intents $ \intent ->
    case intent of
      (Navigate dir) -> navigate dir
      (Wait) -> postMessage "You wait.."

navigate :: Direction -> System World ()
navigate Up = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x) (y - singleStep))
navigate Down = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x) (y + singleStep))
navigate Left' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x - singleStep) (y))
navigate Right' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x + singleStep) (y))

singleStep :: Double
singleStep = 10 / 1000

postMessage :: (MonadIO m) => T.Text -> m ()
postMessage t = liftIO $ putStrLn $ T.unpack t

-- Initial bindings for intents
defaultGameIntents :: [(SDL.Scancode, GameIntent)]
defaultGameIntents =
  [ (SDL.ScancodeW, Navigate Up),
    (SDL.ScancodeA, Navigate Left'),
    (SDL.ScancodeS, Navigate Down),
    (SDL.ScancodeD, Navigate Right'),
    (SDL.ScancodeUp, Navigate Up),
    (SDL.ScancodeLeft, Navigate Left'),
    (SDL.ScancodeDown, Navigate Down),
    (SDL.ScancodeRight, Navigate Right')
    -- , (SDL.KeycodeW, Wait)
  ]