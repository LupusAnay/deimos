module Event where

import World
import qualified SDL
import Apecs
import Control.Monad.IO.Class
import qualified Data.Text as T

-- Handle the entire event payload
handlePayload :: [SDL.EventPayload] -> System World ()
handlePayload = mapM_ handleEvent

-- The main event handler function for dealing with keypresses
handleEvent :: SDL.EventPayload -> System World ()
handleEvent (SDL.KeyboardEvent ev) = handleKeyEvent ev
handleEvent _ = pure ()

-- For the handling keyboard events only
handleKeyEvent :: SDL.KeyboardEventData -> System World ()
handleKeyEvent ev = do
  (state :: GameState) <- get global
  let code = SDL.keysymKeycode $ SDL.keyboardEventKeysym ev
  case SDL.keyboardEventKeyMotion ev of
    SDL.Pressed ->
      case state of
        Game -> gameAction code
    SDL.Released -> pure ()

-- For keyboard events that  take place in the game
gameAction :: SDL.Keycode -> System World ()
gameAction k =
  let intents = lookup k defaultGameIntents in
  case intents of
    Just (Navigate dir) -> navigate dir
    Just Wait -> do
      postMessage "You wait.."
    _ -> pure ()

navigate :: Direction -> System World ()
navigate Up = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x) (y - 10))
navigate Down = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x) (y + 10))
navigate Left' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x - 10) (y))
navigate Right' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x + 10) (y))

postMessage :: (MonadIO m) => T.Text -> m ()
postMessage t = liftIO $ putStrLn $ T.unpack t

-- Initial bindings for intents
defaultGameIntents :: [(SDL.Keycode, GameIntent)]
defaultGameIntents =
  [ (SDL.KeycodeUp , Navigate Up)
  , (SDL.KeycodeLeft , Navigate Left')
  , (SDL.KeycodeDown , Navigate Down)
  , (SDL.KeycodeRight , Navigate Right')
  , (SDL.KeycodeW, Wait)
  ]