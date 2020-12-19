{-# LANGUAGE LambdaCase #-}
module Deimos.System.Event
  ( handlePayload,
    handleKeyEvent,
  )
where

import Apecs (System, cmap, get, global)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Deimos.Component
  ( GameState,
    Player (Player),
    Position (Position),
    World,
  )
import qualified SDL

data Direction
  = Up
  | Down
  | Left'
  | Right'

data GameIntent
  = Navigate Direction
  | Wait


handlePayload :: [SDL.EventPayload] -> System World ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System World ()
handleEvent _ev = do
  pure ()

handleKeyEvent :: Double -> System World ()
handleKeyEvent dT = do
  (_state :: GameState) <- get global
  keyboardState <- SDL.getKeyboardState

  let intents = mapMaybe (\(k, i) -> if keyboardState k then Just i else Nothing) defaultGameIntents
  gameAction dT intents

gameAction :: Double -> [GameIntent] -> System World ()
gameAction dT intents = do
  for_ intents $ \case
    (Navigate dir) -> navigate dT dir
    Wait -> postMessage "You wait.."

navigate :: Double -> Direction -> System World ()
navigate dT Up = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 x (y - singleStep dT))
navigate dT Down = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 x (y + singleStep dT))
navigate dT Left' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x - singleStep dT) y)
navigate dT Right' = cmap (\(Position (SDL.V2 x y), Player) -> Position $ SDL.V2 (x + singleStep dT) y)

singleStep :: Double -> Double
singleStep dT = 0.1 * dT

postMessage :: (MonadIO m) => T.Text -> m ()
postMessage t = liftIO $ putStrLn $ T.unpack t

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
  ]
