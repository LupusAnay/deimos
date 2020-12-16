module Deimos
  ( module Event,
    module Graphic,
    module World,
    initialize,
    step,
  )
where

import Apecs
import Event
import Graphic
import qualified SDL
import World
import Data.Maybe

step :: Double -> System World ()
step dT = do
  everyoneChasePlayer
  pure ()

everyoneChasePlayer :: System World ()
everyoneChasePlayer = do
  (Player, Position p) <- fromJust <$> cfold (\_ p@(Player, Position _) -> Just p) Nothing
  cmap (\(Position p', Not :: Not Player) -> Position $ moveTowards p' p)

moveTowards :: SDL.V2 Double -> SDL.V2 Double ->  SDL.V2 Double
moveTowards p1 p2 = p1 + step
  where
    diff = p2 - p1
    single_len = SDL.signorm diff
    step = if any (isNaN) single_len then pure 0 else single_len / 400


initialize :: Textures -> System World ()
initialize texs = do
  liftIO $ putStrLn "Let's do some IO just to prove we can!"

  set global $ Time 0
  set global $ texs

  newEntity (Player, Name "Lola", Position $ SDL.V2 0 0)
  newEntity (Name "Harold", Position $ SDL.V2 0 0)
  pure ()