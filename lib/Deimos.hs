module Deimos
  ( module Event,
    module Graphic,
    module World,
    initialize,
    step,
  )
where

import Apecs
import Data.Maybe
import Event
import Graphic
import qualified SDL
import World

step :: Double -> System World ()
step dT = do
  everyoneChasePlayer
  pure ()

everyoneChasePlayer :: System World ()
everyoneChasePlayer = do
  (Player, Position p) <- fromJust <$> cfold (\_ p@(Player, Position _) -> Just p) Nothing
  cmap (\(Position p', Not :: Not Player) -> Position $ moveTowards p' p)

moveTowards :: SDL.V2 Double -> SDL.V2 Double -> SDL.V2 Double
moveTowards p1 p2 = p1 + step
  where
    step = SDL.normalize (p2 - p1) / 400 -- 2.5 pixels per second (with 1000 tickrate)

initialize :: Textures -> System World ()
initialize texs = do
  set global $ Time 0
  set global $ texs

  newEntity (Player, Name "Lola", Position $ SDL.V2 0 0)
  newEntity (Name "Harold", Position $ SDL.V2 0 0)
  pure ()