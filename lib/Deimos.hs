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
import Debug.Trace

step :: Double -> System World ()
step dT = do
  everyoneChasePlayer
  pure ()

everyoneChasePlayer :: System World ()
everyoneChasePlayer = do
  (Player, Position p) <- fromJust <$> cfold (\_ p@(Player, Position _) -> Just p) Nothing
  cmap (\(Position p', Not :: Not Player) -> Position $ moveTowards p' p)

moveTowards :: SDL.V2 Double -> SDL.V2 Double ->  SDL.V2 Double
moveTowards p1 p2 = p1 + (single_len / 400) -- Move to 1/400 per tick
  where
    diff = p2 - p1
    single_len = fmap (\a -> if a == 0 then a else a / len) diff
    len = let (SDL.V2 x y) = diff in sqrt $ x^2 + y^2


initialize :: Textures -> System World ()
initialize texs = do
  liftIO $ putStrLn "Let's do some IO just to prove we can!"

  set global $ Time 0
  set global $ texs

  newEntity (Player, Name "Lola", Position $ SDL.V2 0 0)
  newEntity (Name "Harold", Position $ SDL.V2 0 0)
  pure ()