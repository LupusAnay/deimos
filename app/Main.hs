{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Graphics.Gloss
import Linear (V2 (..))
import Graphics.Gloss.Interface.IO.Simulate

newtype Position = Position (V2 Double) deriving (Show)

newtype Velocity = Velocity (V2 Double) deriving (Show)

data Flying = Flying

makeWorldAndComponents "World" [''Position, ''Velocity, ''Flying]

initialize :: System World ()
initialize = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  cmap $ \(Position p, Velocity v) -> Position (v + p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

drawEntityInWorld :: (Position, Entity) -> Picture
drawEntityInWorld (Position (V2 x y), e) = Polygon $ [(realToFrac x, realToFrac y)]

disp = InWindow "Hello World" (640, 640) (10, 10)

simulateDisp :: Display -> System World ()
simulateDisp disp = do
  w <- ask
  liftIO $ simulateIO disp white 60 w (\_ -> drawWorld w) (\_ _ _ -> return w)

drawWorld :: World -> IO Picture
drawWorld w = pure $ Polygon [(realToFrac 1, realToFrac 1), (realToFrac 100, realToFrac 1), (realToFrac 50, realToFrac 100)]

main :: IO ()
main = initWorld >>= runSystem (initialize >> simulateDisp disp)