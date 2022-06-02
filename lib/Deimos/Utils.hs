module Deimos.Utils (
  toCIntV2',
  toCIntV2,
  generateCirclePoints,
  generateCirclePoints',
  toIntV2,
) where

import Foreign.C (CInt)
import GHC.Float
import Linear (V2 (V2))

toCIntV2 :: (Num a1, Enum a2) => V2 a2 -> V2 a1
toCIntV2 (V2 a b) = V2 (fromIntegral $ fromEnum a) (fromIntegral $ fromEnum b)

toCIntV2' :: V2 Double -> V2 CInt
toCIntV2' (V2 a b) = V2 (fromIntegral $ double2Int a) (fromIntegral $ double2Int b)

toIntV2 :: (RealFrac a) => V2 a -> V2 Int
toIntV2 (V2 a b) = V2 (round a) (round b)

generateCirclePoints' :: V2 Int -> Int -> [V2 Int]
generateCirclePoints' (V2 x0 y0) radius =
  map (\t -> V2 (round (x + r * cos t)) (round (y + r * sin t))) [0, 1 / r .. 2 * pi]
  where
    r = fromIntegral @_ @Double radius
    x = fromIntegral @_ @Double x0
    y = fromIntegral @_ @Double y0

generateCirclePoints :: V2 Int -> Int -> [V2 Int]
generateCirclePoints (V2 x0 y0) radius =
  -- Four initial points, plus the generated points
  V2 x0 (y0 + radius) : V2 x0 (y0 - radius) : V2 (x0 + radius) y0 : V2 (x0 - radius) y0 : points
  where
    -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
    points = concatMap generatePoints $ unfoldr step initialValues
    generatePoints (x, y) =
      [V2 (xop x0 x') (yop y0 y') | V2 x' y' <- [V2 x y, V2 y x], xop <- [(+), (-)], yop <- [(+), (-)]]

    -- The initial values for the loop
    initialValues = (1 - radius, 1, (-2) * radius, 0, radius)

    -- One step of the loop. The loop itself stops at Nothing.
    step (f, ddf_x, ddf_y, x, y)
      | x >= y = Nothing
      | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
      where
        (f', ddf_y', y')
          | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
          | otherwise = (f + ddf_x, ddf_y, y)
        ddf_x' = ddf_x + 2
        x' = x + 1
