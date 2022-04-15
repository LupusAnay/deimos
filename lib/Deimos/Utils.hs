module Deimos.Utils (toCIntV2', toCIntV2) where

import Foreign.C (CInt)
import GHC.Float
import Linear (V2 (V2))

toCIntV2' :: (Num a1, Enum a2) => V2 a2 -> V2 a1
toCIntV2' (V2 a b) = V2 (fromIntegral $ fromEnum a) (fromIntegral $ fromEnum b)

toCIntV2 :: V2 Double -> V2 CInt
toCIntV2 (V2 a b) = V2 (fromIntegral $ double2Int a) (fromIntegral $ double2Int b)