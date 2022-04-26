module Deimos.Component.ScreenSize where
import Apecs

data ScreenSize = ScreenSize
  { width :: Int
  , height :: Int
  }
  deriving (Eq, Show, Generic)

instance Component ScreenSize where
  type Storage ScreenSize = Global ScreenSize

instance Semigroup ScreenSize where
  _ <> s2 = s2 -- FIXME: this is law breaking kek

instance Monoid ScreenSize where
  mempty = ScreenSize { width = 0, height = 0 }
