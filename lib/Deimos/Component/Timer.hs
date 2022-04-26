module Deimos.Component.Timer where
import Apecs

newtype Timer = Timer Double
  deriving (Eq, Show, Generic)

instance Component Timer where
  type Storage Timer = Global Timer

instance Semigroup Timer where
  _ <> s2 = s2 -- FIXME: this is law breaking kek

instance Monoid Timer where
  mempty = Timer 0

increaseTimer :: (Has w IO Timer) => Double -> System w ()
increaseTimer t' = global $~ (\(Timer t) -> Timer $ t + t')

resetTimer :: (Has w IO Timer) => System w ()
resetTimer = Apecs.set global (Timer 0)