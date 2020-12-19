module Deimos.Component
  ( World (..),
    initWorld,
    Time (..),
    Player (..),
    Position (..),
    GameState (..),
    Name (..),
    Textures (..),
    Fonts (..),
    MapElement (..),
    position,
  )
where

import Apecs
  ( Component (..),
    Global,
    Has (..),
    Map,
    SystemT (SystemT),
    Unique,
    asks,
    explInit,
    makeWorld,
  )
import qualified Data.HashMap.Strict as HM
import qualified SDL
import SDL.Font (Font)

newtype Time = Time Double deriving (Show)

instance Semigroup Time where
  (<>) = mappend

instance Monoid Time where
  mempty = Time 0

instance Component Time where
  type Storage Time = Global Time

data Player = Player deriving (Show)

instance Component Player where
  type Storage Player = Unique Player

newtype Position = Position (SDL.V2 Double) deriving (Show)

instance Component Position where
  type Storage Position = Map Position

newtype Name = Name String deriving (Show)

instance Component Name where
  type Storage Name = Map Name

data GameState = Game

instance Semigroup GameState where (<>) = mappend

instance Monoid GameState where mempty = Game

instance Component GameState where type Storage GameState = Global GameState

type TextureMap = HM.HashMap String SDL.Texture

newtype Textures = Textures TextureMap

instance Component Textures where type Storage Textures = Global Textures

instance Semigroup Textures where (<>) = mappend

instance Monoid Textures where mempty = Textures HM.empty

type FontMap = HM.HashMap String Font

newtype Fonts = Fonts FontMap

instance Component Fonts where type Storage Fonts = Global Fonts

instance Semigroup Fonts where (<>) = mappend

instance Monoid Fonts where mempty = Fonts HM.empty

data MapElement = Rect | Circle

instance Component MapElement where
  type Storage MapElement = Map MapElement

makeWorld "World" [''Time, ''Player, ''Position, ''Name, ''GameState, ''Fonts, ''Textures, ''MapElement]

position :: Double -> Double -> Position
position x y = Position $ SDL.V2 x y