module World where

import Apecs
import qualified SDL
import qualified Data.HashMap.Strict as HM
import SDL.Font

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

data Direction
  = Up
  | Down
  | Left'
  | Right'

data GameIntent
  = Navigate Direction
  | Wait

instance Component Position where
  type Storage Position = Map Position

newtype Name = Name String deriving (Show)

instance Component Name where
  type Storage Name = Map Name

data GameState = Game
instance Semigroup GameState where (<>) = mappend
instance Monoid GameState where mempty = Game
instance Component GameState where type Storage GameState = Global GameState

type TexResource = (String, SDL.Texture)
type TextureMap = HM.HashMap String SDL.Texture

newtype Textures = Textures TextureMap
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

type FontResource = (String, Font)
type FontMap = HM.HashMap String Font

newtype Fonts = Fonts FontMap
instance Component Fonts where type Storage Fonts = Global Fonts
instance Semigroup Fonts where (<>) = mappend
instance Monoid Fonts where mempty = Fonts HM.empty

makeWorld "World" [''Time, ''Player, ''Position, ''Name, ''GameState, ''Fonts, ''Textures]