module SWOSShell
where

import Graphics.Rendering.OpenGL as OpenGL

import qualified Swos
import Player
import Drawing
import FVector

swosPositionToPosition :: Swos.SWOSPosition -> PlPosition
swosPositionToPosition p 
  | Swos.isGoalkeeper p = Goalkeeper
  | Swos.isDefender   p = Defender
  | Swos.isMidfielder p = Midfielder
  | otherwise           = Attacker

swosPlayerToPlayer :: Float -> Bool -> TextureObject -> TextureObject -> FRange -> FRange -> Swos.SWOSPlayer -> Player
swosPlayerToPlayer inz home htex atex hsize (px, py) p = 
  Player (px - 10, py / 2) (ImageInfo tex hsize) inz ((Swos.plnumber p), home) npos
    where tex = if home then htex else atex
          npos = swosPositionToPosition (Swos.plposition p)


