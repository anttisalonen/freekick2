module Match.SWOSShell
where

import Graphics.Rendering.OpenGL as OpenGL

import qualified Swos
import Drawing
import FVector

import Match.Player

swosPositionToPosition :: Swos.SWOSPosition -> PlPosition
swosPositionToPosition p 
  | Swos.isGoalkeeper p = Goalkeeper
  | Swos.isDefender   p = Defender
  | Swos.isMidfielder p = Midfielder
  | otherwise           = Attacker

swosSkillToSkill :: Int -> Skill
swosSkillToSkill = (/1000) . fromIntegral  . (*60) . (+1)

swosPlayerToPlayer :: Float -> Bool -> TextureObject -> TextureObject -> ImageInfo -> FRange -> FRange -> Swos.SWOSPlayer -> Player
swosPlayerToPlayer inz home htex atex plshimg hsize (px, py) p = 
  Player (px - 10, py / 2) (ImageInfo tex hsize) plshimg inz ((Swos.plnumber p), home) npos 0 plskills
    where tex = if home then htex else atex
          npos = swosPositionToPosition (Swos.plposition p)
          plskills = PlayerSkills shoots passs speeds
          shoots = swosSkillToSkill $ Swos.skshooting $ Swos.plskills p
          passs = swosSkillToSkill $ Swos.skpassing $ Swos.plskills p
          speeds = swosSkillToSkill $ Swos.skspeed $ Swos.plskills p
