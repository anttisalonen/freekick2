module Match.SWOSShell
where

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

swosValueToSkill :: Int -> Skill
swosValueToSkill = (/1000) . fromIntegral . (*19) . (+1)

swosPlayerToPlayer :: Float 
                   -> Bool 
                   -> PlayerTextureSet
                   -> PlayerTextureSet
                   -> ImageInfo 
                   -> FRange 
                   -> FRange 
                   -> Swos.SWOSPlayer 
                   -> Player
swosPlayerToPlayer inz home htexset atexset plshimg hsize (px, py) p = 
  Player (px - 10, py / 2) hsize pltexset plshimg inz ((Swos.plnumber p), home) npos 0 psk 180
    where pltexset = if home then htexset else atexset
          npos = swosPositionToPosition (Swos.plposition p)
          psk = PlayerSkills shoots passs speeds controls
          shoots | Swos.isGoalkeeper (Swos.plposition p) = swosValueToSkill $ Swos.plvalue p
                 | otherwise                             = swosSkillToSkill $ Swos.skshooting $ Swos.plskills p
          passs | Swos.isGoalkeeper (Swos.plposition p)  = swosValueToSkill $ Swos.plvalue p
                | otherwise                              = swosSkillToSkill $ Swos.skpassing $ Swos.plskills p
          speeds = swosSkillToSkill $ Swos.skspeed $ Swos.plskills p
          controls = swosSkillToSkill $ Swos.skcontrol $ Swos.plskills p

