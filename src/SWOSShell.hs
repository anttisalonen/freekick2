module SWOSShell
where

import qualified Swos
import qualified SwosTactics
import Gen

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

swosSkillsToSkills :: Swos.SWOSPlayer -> PlayerSkills
swosSkillsToSkills p = PlayerSkills shoots passs speeds controls
  where shoots | Swos.isGoalkeeper (Swos.plposition p) = swosValueToSkill $ Swos.plvalue p
               | otherwise                             = swosSkillToSkill $ Swos.skshooting $ Swos.plskills p
        passs | Swos.isGoalkeeper (Swos.plposition p)  = swosValueToSkill $ Swos.plvalue p
              | otherwise                              = swosSkillToSkill $ Swos.skpassing $ Swos.plskills p
        speeds = swosSkillToSkill $ Swos.skspeed $ Swos.plskills p
        controls = swosSkillToSkill $ Swos.skcontrol $ Swos.plskills p

swosPlayerToGenPlayer :: Swos.SWOSPlayer -> GenPlayer
swosPlayerToGenPlayer p = GenPlayer (Swos.plnumber p) (Swos.plname p) (swosPositionToPosition $ Swos.plposition p) (swosSkillsToSkills p)

swosColorToColor :: Swos.SWOSColor -> Color
swosColorToColor Swos.Grey = grey
swosColorToColor Swos.White = white
swosColorToColor Swos.Black = black
swosColorToColor Swos.Orange = orange
swosColorToColor Swos.Red = red
swosColorToColor Swos.Blue = blue
swosColorToColor Swos.Brown = brown
swosColorToColor Swos.LightBlue = lightblue
swosColorToColor Swos.Green = green
swosColorToColor Swos.Yellow = yellow

swosKitToKit :: Swos.SWOSKit -> Kit
swosKitToKit k = Kit ktype kcolor1 kcolor2 kshortcol ksockscol
  where ktype     = Swos.kittype k
        kcolor1   = swosColorToColor (Swos.kitfirstcolor k)
        kcolor2   = swosColorToColor (Swos.kitsecondcolor k)
        kshortcol = swosColorToColor (Swos.kitshortcolor k)
        ksockscol = swosColorToColor (Swos.kitsockscolor k)

swosTeamToGenTeam :: Swos.SWOSTeam -> GenTeam
swosTeamToGenTeam t =
  GenTeam (Swos.teamnation t) (Swos.teamname t) (Swos.numPositions (Swos.teamtactics t)) (Swos.teamdivision t) k pls
    where pls = map swosPlayerToGenPlayer (Swos.teamplayers t)
          k = swosKitToKit (Swos.primarykit t)

swosTacticsToGenFormation :: SwosTactics.SWOSTactics -> GenFormation
swosTacticsToGenFormation stac =
  let (d, m, f) = fst $ SwosTactics.organizeTacticsByName stac
      ts = map plPosToTactic (SwosTactics.positions stac)
  in GenFormation ts (d, m, f)

swosTacticsToSimpleFormation :: SwosTactics.SWOSTactics -> SimpleFormation
swosTacticsToSimpleFormation stac =
  let (d, m, f) = fst $ SwosTactics.organizeTacticsByName stac
  in SimpleFormation (SwosTactics.positions stac) (d, m, f)


