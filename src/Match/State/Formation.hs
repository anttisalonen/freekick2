module Match.State.Formation(
  formationPositionAbs,
  shouldDoKickoff,
  shouldAssistKickoff,
  genFormationToFormation,
  kickoffPositionAbs
  )
where

import qualified Data.IntMap as M

import FVector
import qualified Gen

import Match.Player
import Match.Ball

import Match.State.MatchState
import Match.State.MatchBase

defaultTactic :: Gen.Tactic
defaultTactic _ = (0.5, 0.5)

formationPosition :: MatchState -> Player -> FRange
formationPosition m pl =
  let (plnum, plhome) = playerid pl
      bp = absToRel' (pitchsize m) $ to2D $ ballposition $ ball m
      sourcemap = if plhome then homeformation m else awayformation m
      sfunc = M.findWithDefault defaultTactic plnum sourcemap
  in if plhome == homeattacksup m
       then sfunc bp
       else flipSide $ sfunc bp

formationPositionAbs :: MatchState -> Player -> FRange
formationPositionAbs m pl =
  relToAbs m (formationPosition m pl)

toKickoff :: Gen.Tactic -> Gen.Tactic
toKickoff m b = let (x, y) = m b
                in (x / 2, y)

kickoffPosition :: MatchState -> Player -> FRange
kickoffPosition m pl =
  let (x, y) = formationPosition m pl
  in if playerHome pl == homeattacksup m
       then (x, y / 2)
       else (x, 1.0 - ((1 - y) / 2))

kickoffPositionAbs :: MatchState -> Player -> FRange
kickoffPositionAbs m pl =
  let rel = kickoffPosition m pl
  in relToAbs m rel

checkFlip :: MatchState -> FRange -> FRange
checkFlip m = if not (homeattacksup m) then flipSide else id

shouldDoKickoff :: MatchState -> Player -> Bool
shouldDoKickoff m pl = kickoffer m == playerid pl
  
shouldAssistKickoff :: MatchState -> Player -> Bool
shouldAssistKickoff m pl = kickoffAssister m == playerid pl

kickoffer :: MatchState -> PlayerID
kickoffer m =
  let forws = filter (\p -> plpos p == Gen.Attacker) (M.elems $ pls m)
      mids = filter (\p -> plpos p == Gen.Midfielder) (M.elems $ pls m)
      defs = filter (\p -> plpos p == Gen.Defender) (M.elems $ pls m)
      pls = if homekickoff m then homeplayers else awayplayers
  in playerid $ head (forws ++ mids ++ defs)

kickoffAssister :: MatchState -> PlayerID
kickoffAssister m =
  let forws = filter (\p -> plpos p == Gen.Attacker) (M.elems $ pls m)
      mids = filter (\p -> plpos p == Gen.Midfielder) (M.elems $ pls m)
      defs = filter (\p -> plpos p == Gen.Defender) (M.elems $ pls m)
      pls = if homekickoff m then homeplayers else awayplayers
  in playerid $ head $ tail (forws ++ mids ++ defs)

mkGoalkeeperFormation :: [Int] -> Formation
mkGoalkeeperFormation pls = M.fromList (zip pls (repeat (\_ -> (0.5, 0.02))))

genFormationToFormation :: PlayerMap -> Gen.GenFormation -> Formation
genFormationToFormation pls' frm =
  let pls = M.elems pls'
      gs = take 1 $ map playerNumber $ filter (\p -> plpos p == Gen.Goalkeeper) pls
      ds = take d $ map playerNumber $ filter (\p -> plpos p == Gen.Defender) pls
      ms = take m $ map playerNumber $ filter (\p -> plpos p == Gen.Midfielder) pls
      fs = take f $ map playerNumber $ filter (\p -> plpos p == Gen.Attacker) pls
      gmap = mkGoalkeeperFormation gs
      (d, m, f) = Gen.playerorder frm
      ts = Gen.tactics frm
      smap = M.fromList (zip (ds ++ ms ++ fs) ts)
  in gmap `M.union` smap


