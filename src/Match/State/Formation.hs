module Match.State.Formation
where

import qualified Data.IntMap as M

import Utils
import FVector
import qualified SwosTactics

import Match.Player
import Match.Ball

import Match.State.MatchState
import Match.State.MatchBase

mkGoalkeeperFormation :: Bool -> [Int] -> Formation
mkGoalkeeperFormation True  pls = M.fromList (zip pls (repeat (\_ -> (0.5, 0.02))))
mkGoalkeeperFormation False pls = M.fromList (zip pls (repeat (\_ -> (0.5, 0.98))))

ballrectangle :: (Float, Float) -> Int
ballrectangle (bx, by) = v
  where v = y * 5 + x
        x = clamp 0 4 $ floor $ (1 - bx) * 5
        y = clamp 0 6 $ floor $ by * 7

plpoint :: Int -> [Int] -> FRange
plpoint br ts = 
  let pn = ts !! (min 34 br)
      xp = pn `mod` 15
      yp = pn `div` 16
      x = 1 - fromIntegral xp * (1/15)
      y = fromIntegral yp * (1/16)
  in (x, y)

plPosToTactic :: [Int] -> Tactic
plPosToTactic ps = \b -> plpoint (ballrectangle b) ps

createFormation :: Bool -> PlayerMap -> SwosTactics.SWOSTactics -> Formation
createFormation home pls' stac =
  let pls = M.elems pls'
      gs = take 1 $ map playerNumber $ filter (\p -> plpos p == Goalkeeper) pls
      ds = take d $ map playerNumber $ filter (\p -> plpos p == Defender) pls
      ms = take m $ map playerNumber $ filter (\p -> plpos p == Midfielder) pls
      fs = take f $ map playerNumber $ filter (\p -> plpos p == Attacker) pls
      gmap = mkGoalkeeperFormation home gs
      (d, m, f) = fst $ SwosTactics.organizeTacticsByName stac
      smap = M.fromList (zip (ds ++ ms ++ fs) (map plPosToTactic (SwosTactics.positions stac)))
  in gmap `M.union` smap

defaultTactic :: Tactic
defaultTactic _ = (0.5, 0.5)

formationPosition :: MatchState -> Player -> FRange
formationPosition m pl =
  let (plnum, plhome) = playerid pl
      bp = absToRel' (pitchsize m) $ to2D $ ballposition $ ball m
      sourcemap = if plhome then homeformation m else awayformation m
      sfunc = M.findWithDefault defaultTactic plnum sourcemap
  in checkFlip m $ sfunc bp

formationPositionAbs :: MatchState -> Player -> FRange
formationPositionAbs m pl =
  relToAbs m (formationPosition m pl)

toKickoff :: Tactic -> Tactic
toKickoff m b = let (x, y) = m b
                in (x / 2, y)

kickoffPosition :: MatchState -> Player -> FRange
kickoffPosition m pl =
  let (x, y) = formationPosition m pl
  in if playerHome pl == homeattacksup m
       then (x, y / 2)
       else (x, 1 - (y / 2))

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
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ pls m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ pls m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ pls m)
      pls = if homekickoff m then homeplayers else awayplayers
  in playerid $ head (forws ++ mids ++ defs)

kickoffAssister :: MatchState -> PlayerID
kickoffAssister m =
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ pls m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ pls m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ pls m)
      pls = if homekickoff m then homeplayers else awayplayers
  in playerid $ head $ tail (forws ++ mids ++ defs)


