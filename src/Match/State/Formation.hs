module Match.State.Formation
where

import qualified Data.IntMap as M

import FVector

import Match.Player

import Match.State.MatchState
import Match.State.MatchBase

mkGoalkeeperFormation :: Bool -> [Player] -> Formation
mkGoalkeeperFormation True  pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 0.02)))
mkGoalkeeperFormation False pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 0.98)))

kickoffPositions :: FRange -> (Int -> [FRange]) -> Bool -> [Player] -> Formation
kickoffPositions cnt others home pls =
  let numpls = length pls
      hasscentre = numpls `mod` 2 == 1
      pairpls = if hasscentre then drop 1 pls else pls
      cplrpos = cnt
      pairposs = others numpls
      plrposs = if hasscentre
                  then cplrpos : take (numpls - 1) pairposs
                  else take numpls pairposs
      plrposs' = if home then plrposs else map flipSide plrposs
  in M.fromList (zip (map playerNumber pls) (plrposs'))

mkDefenderFormation :: Bool -> [Player] -> Formation
mkDefenderFormation = kickoffPositions (0.5, 0.2) defs
  where defs 1 = []
        defs 2 = [(0.33, 0.24), (0.76, 0.21)]
        defs 3 = [(0.258, 0.252), (0.753, 0.257)]
        defs 4 = [(0.22, 0.250), (0.83, 0.250), (0.48, 0.25), (0.60, 0.27)]
        defs 5 = [(0.23, 0.258), (0.88, 0.257), (0.41, 0.29), (0.62, 0.26)]
        defs _ = repeat (0.5, 0.2)

mkMidfielderFormation :: Bool -> [Player] -> Formation
mkMidfielderFormation = kickoffPositions (0.5, 0.3) mids
  where mids 1 = []
        mids 2 = [(0.32, 0.34), (0.76, 0.37)]
        mids 3 = [(0.25, 0.41), (0.751, 0.46)]
        mids 4 = [(0.13, 0.452), (0.92, 0.453), (0.41, 0.30), (0.63, 0.38)]
        mids 5 = [(0.12, 0.450), (0.95, 0.451), (0.46, 0.38), (0.67, 0.39)]
        mids _ = repeat (0.5, 0.3)

mkAttackerFormation :: Bool -> [Player] -> Formation
mkAttackerFormation = kickoffPositions (0.5, 0.4) poss
  where poss 1 = []
        poss 2 = [(0.31, 0.42), (0.64, 0.44)]
        poss 3 = [(0.256, 0.452), (0.754, 0.458)]
        poss 4 = [(0.21, 0.455), (0.82, 0.453), (0.41, 0.42), (0.62, 0.46)]
        poss _ = repeat (0.5, 0.4)

createFormation :: Bool -> PlayerMap -> Formation
createFormation home pls' =
  let pls = M.elems pls'
      gs = filter (\p -> plpos p == Goalkeeper) pls
      ds = filter (\p -> plpos p == Defender) pls
      ms = filter (\p -> plpos p == Midfielder) pls
      fs = filter (\p -> plpos p == Attacker) pls
      gmap = mkGoalkeeperFormation home gs
      dmap = mkDefenderFormation home ds
      mmap = mkMidfielderFormation home ms
      fmp = mkAttackerFormation home fs
  in gmap `M.union` dmap `M.union` mmap `M.union` fmp

formationPosition :: MatchState -> Player -> FRange
formationPosition m pl =
  let (kx, ky) = kickoffPosition m pl
  in if playerHome pl
       then (kx, ky * 1.5)
       else (kx, 1 - (1 - ky) * 1.2)

formationPositionAbs :: MatchState -> Player -> FRange
formationPositionAbs m pl =
  relToAbs m (formationPosition m pl)

kickoffPosition :: MatchState -> Player -> FRange
kickoffPosition m pl =
  let (plnum, plhome) = playerid pl 
      sourcemap       = if plhome then homeformation m else awayformation m
  in M.findWithDefault (0.5, 0.5) plnum sourcemap

kickoffPositionAbs :: MatchState -> Player -> FRange
kickoffPositionAbs m pl =
  let rel = kickoffPosition m pl
  in relToAbs m rel

shouldDoKickoff :: MatchState -> Player -> Bool
shouldDoKickoff m pl = kickoffer m == playerid pl
  
shouldAssistKickoff :: MatchState -> Player -> Bool
shouldAssistKickoff m pl = kickoffAssister m == playerid pl

kickoffer :: MatchState -> PlayerID
kickoffer m =
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ homeplayers m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ homeplayers m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ homeplayers m)
  in playerid $ head (forws ++ mids ++ defs)

kickoffAssister :: MatchState -> PlayerID
kickoffAssister m =
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ homeplayers m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ homeplayers m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ homeplayers m)
  in playerid $ head $ tail (forws ++ mids ++ defs)


