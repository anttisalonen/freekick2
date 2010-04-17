module Match.Internal.Formation
where

import qualified Data.IntMap as M

import FVector
import Player

import Match.Internal.MatchState
import Match.Internal.MatchBase

mkGoalkeeperFormation :: Bool -> [Player] -> Formation
mkGoalkeeperFormation True  pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 0)))
mkGoalkeeperFormation False pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 1)))

mkDefenderFormation :: Bool -> [Player] -> Formation
mkDefenderFormation = kickoffPositions (0.5, 0.2) defs
  where defs 1 = []
        defs 2 = [(0.3, 0.2), (0.7, 0.2)]
        defs 3 = [(0.25, 0.25), (0.75, 0.25)]
        defs 4 = [(0.2, 0.25), (0.8, 0.25), (0.4, 0.2), (0.6, 0.2)]
        defs 5 = [(0.2, 0.25), (0.8, 0.25), (0.4, 0.2), (0.6, 0.2)]
        defs _ = repeat (0.5, 0.2)

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

mkMidfielderFormation :: Bool -> [Player] -> Formation
mkMidfielderFormation = kickoffPositions (0.5, 0.3) mids
  where mids 1 = []
        mids 2 = [(0.3, 0.3), (0.7, 0.3)]
        mids 3 = [(0.25, 0.4), (0.75, 0.4)]
        mids 4 = [(0.1, 0.45), (0.9, 0.45), (0.4, 0.3), (0.6, 0.3)]
        mids 5 = [(0.1, 0.45), (0.9, 0.45), (0.4, 0.3), (0.6, 0.3)]
        mids _ = repeat (0.5, 0.3)

mkAttackerFormation :: Bool -> [Player] -> Formation
mkAttackerFormation = kickoffPositions (0.5, 0.4) poss
  where poss 1 = []
        poss 2 = [(0.3, 0.4), (0.7, 0.4)]
        poss 3 = [(0.25, 0.45), (0.75, 0.45)]
        poss 4 = [(0.2, 0.45), (0.8, 0.45), (0.4, 0.4), (0.6, 0.4)]
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
  let (plnum, plhome) = playerid pl 
      sourcemap       = if plhome then homeformation m else awayformation m
  in M.findWithDefault (0.5, 0.5) plnum sourcemap

formationPositionAbs :: MatchState -> Player -> FRange
formationPositionAbs m pl =
  let rel = formationPosition m pl
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


