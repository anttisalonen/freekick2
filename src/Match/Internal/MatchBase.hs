module Match.Internal.MatchBase
where

import qualified Data.IntMap as M
import Data.List
import Data.Ord

import Ball
import FVector
import Player

import Match.Internal.MatchState

plspeed :: Float
plspeed = 0.2

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

inKickDistance :: MatchState -> Player -> Bool
inKickDistance m p = 
  let (bx, by, bz) = ballposition (ball m)
      pp = plposition p
  in if bz > 0.8
       then False
       else dist2 (bx, by) pp < 0.5

nearestToBall :: MatchState -> Player
nearestToBall m =
  let hp = nearestHPToBall m
      ap = nearestAPToBall m
      hd = distanceToBall m hp
      ad = distanceToBall m ap
  in if hd <= ad
       then hp
       else ap

nearestOPToBall :: MatchState -> Player -> Player
nearestOPToBall m pl =
  if playerHome pl
    then nearestHPToBall m
    else nearestAPToBall m

nearestHPToBall :: MatchState -> Player
nearestHPToBall m =
  head $ sortBy (comparing (distanceToBall m)) (M.elems $ homeplayers m)

nearestAPToBall :: MatchState -> Player
nearestAPToBall m =
  head $ sortBy (comparing (distanceToBall m)) (M.elems $ awayplayers m)

distanceToBall :: MatchState -> Player -> Float
distanceToBall m pl = dist2 (to2D (ballposition (ball m))) (plposition pl)

ballCoords :: MatchState -> FRange
ballCoords = to2D . ballposition . ball

flipSide :: FRange -> FRange
flipSide (x, y) = (1 - x, 1 - y)

absToRel :: MatchState -> FRange -> FRange
absToRel m (x, y) =
  let (px, py) = pitchsize m
  in (x / px, y / py)

relToAbs' :: FRange -> FRange -> FRange
relToAbs' (px, py) (x, y) =
  (px * x, py * y)

relToAbs :: MatchState -> FRange -> FRange
relToAbs m c =
  let ps = pitchsize m
  in relToAbs' ps c

ownPlayers :: MatchState -> Player -> [Player]
ownPlayers m pl =
  if playerHome pl
    then M.elems (homeplayers m)
    else M.elems (awayplayers m)

opponentPlayers :: MatchState -> Player -> [Player]
opponentPlayers m pl = 
  if not $ playerHome pl
    then M.elems (homeplayers m)
    else M.elems (awayplayers m)

allPlayers :: MatchState -> [Player]
allPlayers m = M.elems (homeplayers m) ++ (M.elems (awayplayers m))

oppositeGoalAbs :: MatchState -> Player -> FRange
oppositeGoalAbs m pl =
  oppositeGoalAbs' (pitchsize m) (playerHome pl)

oppositeGoalAbs' :: FRange -> Bool -> FRange
oppositeGoalAbs' ps home =
  if home
    then relToAbs' ps (0.5, 1.0)
    else relToAbs' ps (0.5, 0.0)

aiControlled :: MatchState -> PlayerID -> Bool
aiControlled s n =
  case controlledpl s of
    Nothing -> True
    Just p  -> n /= p


