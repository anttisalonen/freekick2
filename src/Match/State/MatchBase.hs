module Match.State.MatchBase
where

import Control.Monad
import qualified Data.IntMap as M
import Data.Maybe
import Data.List
import Data.Ord

import FVector
import qualified Gen

import Match.Ball
import Match.Player

import Match.State.MatchState

plspeed :: Float -> Float -> Float -> Player -> Float
plspeed sc smin dt p = sc * plsp * dt
  where plsp = smin + (1 - smin) * Gen.shootingskill (plskills p)

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

inKickDistance :: MatchState -> Player -> Bool
inKickDistance m p = 
  let (bx, by, bz) = ballposition (ball m)
      pp = plposition p
  in if bz > maxkickheight (params m)
       then False
       else dist2 (bx, by) pp < kickdistance (params m)

inDribbleDistance :: MatchState -> Player -> Bool
inDribbleDistance m p = 
  let (bx, by, bz) = ballposition (ball m)
      pp = plposition p
  in if bz > maxdribbleheight (params m)
       then False
       else dist2 (bx, by) pp < dribbledistance (params m)

inCatchDistance :: MatchState -> Player -> Bool
inCatchDistance m p = 
  let (bx, by, bz) = ballposition (ball m)
      pp = plposition p
      ppos = plpos p
  in if ppos /= Gen.Goalkeeper
       then False
       else if bz > maxcatchheight (params m)
              then False
              else dist2 (bx, by) pp < catchdistance (params m)

nearestToBall :: MatchState -> Player
nearestToBall m =
  let hp = nearestHPToBall m
      ap' = nearestAPToBall m
      hd = distanceToBall m hp
      ad = distanceToBall m ap'
  in if hd <= ad
       then hp
       else ap'

nearestOwnToBall :: MatchState -> Player -> Player
nearestOwnToBall m pl =
  if playerHome pl
    then nearestHPToBall m
    else nearestAPToBall m

nearestOppToBall :: MatchState -> Player -> Player
nearestOppToBall m pl =
  if not $ playerHome pl
    then nearestHPToBall m
    else nearestAPToBall m

nearestOPToPointwoGK :: MatchState -> FRange -> Player -> Player
nearestOPToPointwoGK m p pl =
  if playerHome pl
    then nearestToPointHwoGK m p
    else nearestToPointAwoGK m p

nearestHPToBall :: MatchState -> Player
nearestHPToBall = head . hpsToBallByDist

nearestAPToBall :: MatchState -> Player
nearestAPToBall = head . apsToBallByDist

nearestIsHP :: MatchState -> Bool
nearestIsHP m = distanceToBall m h <= distanceToBall m a
  where h = nearestHPToBall m
        a = nearestAPToBall m

opsToBallByDist :: MatchState -> Player -> [Player]
opsToBallByDist m pl =
  if playerHome pl
    then hpsToBallByDist m
    else apsToBallByDist m

hpsToBallByDist :: MatchState -> [Player]
hpsToBallByDist m = sortBy (comparing (distanceToBall m)) (M.elems $ homeplayers m)

apsToBallByDist :: MatchState -> [Player]
apsToBallByDist m = sortBy (comparing (distanceToBall m)) (M.elems $ awayplayers m)

-- without goalkeeper
nearestToBallHwoGK :: MatchState -> Player
nearestToBallHwoGK m = nearestToPointHwoGK m (to2D $ ballposition $ ball m)

nearestToBallAwoGK :: MatchState -> Player
nearestToBallAwoGK m = nearestToPointAwoGK m (to2D $ ballposition $ ball m)

nearestToPointHwoGK :: MatchState -> FRange -> Player
nearestToPointHwoGK m p =
  head $ sortBy (comparing (\pl -> dist2 p (plposition pl))) (filter (\pl -> plpos pl /= Gen.Goalkeeper) $ M.elems $ homeplayers m)

nearestToPointAwoGK :: MatchState -> FRange -> Player
nearestToPointAwoGK m p =
  head $ sortBy (comparing (\pl -> dist2 p (plposition pl))) (filter (\pl -> plpos pl /= Gen.Goalkeeper) $ M.elems $ awayplayers m)

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

absToRel' :: FRange -> FRange -> FRange
absToRel' (px, py) (x, y) = (x / px, y / py)

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

ownGoalAbs :: MatchState -> Player -> FRange
ownGoalAbs m pl =
  oppositeGoalAbs' (pitchsize m) (not $ playerHome pl == homeattacksup m)

ownGoalAbs' :: FRange -> Bool -> FRange
ownGoalAbs' ps home = oppositeGoalAbs' ps (not home)

oppositeGoalAbs :: MatchState -> Player -> FRange
oppositeGoalAbs m pl =
  oppositeGoalAbs' (pitchsize m) (playerHome pl == homeattacksup m)

oppositeGoalAbs' :: FRange -> Bool -> FRange
oppositeGoalAbs' ps attackup =
  if attackup
    then relToAbs' ps (0.5, 1.0)
    else relToAbs' ps (0.5, 0.0)

aiControlled :: MatchState -> PlayerID -> Bool
aiControlled s n =
  case controlledpl s of
    Nothing -> True
    Just p  -> n /= p

homeRestarts :: MatchState -> Bool
homeRestarts m = not $ fromMaybe True (liftM snd $ lasttouch m)

pausedBallplay :: MatchState -> Bool
pausedBallplay m = pausedBallplay' (ballplay m)

pausedBallplay' :: BallPlay -> Bool
pausedBallplay' (WaitForKickoff _) = True
pausedBallplay' (OutOfPlay _ _)    = True
pausedBallplay' (Finished _)       = True
pausedBallplay' _                  = False

inPlay :: BallPlay -> Bool
inPlay InPlay = True
inPlay _      = False

finishedSince :: BallPlay -> Maybe Int
finishedSince (Finished t) = Just t
finishedSince _            = Nothing

canDribble :: MatchState -> Player -> Bool
canDribble m pl =
  inPlay (ballplay m) &&
  inDribbleDistance m pl && 
  kicktimer pl <= 0 && 
  len3 (ballvelocity (ball m)) < (maxballdspeed (params m))

opponentLastTouched :: MatchState -> Player -> Bool
opponentLastTouched m pl =
  case lasttouch m of
    Nothing     -> False
    Just (_, h) -> playerHome pl /= h

