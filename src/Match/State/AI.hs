module Match.State.AI(doAI)
where

import Data.List
import Data.Function

import FVector

import Match.Player
import Match.Ball

import Match.State.MatchState
import Match.State.MatchBase
import Match.State.Actions
import Match.State.Formation

offBallAI :: MatchState -> Player -> PlAction
offBallAI m pl | nearestOwnToBall m pl == pl = 
    (pl, Goto (ballCoords m))
offBallAI m pl | supportingDefense m pl = 
    (pl, Goto (defenseSupporterCoords m pl))
offBallAI m pl | otherwise =
    (pl, Goto (formationPositionAbs m pl))

supportingOffense :: MatchState -> Player -> Bool
supportingOffense m pl =
  not (nearestIsHP m) /= playerHome pl && supportingPlayer m pl

supportingDefense :: MatchState -> Player -> Bool
supportingDefense m pl =
  nearestIsHP m /= playerHome pl && supportingPlayer m pl

supportingPlayer :: MatchState -> Player -> Bool
supportingPlayer m pl = pl == (opsToBallByDist m pl !! 1)

defenseSupporterCoords :: MatchState -> Player -> FRange
defenseSupporterCoords m pl = 
  let og = ownGoalAbs m pl
      op = plposition $ nearestOppToBall m pl
  in onLine 3 op og

-- point between points v1 and v2,
-- with distance n from v1.
onLine :: Float -> FRange -> FRange -> FRange
onLine n v1 v2 =
  let dv = v1 `diff2` v2
  in v1 `diff2` ((normalize2 (neg2 dv)) `mul2` n)


pass :: Player -> Player -> PlAction
pass receiver passer = 
  let passpwr = getPassPower (plposition receiver) passer
  in (passer, Kick passpwr)

bestPassTarget :: MatchState -> Player -> (Float, PlAction)
bestPassTarget m pl = (passpts, pass passtarget pl)
  where 
    (passpts, passtarget) = 
      head $ sortBy (flipCompare `on` fst) $ map (passValue m pl) (filter (/= pl) $ ownPlayers m pl)

flipCompare :: (Ord a) => a -> a -> Ordering
flipCompare a b
  | a < b     = GT
  | a > b     = LT
  | otherwise = EQ

passValue :: MatchState -> Player -> Player -> (Float, Player)
passValue m _ receiver =
  (shootPositionValue m (playerHome receiver) (plposition receiver), receiver)

-- range: 0 .. 100
shootPositionValue :: MatchState -> Bool -> FRange -> Float
shootPositionValue _ home pos =
  max 0 (100 - (dist2 pos (oppositeGoalAbs' pos home)))

beforeKickoffAI :: MatchState -> [PlAction]
beforeKickoffAI m = 
  forAIPlayers m $ \pl ->
    if shouldDoKickoff m pl
      then (pl, Goto (relToAbs m (0.5, 0.5)))
      else if shouldAssistKickoff m pl
             then (pl, Goto (relToAbs m (0.52, 0.5)))
             else (pl, Goto (kickoffPositionAbs m pl))

forAIPlayers :: MatchState -> (Player -> a) -> [a]
forAIPlayers m f = map f (filter (\pl -> aiControlled m (playerid pl)) (allPlayers m))

doAI :: MatchState -> [PlAction]
doAI m = 
  case ballplay m of
    BeforeKickoff    -> beforeKickoffAI m
    WaitForKickoff _ -> beforeKickoffAI m
    DoKickoff -> 
      forAIPlayers m $ \pl ->
        if shouldDoKickoff m pl
          then kickoff m pl
          else if shouldAssistKickoff m pl
                 then (pl, Goto (relToAbs m (0.52, 0.5)))
                 else (pl, Goto (kickoffPositionAbs m pl))
    InPlay -> 
      forAIPlayers m $ \pl ->
        if inKickDistance m pl && kicktimer pl <= 0
          then onBallAI m pl
          else offBallAI m pl
    OutOfPlayWaiting _ _ -> []
    OutOfPlay _ r        -> beforeRestartAI m r
    RestartPlay r        ->
      forAIPlayers m $ \pl ->
        if shouldRestart m r pl
          then restart m r pl
          else restartLookout m r pl

shouldRestart :: MatchState -> Restart -> Player -> Bool
shouldRestart m (ThrowIn p) pl =
  pl == nearestOPToPointwoGK m p pl && (homeRestarts m == playerHome pl)
shouldRestart m (CornerKick p) pl =
  pl == nearestOPToPointwoGK m p pl && (homeRestarts m == playerHome pl)
shouldRestart m (GoalKick _) pl =
  plpos pl == Goalkeeper && (homeRestarts m == playerHome pl)

beforeRestartAI :: MatchState -> Restart -> [PlAction]
beforeRestartAI m r =
  forAIPlayers m $ \pl ->
    if shouldRestart m r pl
      then (pl, Goto (ballCoords m))
      else restartLookout m r pl

restartLookout :: MatchState -> Restart -> Player -> PlAction
restartLookout m _ pl = 
  (pl, Goto (formationPositionAbs m pl))

restart :: MatchState -> Restart -> Player -> PlAction
restart m _ pl =
  if not (inKickDistance m pl)
    then (pl, Goto (to2D (ballposition (ball m))))
    else snd $ bestPassTarget m pl

onBallAI :: MatchState -> Player -> PlAction
onBallAI m pl = 
  let passact    = bestPassTarget m pl
      dribbleact = bestDribbleTarget m pl
      shootact   = shootScore m pl
  in snd $ head $ sortBy (flipCompare `on` fst) [passact, dribbleact, shootact]

shootScore :: MatchState -> Player -> (Float, PlAction)
shootScore m pl =
  (scorepts, (pl, Kick kickpwr))
   where 
     vecttogoal = oppositeGoalAbs m pl `diff2` plposition pl
     scorepts =
       shootPositionValue m (playerHome pl) (plposition pl)
     kickpwr =
       to3D (vecttogoal `mul2` 1.5) 3

bestDribbleTarget :: MatchState -> Player -> (Float, PlAction)
bestDribbleTarget m pl = (1, dribble (oppositeGoalAbs m pl) pl)

dribble :: FRange -> Player -> PlAction
dribble _ pl = (pl, Idle)

getPassPower :: FRange -> Player -> FVector3
getPassPower recv pl = 
  to3D ((recv `diff2` (plposition pl)) `mul2` 5) 0

kickoff :: MatchState -> Player -> PlAction
kickoff m p = 
  if not (inKickDistance m p)
    then (p, Goto (to2D (ballposition (ball m))))
    else (p, Kick (20, 0, 0))


