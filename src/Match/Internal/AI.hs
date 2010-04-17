module Match.Internal.AI(doAI)
where

import Data.List
import Data.Function

import FVector
import Player
import Ball

import Match.Internal.MatchState
import Match.Internal.MatchBase
import Match.Internal.Actions
import Match.Internal.Formation

offBallAI :: MatchState -> Player -> PlAction
offBallAI m pl = 
  if nearestOPToBall m pl == pl
    then (pl, Goto (ballCoords m))
    else (pl, Goto (formationPositionAbs m pl))

pass :: Player -> Player -> PlAction
pass receiver passer = 
  let passpwr = getPassPower (plposition receiver) passer
  in (passer, Kick passpwr)

bestPassTarget :: MatchState -> Player -> (Float, Player)
bestPassTarget m pl = 
  head $ filter ((/= pl) . snd) $ sortBy (flipCompare `on` fst) $ map (passValue m pl) (ownPlayers m pl)

flipCompare a b
  | a < b     = GT
  | a > b     = LT
  | otherwise = EQ

passValue :: MatchState -> Player -> Player -> (Float, Player)
passValue m passer receiver =
  (max 0 (100 - (dist2 (plposition receiver) (oppositeGoalAbs m passer))), receiver)

beforeKickoffAI :: MatchState -> [PlAction]
beforeKickoffAI m = 
  forAIPlayers m $ \pl -> do
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
      forAIPlayers m $ \pl -> do
          if shouldDoKickoff m pl
            then kickoff m pl
            else (pl, Idle)
    InPlay -> 
      forAIPlayers m $ \pl -> do
          if inKickDistance m pl
            then onBallAI m pl
            else offBallAI m pl

onBallAI :: MatchState -> Player -> PlAction
onBallAI m pl = 
  let (passscore, passpl) = bestPassTarget m pl
      (dribblescore, dribbledir) = bestDribbleTarget m pl
  in if passscore > dribblescore
       then pass passpl pl
       else dribble dribbledir pl

bestDribbleTarget :: MatchState -> Player -> (Float, FRange)
bestDribbleTarget m pl = (1, oppositeGoalAbs m pl)

dribble :: FRange -> Player -> PlAction
dribble _ pl = (pl, Idle)

getPassPower :: FRange -> Player -> FVector3
getPassPower recv pl = 
  to3D ((recv `diff2` (plposition pl)) `mul2` 1) 0

kickoff :: MatchState -> Player -> PlAction
kickoff m p = 
  if not (inKickDistance m p)
    then (p, Goto (to2D (ballposition (ball m))))
    else (p, Kick (20, 0, 0))


