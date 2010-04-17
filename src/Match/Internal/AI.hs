module Match.Internal.AI(doAI)
where

import Control.Monad
import Control.Monad.State as State
import Data.List
import qualified Data.IntMap as M
import Data.Function

import FVector
import Player
import Ball

import Match.Internal.MatchState
import Match.Internal.MatchBase
import Match.Internal.Actions
import Match.Internal.Formation

offBallAI :: Player -> Match ()
offBallAI pl = do
  s <- State.get
  goto (formationPositionAbs s pl) pl

pass :: Player -> Player -> Match ()
pass receiver passer = do
  let passpwr = getPassPower (plposition receiver) passer
  kick passpwr passer

bestPassTarget :: MatchState -> Player -> (Float, Player)
bestPassTarget m pl = 
  head $ sortBy (compare `on` fst) $ map (passValue m pl) (ownPlayers m pl)

passValue :: MatchState -> Player -> Player -> (Float, Player)
passValue m passer receiver =
  (max 0 (100 - (dist2 (plposition receiver) (oppositeGoalAbs m passer))), receiver)

beforeKickoffAI :: Match ()
beforeKickoffAI = do
  s <- State.get
  forM_ (M.elems (awayplayers s) ++ (M.elems (homeplayers s))) $ \pl -> do
    when (aiControlled s (playerid pl)) $ do
      if shouldDoKickoff s pl
        then goto (relToAbs s (0.5, 0.5)) pl
        else if shouldAssistKickoff s pl
               then goto (relToAbs s (0.52, 0.5)) pl
               else goto (formationPositionAbs s pl) pl

doAI :: Match ()
doAI = do
  s <- State.get
  case ballplay s of
    BeforeKickoff    -> beforeKickoffAI
    WaitForKickoff _ -> beforeKickoffAI
    DoKickoff -> do
      forM_ (M.elems (awayplayers s) ++ (M.elems (homeplayers s))) $ \pl -> do
        when (aiControlled s (playerid pl)) $ do
          if shouldDoKickoff s pl
            then kickoff pl
            else return ()
    InPlay -> do
      forM_ (allPlayers s) $ \pl -> do
        when (aiControlled s (playerid pl)) $ do
          if inKickDistance s pl
            then onBallAI pl
            else offBallAI pl

onBallAI :: Player -> Match ()
onBallAI pl = do
  s <- State.get
  let (passscore, passpl) = bestPassTarget s pl
      (dribblescore, dribbledir) = bestDribbleTarget s pl
  if passscore > dribblescore
    then pass passpl pl
    else dribble dribbledir pl

bestDribbleTarget :: MatchState -> Player -> (Float, FRange)
bestDribbleTarget m pl = (1, oppositeGoalAbs m pl)

dribble :: FRange -> Player -> Match ()
dribble _ _ = return ()

getPassPower :: FRange -> Player -> FVector3
getPassPower _ _ = (20, 0, 0)

kickoff :: Player -> Match ()
kickoff p = do
  s <- State.get
  if not (inKickDistance s p)
    then goto (to2D (ballposition (ball s))) p
    else kick (20, 0, 0) p


