module Match.Internal.AI(doAI)
where

import Control.Monad
import Control.Monad.State as State
import Data.List
import qualified Data.IntMap as M
import Data.Function

import FVector
import Player

import Match.Internal.MatchState
import Match.Internal.MatchBase
import Match.Internal.Actions
import Match.Internal.Formation

offBallAI :: Player -> Match ()
offBallAI pl = do
  s <- State.get
  goto (formationPositionAbs s pl) pl

pass :: Player -> Match ()
pass pl = do
  s <- State.get
  let tgtpl = bestPassTarget s pl
      passpwr = getPassPower (plposition tgtpl) pl
  kick passpwr pl

bestPassTarget :: MatchState -> Player -> Player
bestPassTarget m pl = 
  snd $ head $ sortBy (compare `on` fst) $ map (passValue m pl) (ownPlayers m pl)

passValue :: MatchState -> Player -> Player -> (Float, Player)
passValue m passer receiver =
  (max 0 (100 - (dist2 (plposition receiver) (oppositeGoalAbs (pitchsize m) (playerHome passer)))), receiver)

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
        if inKickDistance s pl
          then onBallAI pl
          else offBallAI pl

onBallAI :: Player -> Match ()
onBallAI pl = do
  s <- State.get
  if canDribble s pl
    then dribble pl
    else pass pl

canDribble :: MatchState -> Player -> Bool
canDribble _ _ = False

dribble :: Player -> Match ()
dribble _ = return ()

getPassPower :: FRange -> Player -> FVector3
getPassPower _ _ = (20, 0, 0)


