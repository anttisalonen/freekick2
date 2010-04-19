{-# LANGUAGE TemplateHaskell #-}
module Match.Internal.MatchState
where

import Control.Monad.State as State
import qualified Data.IntMap as M
import System.Random

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import DeriveMod
import Ball
import FVector
import Player

data Restart = ThrowIn FRange
             | GoalKick FRange
             | CornerKick FRange

getRestartPoint :: Restart -> FRange
getRestartPoint (ThrowIn r) = r
getRestartPoint (GoalKick r) = r
getRestartPoint (CornerKick r) = r

data BallPlay = BeforeKickoff 
              | WaitForKickoff Int 
              | DoKickoff 
              | InPlay
              | OutOfPlayWaiting Int Restart -- ai does nothing
              | OutOfPlay Int Restart        -- ai moves to positions
              | RestartPlay Restart          -- ai restarts play

data MatchEvent = BallKicked

type Formation = M.IntMap FRange

type PlayerMap = M.IntMap Player

data Team = Team {
    players    :: PlayerMap
  , formation  :: Formation
  , goals      :: Int
  , teamname   :: String
}
$(deriveMods ''Team)

data MatchState = MatchState {
    pitchlist      :: DisplayList
  , currkeys       :: [SDLKey]
  , pitchsize      :: (Float, Float)
  , campos         :: (Float, Float)
  , hometeam       :: Team
  , awayteam       :: Team
  , controlledpl   :: Maybe PlayerID
  , ballplay       :: BallPlay
  , ball           :: Ball
  , pendingevents  :: [MatchEvent]
  , lasttouch      :: Maybe PlayerID
  , matchfont1     :: Font
  , matchfont2     :: Font
  , randomgen      :: StdGen
  }
$(deriveMods ''MatchState)

modHomeplayers :: (PlayerMap -> PlayerMap) -> MatchState -> MatchState
modHomeplayers f = modHometeam (modPlayers f)

modAwayplayers :: (PlayerMap -> PlayerMap) -> MatchState -> MatchState
modAwayplayers f = modAwayteam (modPlayers f)

sModHomegoals :: (Int -> Int) -> Match ()
sModHomegoals f = modify $ modHometeam (modGoals f)

sModAwaygoals :: (Int -> Int) -> Match ()
sModAwaygoals f = modify $ modAwayteam (modGoals f)

modPlayer :: PlayerID -> (Player -> Player) -> MatchState -> MatchState
modPlayer (pln, True)  f = modHomeplayers (M.adjust f pln)
modPlayer (pln, False) f = modAwayplayers (M.adjust f pln)

homeplayers :: MatchState -> PlayerMap
homeplayers = players . hometeam

awayplayers :: MatchState -> PlayerMap
awayplayers = players . awayteam

hometeamname :: MatchState -> String
hometeamname = teamname . hometeam

awayteamname :: MatchState -> String
awayteamname = teamname . awayteam

homegoals :: MatchState -> Int
homegoals = goals . hometeam

awaygoals :: MatchState -> Int
awaygoals = goals . awayteam

homeformation :: MatchState -> Formation
homeformation = formation . hometeam

awayformation :: MatchState -> Formation
awayformation = formation . awayteam

findPlayer :: PlayerID -> MatchState -> Maybe Player
findPlayer (pln, True)  m = M.lookup pln (homeplayers m)
findPlayer (pln, False) m = M.lookup pln (awayplayers m)

modAllPlayers :: (Player -> Player) -> MatchState -> MatchState
modAllPlayers f m = modHomeplayers (M.map f) (modAwayplayers (M.map f) m)

sModAllPlayers ::
     (Player -> Player) -> Match ()
sModAllPlayers f = modify $ modAllPlayers f

sModPlayer ::
     PlayerID -> (Player -> Player) -> Match ()
sModPlayer p f = modify $ modPlayer p f

type Match = StateT MatchState IO


