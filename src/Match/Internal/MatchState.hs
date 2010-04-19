{-# LANGUAGE TemplateHaskell #-}
module Match.Internal.MatchState
where

import Control.Monad.State as State
import qualified Data.IntMap as M

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import DeriveMod
import Ball
import FVector
import Player

type PlayerMap = M.IntMap Player
type Formation = M.IntMap FRange

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

data MatchState = MatchState {
    pitchlist      :: DisplayList
  , currkeys       :: [SDLKey]
  , pitchsize      :: (Float, Float)
  , campos         :: (Float, Float)
  , homeplayers    :: PlayerMap
  , awayplayers    :: PlayerMap
  , homeformation  :: Formation
  , awayformation  :: Formation
  , controlledpl   :: Maybe PlayerID
  , ballplay       :: BallPlay
  , ball           :: Ball
  , pendingevents  :: [MatchEvent]
  , lasttouch      :: Maybe PlayerID
  , homegoals      :: Int
  , awaygoals      :: Int
  , matchfont1     :: Font
  , matchfont2     :: Font
  , hometeamname   :: String
  , awayteamname   :: String
  }
$(deriveMods ''MatchState)

modPlayer :: PlayerID -> (Player -> Player) -> MatchState -> MatchState
modPlayer (pln, True)  f = modHomeplayers (M.adjust f pln)
modPlayer (pln, False) f = modAwayplayers (M.adjust f pln)

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


