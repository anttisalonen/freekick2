{-# LANGUAGE TemplateHaskell #-}
module Match.State.MatchState
where

import Control.Monad.State as State
import qualified Data.IntMap as M
import System.Random

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import DeriveMod
import FVector

import Match.Ball
import Match.Player

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
              | Finished Int

data MatchEvent = BallKicked

type Tactic = FRange -> FRange

type Formation = M.IntMap Tactic

type PlayerMap = M.IntMap Player

data TeamOwner = HumanOwner | AIOwner
  deriving (Eq)

data Team = Team {
    players    :: PlayerMap
  , formation  :: Formation
  , goals      :: Int
  , teamname   :: String
  , teamowner  :: TeamOwner
}
$(deriveMods ''Team)

data MatchParams = MatchParams {
    matchtimedelta     :: Float -- time increase in seconds/frame
  , kickofftimer       :: Int -- time to spend in WaitForKickoff (ms)
  , kickoffballtimer   :: Int -- time after ball is centered in WaitForKickoff (ms)
  , oopthrowintimer    :: Int -- time to spend in OutOfPlayWaiting (throwin) (ms)
  , oopgoalkicktimer   :: Int -- time to spend in OutOfPlayWaiting (goal kick) (ms)
  , oopcornerkicktimer :: Int -- time to spend in OutOfPlayWaiting (corner kick) (ms)
  , ooptimer           :: Int -- time to spend in OutOfPlay (ms)
  , oopmoveballtimer   :: Int -- time after ball is moved in OutOfPlay (ms)
  , ballbounciness     :: Float -- 1: no bounce; 2: 100% elastic bounce
  , ballgravitypull    :: Float -- m/s^2 (approximate)
  , ballairviscosity   :: Float -- 0: none; 1/frametime(=50): complete
  , ballrollfriction   :: Float -- as in air viscosity
  , plcontrolmin       :: Float -- control coeff with control skill = 0
  , maxkickveclen      :: Float -- maximum length of kick vector
  , stillpassveclen    :: Float -- max. length of kick vector to still count as pass
  , stillpassvecheight :: Float -- max. height of kick vector to still count as pass
  , maxkickvar         :: Float -- percentage [0..1] of kick target variation in x, y
  , maxkickvarz        :: Float -- percentage [0..1] of kick target variation in z
  , setkicktimer       :: Int   -- kick timer in ms (max. time between kicks by a player)
  , aimaxlowpasslen    :: Float -- max. distance where to pass low
  , ailowpasspower     :: Float -- pass power coeff
  , aihighpasspower    :: Float -- pass power coeff
  , aihighpasszpower   :: Float -- pass power coeff (z)
  , plspeedcoeff       :: Float -- player speed coeff
  , plspeedmin         :: Float -- speed coeff with speed skill = 0
  , kickdistance       :: Float -- max kick distance
  , dribbledistance    :: Float -- max dribble distance
  , catchdistance      :: Float -- max catch distance
  , maxkickheight      :: Float -- max ball height for kick
  , maxdribbleheight   :: Float -- max ball height for dribble
  , maxcatchheight     :: Float -- max ball height for catch
  , maxballdspeed      :: Float -- max ball speed for dribbling
  , ctrlquitkey        :: SDLKey -- key to quit
  , ctrlshootkey       :: SDLKey -- key to shoot
  , ctrlleftkey        :: SDLKey
  , ctrlrightkey       :: SDLKey
  , ctrlupkey          :: SDLKey
  , ctrldownkey        :: SDLKey
  , ctrlpausekey       :: SDLKey
  }

defaultParams = MatchParams
  30 2000 1000 -- general
  1000 1000 1000 1000 1000  -- oop
  1.5 (-10) 0.5 1.2 -- ball
  0.6 -- control
  50 -- kick
  20 2 -- pass
  0.5 0.5 -- kick variation
  1000 -- kick timer
  20 4 1.5 0.25 -- ai pass
  20 0.7 -- speed
  1.2 0.8 2.0 -- max distances
  0.8 0.5 2.4 -- max heights
  50 -- max ball dribble speed
  SDLK_ESCAPE SDLK_RCTRL SDLK_LEFT SDLK_RIGHT SDLK_UP SDLK_DOWN SDLK_p

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
  , matchtime      :: (Bool, Float) -- half time?, seconds
  , paused         :: Bool
  , kickpower      :: Int
  , frametime      :: Float
  , homekickoff    :: Bool
  , homeattacksup  :: Bool
  , params         :: MatchParams
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


