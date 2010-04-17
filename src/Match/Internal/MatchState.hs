{-# LANGUAGE TemplateHaskell #-}
module Match.Internal.MatchState
where

import Control.Monad
import Control.Monad.State as State
import Data.Maybe
import Data.List
import qualified Data.IntMap as M
import System.CPUTime
import Data.Word
import Data.Function

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import qualified Swos
import SDLUtils
import Drawing
import DrawPitch
import DeriveMod
import Ball
import FVector
import Player
import SWOSShell

type PlayerMap = M.IntMap Player
type Formation = M.IntMap FRange

data BallPlay = BeforeKickoff | WaitForKickoff Int | DoKickoff | InPlay

data Action = BallKicked

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
  , pendingactions :: [Action]
  , lasttouch      :: Maybe PlayerID
  }
$(deriveMods ''MatchState)

modPlayer :: PlayerID -> (Player -> Player) -> MatchState -> MatchState
modPlayer (pln, True)  f = modHomeplayers (M.adjust f pln)
modPlayer (pln, False) f = modAwayplayers (M.adjust f pln)

type Match = StateT MatchState IO


