{-# LANGUAGE TemplateHaskell #-}
module Match.Internal.MatchState
where

import Control.Monad.State as State
import qualified Data.IntMap as M

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import DeriveMod
import Ball
import FVector
import Player

type PlayerMap = M.IntMap Player
type Formation = M.IntMap FRange

data BallPlay = BeforeKickoff | WaitForKickoff Int | DoKickoff | InPlay

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
  }
$(deriveMods ''MatchState)

modPlayer :: PlayerID -> (Player -> Player) -> MatchState -> MatchState
modPlayer (pln, True)  f = modHomeplayers (M.adjust f pln)
modPlayer (pln, False) f = modAwayplayers (M.adjust f pln)

type Match = StateT MatchState IO


