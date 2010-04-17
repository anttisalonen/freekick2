module Match.Internal.Actions
where

import Control.Monad.State as State

import Ball
import FVector
import Player

import Match.Internal.MatchState
import Match.Internal.MatchBase

goto :: FRange -> Player -> Match ()
goto (x, y) pl = do
  let (curx,  cury)  = plposition pl
      (diffx, diffy) = (curx - x, cury - y)
      c              = playerid pl
  if abs diffx < plspeed && abs diffy < plspeed
    then modify $ modPlayer c $ modPlposition (const (x, y))
    else do
      let ang = atan2 diffy diffx
          xvel = cos ang * plspeed
          yvel = sin ang * plspeed
      modify $ modPlayer c $ modPlposition (goRight (-xvel))
      modify $ modPlayer c $ modPlposition (goUp (-yvel))

kick :: FVector3 -> Player -> Match ()
kick vec p = do
  s <- State.get
  if not (inKickDistance s p)
    then return ()
    else do
      sModBall $ modBallvelocity $ const vec
      sModPendingevents $ (BallKicked:)
      sModLasttouch $ const $ Just $ playerid p

kickoff :: Player -> Match ()
kickoff p = do
  s <- State.get
  if not (inKickDistance s p)
    then goto (to2D (ballposition (ball s))) p
    else kick (20, 0, 0) p


