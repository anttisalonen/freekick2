module Match.Internal.Actions(Action(..),
  PlAction,
  act,
  actP,
  plact)
where

import Control.Monad.State as State

import Ball
import FVector
import Player

import Match.Internal.MatchState
import Match.Internal.MatchBase

data Action = Goto FRange | Kick FVector3 | Idle

type PlAction = (Player, Action)

plact :: PlAction -> Match ()
plact = uncurry act

act :: Player -> Action -> Match ()
act p (Goto t) = goto t p
act p (Kick t) = kick t p
act _ Idle     = return ()

actP :: PlayerID -> Action -> Match ()
actP p a = do
  s <- State.get
  case findPlayer p s of
    Nothing -> return ()
    Just p  -> act p a

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
  if not (inKickDistance s p) || (kicktimer p > 0)
    then return ()
    else do
      sModBall $ modBallvelocity $ (const (capLen3 100 vec))
      sModPendingevents $ (BallKicked:)
      sModLasttouch $ const $ Just $ playerid p
      sModPlayer (playerid p) $ modKicktimer $ const 1000

