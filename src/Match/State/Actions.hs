module Match.State.Actions(Action(..),
  PlAction,
  act,
  actP,
  plact)
where

import Control.Monad.State as State
import System.Random

import FVector

import Match.Ball
import Match.Player

import Match.State.MatchState
import Match.State.MatchBase

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
    Just pl  -> act pl a

playerControlCoeff :: Player -> Float
playerControlCoeff pl =
  0.6 + 0.4 * (controlskill $ plskills pl)

goto :: FRange -> Player -> Match ()
goto (x, y) pl = do
  s <- State.get
  let dt = frametime s
  let (curx,  cury)  = plposition pl
      (diffx, diffy) = (curx - x, cury - y)
      c              = playerid pl
      addvec = if abs diffx < plspeed dt pl && abs diffy < plspeed dt pl
                 then (-diffx, -diffy)
                 else
                   let ang = atan2 diffy diffx
                       xvel = cos ang * plspeed dt pl
                       yvel = sin ang * plspeed dt pl
                   in (-xvel, -yvel)
      dribbling = canDribble s pl && len2 addvec > 0.0000001
      runvec = if dribbling then addvec `mul2` playerControlCoeff pl else addvec
  when dribbling $ do
    -- liftIO $ putStrLn $ "Dribbling to velocity: " ++ show addvec
    sModBall $ modBallposition $ const $ to3D (plposition pl `add2` (addvec `mul2` 2)) 0
    -- sModBall $ modBallvelocity (const nullFVector3)
    sModPendingevents $ (BallKicked:)
    sModLasttouch $ const $ Just $ playerid pl
  modify $ modPlayer c $ modPlposition $ add2 runvec

getRandomR :: (Random a) => (a, a) -> State StdGen a
getRandomR v = State $ \s -> randomR v s

getKickVec :: FVector3 -> Player -> Match FVector3
getKickVec v p = do
  s <- State.get
  let (x, y, z) = capLen3 50 v
      relskill = if len2 (x, y) < 20 && z < 2
                   then passingskill $ plskills p
                   else shootingskill $ plskills p
      vlen = len3 (x, y, z)
  let xvarmax = vlen * (0.5 * (1 - relskill))
  let yvarmax = vlen * (0.5 * (1 - relskill))
  let zvarmax = abs z * (0.5 * (1 - relskill))
  let (vc, g') = flip runState (randomgen s) $ do
                  x' <- getRandomR (-xvarmax, xvarmax)
                  y' <- getRandomR (-yvarmax, yvarmax)
                  z' <- getRandomR (0, zvarmax)
                  return (x + x', y + y', z + z')
  sModRandomgen $ const g'
  -- liftIO $ putStrLn $ "Orig: " ++ (show $ capLen3 40 v)
  -- liftIO $ putStrLn $ "New:  " ++ (show $ capLen3 40 vc)
  return $ capLen3 50 vc

kick :: FVector3 -> Player -> Match ()
kick vec p = do
  s <- State.get
  if not (inKickDistance s p) || (kicktimer p > 0)
    then return ()
    else do
      kvec <- getKickVec vec p
      sModBall $ modBallvelocity $ const kvec
      sModPendingevents $ (BallKicked:)
      sModLasttouch $ const $ Just $ playerid p
      sModPlayer (playerid p) $ modKicktimer $ const 1000

