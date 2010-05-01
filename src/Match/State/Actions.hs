module Match.State.Actions(Action(..),
  PlAction,
  act,
  actP,
  plact)
where

import Control.Monad.State as State
import System.Random

import Utils
import FVector
import qualified Gen

import Match.Ball
import Match.Player

import Match.State.MatchState
import Match.State.MatchBase

data Action = Goto FRange | Kick FVector3 | HoldBall | Idle

type PlAction = (Player, Action)

plact :: PlAction -> Match ()
plact = uncurry act

act :: Player -> Action -> Match ()
act p (Goto t)    = goto t p
act p (Kick t)    = kick t p
act p (HoldBall)  = hold p
act p Idle        = idle p

actP :: PlayerID -> Action -> Match ()
actP p a = do
  s <- State.get
  case findPlayer p s of
    Nothing -> return ()
    Just pl  -> act pl a

playerControlCoeff :: Float -> Float -> Player -> Float
playerControlCoeff mn mx pl =
  mn + (mx - mn) * (Gen.controlskill $ plskills pl)

goto :: FRange -> Player -> Match ()
goto (x, y) pl = do
  s <- State.get
  let dt = frametime s
  let (curx,  cury)  = plposition pl
      (diffx, diffy) = (curx - x, cury - y)
      c              = playerid pl
      spd            = plspeed (plspeedcoeff (params s)) (plspeedmin (params s)) dt pl
      atpos          = abs diffx < spd && abs diffy < spd
      addvec = if atpos
                 then (-diffx, -diffy)
                 else
                   let ang = atan2 diffy diffx
                       xvel = cos ang * spd
                       yvel = sin ang * spd
                   in (-xvel, -yvel)
      dribbling = canDribble s pl && len2 addvec > 0.0000001
      (px, py) = pitchsize s
      runvec = if dribbling 
                 then addvec `mul2` playerControlCoeff (plcontrolmin (params s)) (plcontrolmax (params s)) pl 
                 else addvec
  when dribbling $ do
    sModBall $ modBallposition $ const $ to3D (plposition pl `add2` (addvec `mul2` 2)) 0
    sModBall $ modBallvelocity (const nullFVector3)
    sModPendingevents $ (BallKicked:)
    sModLasttouch $ const $ Just $ playerid pl
  doRot (Just (x, y)) pl
  let npos = clamp2 (-4, -0.2) (px + 4, py + 0.2) (plposition pl `add2` runvec)
  modify $ modPlayer c $ modPlposition $ const npos

getRandomR :: (Random a) => (a, a) -> State StdGen a
getRandomR v = State $ \s -> randomR v s

getKickVec :: FVector3 -> Player -> Match FVector3
getKickVec v p = do
  s <- State.get
  let (x, y, z) = capLen3 (maxkickveclen (params s)) v
      relskill = if len2 (x, y) < (stillpassveclen (params s)) && z < (stillpassvecheight (params s))
                   then Gen.passingskill $ plskills p
                   else Gen.shootingskill $ plskills p
      vlen = len3 (x, y, z)
  let xvarmax = vlen * ((maxkickvar (params s)) * (1 - relskill))
  let yvarmax = vlen * ((maxkickvar (params s)) * (1 - relskill))
  let zvarmax = abs z * ((maxkickvarz (params s)) * (1 - relskill))
  let (vc, g') = flip runState (randomgen s) $ do
                  x' <- getRandomR (-xvarmax, xvarmax)
                  y' <- getRandomR (-yvarmax, yvarmax)
                  z' <- getRandomR (0, zvarmax)
                  return (x + x', y + y', z + z')
  sModRandomgen $ const g'
  -- liftIO $ putStrLn $ "Orig: " ++ (show $ capLen3 40 v)
  -- liftIO $ putStrLn $ "New:  " ++ (show $ capLen3 40 vc)
  return $ capLen3 (maxkickveclen (params s)) vc

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
      sModPlayer (playerid p) $ modKicktimer $ const (setkicktimer (params s))

hold :: Player -> Match ()
hold pl = do
  s <- State.get
  when (inCatchDistance s pl) $ do
    sModBall $ modBallvelocity $ const $ nullFVector3
    sModBall $ modBallposition $ const $ to3D (plposition pl) 0
    sModLasttouch $ const $ Just $ playerid pl

doRot :: Maybe FRange -> Player -> Match ()
doRot mtgt pl = do
  s <- State.get
  let (curx, cury)   = plposition pl
      bpos@(bx, by)  = to2D $ ballposition $ ball s
      gpos           = oppositeGoalAbs s pl
      atball         = abs (curx - bx) < 0.5 && abs (cury - by) < 0.5
      maintgtsimple  = if atball then gpos else bpos
      (ttx, tty)     = case mtgt of
        Nothing       -> maintgtsimple
        Just (tx, ty) -> let attgt = abs (curx - tx) < 0.5 && abs (cury - ty) < 0.5
                         in if attgt
                              then maintgtsimple
                              else (tx, ty)
  -- yup, this is correct (0 degrees = north)
  sModPlayer (playerid pl) $ modPlrotation $ const $ wrap 0 360 $ radToDeg $ atan2 (ttx - curx) (tty - cury)

idle :: Player -> Match ()
idle pl = doRot Nothing pl

