{-# LANGUAGE TemplateHaskell #-}
module Match.Match(playMatch, TeamOwner(..),
  MatchTextureSet(..))
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

import Match.Internal.MatchState

data TeamOwner = HumanOwner | AIOwner

data MatchTextureSet = MatchTextureSet {
    pitchtexture      :: TextureObject
  , hometexture       :: TextureObject
  , awaytexture       :: TextureObject
  , ballimginfo       :: ImageInfo
  , humandrawsize     :: FRange
  }

playMatch :: MatchTextureSet -> Font -> (Swos.SWOSTeam, TeamOwner) -> (Swos.SWOSTeam, TeamOwner) -> IO ()
playMatch texs _ (ht, _) (at, _) = do
  let psize = (68, 105)
      contr = Nothing
  plist <- liftIO $ defineNewList Compile (drawPitch (pitchtexture texs) (16, 16) psize)
  evalStateT runMatch (initMatchState plist psize (20, 40) texs (ht, at) contr)
  putStrLn "Match played! Yay!"
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

initMatchState :: DisplayList 
               -> FRange -> FRange 
               -> MatchTextureSet 
               -> (Swos.SWOSTeam, Swos.SWOSTeam) 
               -> Maybe PlayerID -> MatchState
initMatchState plist psize cpos pltexs (ht, at) c = 
  MatchState plist [] psize cpos hps aps hf af c BeforeKickoff 
             (initialBall onPitchZ psize (ballimginfo pltexs))
             [] Nothing
  where hps = createPlayers True pltexs psize ht
        aps = createPlayers False pltexs psize at
        hf  = createFormation True hps
        af  = createFormation False aps

onPitchZ :: Float
onPitchZ = 1

mkGoalkeeperFormation :: Bool -> [Player] -> Formation
mkGoalkeeperFormation True  pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 0)))
mkGoalkeeperFormation False pls = M.fromList (zip (map playerNumber pls) (repeat (0.5, 1)))

mkDefenderFormation :: Bool -> [Player] -> Formation
mkDefenderFormation = kickoffPositions (0.5, 0.2) defs
  where defs 1 = []
        defs 2 = [(0.3, 0.2), (0.7, 0.2)]
        defs 3 = [(0.25, 0.25), (0.75, 0.25)]
        defs 4 = [(0.2, 0.25), (0.8, 0.25), (0.4, 0.2), (0.6, 0.2)]
        defs 5 = [(0.2, 0.25), (0.8, 0.25), (0.4, 0.2), (0.6, 0.2)]
        defs _ = repeat (0.5, 0.2)

kickoffPositions :: FRange -> (Int -> [FRange]) -> Bool -> [Player] -> Formation
kickoffPositions cnt others home pls =
  let numpls = length pls
      hasscentre = numpls `mod` 2 == 1
      pairpls = if hasscentre then drop 1 pls else pls
      cplrpos = cnt
      pairposs = others numpls
      plrposs = if hasscentre
                  then cplrpos : take (numpls - 1) pairposs
                  else take numpls pairposs
      plrposs' = if home then plrposs else map flipSide plrposs
  in M.fromList (zip (map playerNumber pls) (plrposs'))

flipSide :: FRange -> FRange
flipSide (x, y) = (1 - x, 1 - y)

mkMidfielderFormation :: Bool -> [Player] -> Formation
mkMidfielderFormation = kickoffPositions (0.5, 0.3) mids
  where mids 1 = []
        mids 2 = [(0.3, 0.3), (0.7, 0.3)]
        mids 3 = [(0.25, 0.4), (0.75, 0.4)]
        mids 4 = [(0.1, 0.45), (0.9, 0.45), (0.4, 0.3), (0.6, 0.3)]
        mids 5 = [(0.1, 0.45), (0.9, 0.45), (0.4, 0.3), (0.6, 0.3)]
        mids _ = repeat (0.5, 0.3)

mkAttackerFormation :: Bool -> [Player] -> Formation
mkAttackerFormation = kickoffPositions (0.5, 0.4) poss
  where poss 1 = []
        poss 2 = [(0.3, 0.4), (0.7, 0.4)]
        poss 3 = [(0.25, 0.45), (0.75, 0.45)]
        poss 4 = [(0.2, 0.45), (0.8, 0.45), (0.4, 0.4), (0.6, 0.4)]
        poss _ = repeat (0.5, 0.4)

createFormation :: Bool -> PlayerMap -> Formation
createFormation home pls' =
  let pls = M.elems pls'
      gs = filter (\p -> plpos p == Goalkeeper) pls
      ds = filter (\p -> plpos p == Defender) pls
      ms = filter (\p -> plpos p == Midfielder) pls
      fs = filter (\p -> plpos p == Attacker) pls
      gmap = mkGoalkeeperFormation home gs
      dmap = mkDefenderFormation home ds
      mmap = mkMidfielderFormation home ms
      fmp = mkAttackerFormation home fs
  in gmap `M.union` dmap `M.union` mmap `M.union` fmp

createPlayers :: Bool -> MatchTextureSet -> FRange -> Swos.SWOSTeam -> PlayerMap
createPlayers home texs psize t =
  let (d, m, f) = Swos.numPositions (Swos.teamtactics t)
      g  = take 1 $ filter (\p -> Swos.isGoalkeeper (Swos.plposition p)) (Swos.teamplayers t)
      ds = take d $ filter (\p -> Swos.isDefender (Swos.plposition p)) (Swos.teamplayers t)
      ms = take m $ filter (\p -> Swos.isMidfielder (Swos.plposition p)) (Swos.teamplayers t)
      fs = take f $ filter (\p -> Swos.isAttacker (Swos.plposition p)) (Swos.teamplayers t)
      pllist = g ++ ds ++ ms ++ fs
      plnums = map Swos.plnumber pllist
  in M.fromList 
       (zip plnums 
            (map (swosPlayerToPlayer onPitchZ
                                     home 
                                     (hometexture texs) 
                                     (awaytexture texs) 
                                     (humandrawsize texs) 
                                     psize) 
                 pllist))

keyChanges :: [SDL.Event] -> [(SDLKey, Bool)]
keyChanges = catMaybes . map f
  where f (KeyDown (Keysym n _ _)) = Just (n, True)
        f (KeyUp   (Keysym n _ _)) = Just (n, False)
        f _                        = Nothing

updateKeyMap :: [(SDLKey, Bool)] -> [SDLKey] -> [SDLKey]
updateKeyMap []              m = m
updateKeyMap ((k, True):ns)  m = updateKeyMap ns (k:m)
updateKeyMap ((k, False):ns) m = updateKeyMap ns (filter (/= k) m)

plspeed :: Float
plspeed = 0.2

handleKeyEvents :: Match Bool
handleKeyEvents = do
  evts <- liftIO $ pollAllSDLEvents
  sModCurrkeys $ updateKeyMap (keyChanges evts)
  s <- State.get
  let ks = currkeys s
  when (SDLK_UP `elem` ks) $ sModCampos (goUp 1)
  when (SDLK_DOWN `elem` ks) $ sModCampos (goUp (-1))
  when (SDLK_LEFT `elem` ks) $ sModCampos (goRight (-1))
  when (SDLK_RIGHT `elem` ks) $ sModCampos (goRight 1)
  case controlledpl s of
    Nothing -> return ()
    Just c  -> do
      when (SDLK_w `elem` ks) $ modify $ modPlayer c $ modPlposition (goUp plspeed)
      when (SDLK_s `elem` ks) $ modify $ modPlayer c $ modPlposition (goUp (-plspeed))
      when (SDLK_a `elem` ks) $ modify $ modPlayer c $ modPlposition (goRight (-plspeed))
      when (SDLK_d `elem` ks) $ modify $ modPlayer c $ modPlposition (goRight plspeed)
  return (SDLK_ESCAPE `elem` ks)

frameTime :: Word32 -- milliseconds
frameTime = 10

drawMatch :: Match ()
drawMatch = do
  s <- State.get
  (w, h) <- liftIO $ getWindowSize
  liftIO $ do
    clear [ColorBuffer, DepthBuffer]
    setCamera' (campos s, (fromIntegral (w `div` 20), fromIntegral (h `div` 20)))
    callList (pitchlist s)
    mapM_ drawPlayer $ sortBy (compare `on` playerHeight) (M.elems (homeplayers s) ++ M.elems (awayplayers s))
    drawBall (ball s)
    glSwapBuffers

aiControlled :: MatchState -> PlayerID -> Bool
aiControlled s n =
  case controlledpl s of
    Nothing -> True
    Just p  -> n /= p

formationPosition :: MatchState -> Player -> FRange
formationPosition m pl =
  let (plnum, plhome) = playerid pl 
      sourcemap       = if plhome then homeformation m else awayformation m
  in M.findWithDefault (0.5, 0.5) plnum sourcemap

absToRel :: MatchState -> FRange -> FRange
absToRel m (x, y) =
  let (px, py) = pitchsize m
  in (x / px, y / py)

relToAbs' :: FRange -> FRange -> FRange
relToAbs' (px, py) (x, y) =
  (px * x, py * y)

relToAbs :: MatchState -> FRange -> FRange
relToAbs m c =
  let ps = pitchsize m
  in relToAbs' ps c

formationPositionAbs :: MatchState -> Player -> FRange
formationPositionAbs m pl =
  let rel = formationPosition m pl
  in relToAbs m rel

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

kickoffer :: MatchState -> PlayerID
kickoffer m =
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ homeplayers m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ homeplayers m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ homeplayers m)
  in playerid $ head (forws ++ mids ++ defs)

kickoffAssister :: MatchState -> PlayerID
kickoffAssister m =
  let forws = filter (\p -> plpos p == Attacker) (M.elems $ homeplayers m)
      mids = filter (\p -> plpos p == Midfielder) (M.elems $ homeplayers m)
      defs = filter (\p -> plpos p == Defender) (M.elems $ homeplayers m)
  in playerid $ head $ tail (forws ++ mids ++ defs)

shouldDoKickoff :: MatchState -> Player -> Bool
shouldDoKickoff m pl = kickoffer m == playerid pl
  
shouldAssistKickoff :: MatchState -> Player -> Bool
shouldAssistKickoff m pl = kickoffAssister m == playerid pl

kick :: FVector3 -> Player -> Match ()
kick vec p = do
  s <- State.get
  if not (inKickDistance s p)
    then return ()
    else do
      sModBall $ modBallvelocity $ const vec
      sModPendingactions $ (BallKicked:)
      sModLasttouch $ const $ Just $ playerid p

inKickDistance :: MatchState -> Player -> Bool
inKickDistance m p = 
  let (bx, by, bz) = ballposition (ball m)
      pp = plposition p
  in if bz > 0.8
       then False
       else dist2 (bx, by) pp < 0.5

kickoff :: Player -> Match ()
kickoff p = do
  s <- State.get
  if not (inKickDistance s p)
    then goto (to2D (ballposition (ball s))) p
    else kick (20, 0, 0) p

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

oppositeGoalAbs :: FRange -> Bool -> FRange
oppositeGoalAbs ps home =
  if home
    then relToAbs' ps (0.5, 1.0)
    else relToAbs' ps (0.5, 0.0)

ownPlayers :: MatchState -> Player -> [Player]
ownPlayers m pl =
  if playerHome pl
    then M.elems (homeplayers m)
    else M.elems (awayplayers m)

opponentPlayers :: MatchState -> Player -> [Player]
opponentPlayers m pl = 
  if not $ playerHome pl
    then M.elems (homeplayers m)
    else M.elems (awayplayers m)

offBallAI :: Player -> Match ()
offBallAI pl = do
  s <- State.get
  goto (formationPositionAbs s pl) pl

allPlayers :: MatchState -> [Player]
allPlayers m = M.elems (homeplayers m) ++ (M.elems (awayplayers m))

playerOnHisSide :: MatchState -> Player -> Bool
playerOnHisSide m p =
  let (_, y) = absToRel m (plposition p)
      home   = playerHome p
  in if home then y <= 0.5 else y >= 0.5

updateBallPlay :: Match ()
updateBallPlay = do
  s <- State.get
  case ballplay s of
    BeforeKickoff -> do
      when (all (playerOnHisSide s) (allPlayers s)) $
        sModBallplay (const $ WaitForKickoff 2000)
    WaitForKickoff timer -> do
      if timer < 0
        then sModBallplay (const DoKickoff)
        else sModBallplay (const $ WaitForKickoff (timer - fromIntegral frameTime))
    DoKickoff -> do
      return () -- updated by handleAction BallKicked
    InPlay -> do
      return () -- TODO

handleAction :: Action -> Match ()
handleAction BallKicked = do
  s <- State.get
  case ballplay s of
    DoKickoff -> do
      sModBallplay $ const InPlay
    _ -> return ()

handleActions :: Match ()
handleActions = do
  s <- State.get
  mapM_ handleAction (pendingactions s)
  sModPendingactions $ const []

updateBallPosition :: Match ()
updateBallPosition = do
  s <- State.get
  let dt = fromIntegral frameTime / 1000
  sModBall $ modBallposition (*+* ((ballvelocity (ball s)) *** (fromIntegral frameTime / 1000)))
  sModBall $ collCheckBall
  sModBall $ gravitateBall dt
  sModBall $ slowDownBall dt

runMatch :: Match ()
runMatch = do
  t1 <- liftIO $ getCPUTime
  quitting <- handleKeyEvents
  if quitting
    then return ()
    else do
      drawMatch
      doAI
      handleActions
      updateBallPosition
      updateBallPlay
      t2 <- liftIO $ getCPUTime
      let tdiff = floor $ fromIntegral (t2 - t1) * (1e-9 :: Float)
      when (tdiff < frameTime) $ liftIO $ SDL.delay (frameTime - tdiff)
      runMatch

