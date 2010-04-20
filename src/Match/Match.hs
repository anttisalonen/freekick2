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
import Text.Printf
import System.Random

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import qualified Swos
import SDLUtils
import Drawing
import FVector

import Match.DrawPitch
import Match.Ball
import Match.Player
import Match.SWOSShell

import Match.State.MatchState
import Match.State.MatchBase
import Match.State.AI
import Match.State.Actions
import Match.State.Formation
import Match.State.Controls

data MatchTextureSet = MatchTextureSet {
    pitchtexture      :: TextureObject
  , hometexture       :: TextureObject
  , awaytexture       :: TextureObject
  , playershadowinfo  :: ImageInfo
  , ballimginfo       :: ImageInfo
  , ballshadowinfo    :: ImageInfo
  , humandrawsize     :: FRange
  }

playMatch :: MatchTextureSet -> Font -> Font -> (Swos.SWOSTeam, TeamOwner) -> (Swos.SWOSTeam, TeamOwner) -> IO ()
playMatch texs f f2 ht at = do
  let psize = (68, 105)
      contr = Nothing
  plist <- liftIO $ defineNewList Compile (drawPitch (pitchtexture texs) (16, 16) psize)
  evalStateT runMatch (initMatchState plist psize (20, 40) texs ht at contr f f2)
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

initMatchState :: DisplayList 
               -> FRange -> FRange 
               -> MatchTextureSet 
               -> (Swos.SWOSTeam, TeamOwner) 
               -> (Swos.SWOSTeam, TeamOwner) 
               -> Maybe PlayerID 
               -> Font -> Font
               -> MatchState
initMatchState plist psize cpos pltexs (ht, ho) (at, ao) c f1 f2 = 
  MatchState plist [] psize cpos (Team hps hf 0 (Swos.teamname ht) ho) (Team aps af 0 (Swos.teamname at) ao) c BeforeKickoff 
             (initialBall onPitchZ psize (ballimginfo pltexs) (ballshadowinfo pltexs))
             [] Nothing f1 f2 (mkStdGen 21) (False, 0)
  where hps = createPlayers True pltexs psize ht
        aps = createPlayers False pltexs psize at
        hf  = createFormation True hps
        af  = createFormation False aps

onPitchZ :: Float
onPitchZ = 1

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
                                     (playershadowinfo texs)
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

fetchKeyEvents :: Match Bool
fetchKeyEvents = do
  evts <- liftIO $ pollAllSDLEvents
  sModCurrkeys $ updateKeyMap (keyChanges evts)
  s <- State.get
  let ks = currkeys s
  return (SDLK_ESCAPE `elem` ks)

frameTime :: Word32 -- milliseconds
frameTime = 10

camZoomLevel :: (Num a) => a
camZoomLevel = 20

cameraCenter w h s =
   let (bx, by) =
         case ballplay s of
           OutOfPlayWaiting _ r -> getRestartPoint r
           OutOfPlay _ r        -> getRestartPoint r
           RestartPlay r        -> getRestartPoint r
           _                    -> to2D (ballposition $ ball s)
   in (bx - (w / (2 * camZoomLevel)), by - (h / (2 * camZoomLevel)))

drawMatch :: Match ()
drawMatch = do
  s <- State.get
  (w, h) <- liftIO $ getWindowSize
  let text = printf "%16s %d - %d %-16s" (hometeamname s) (homegoals s) (awaygoals s) (awayteamname s)
      coords = (fromIntegral w / 2, 50)
  liftIO $ do
    clear [ColorBuffer, DepthBuffer]
    let cpos = cameraCenter (fromIntegral w) (fromIntegral h) s
    setCamera' (cpos, (fromIntegral (w `div` camZoomLevel), fromIntegral (h `div` camZoomLevel)))
    callList (pitchlist s)
    drawBallShadow $ ball s
    mapM_ drawPlayerShadow (allPlayers s)
    mapM_ drawSprite $ sortBy (compare `on` getDepth) (SB (ball s) : map SB (allPlayers s))
    case controlledpl s of
      Nothing            -> return ()
      Just i@(pid, _) -> case findPlayer i s of
                              Nothing -> return ()
                              Just pl -> do
                                let (plx, ply) = plposition pl
                                writeOnPitch (matchfont2 s) (show pid) (plx, ply + 2)
    -- writeTexts come in last, as they reset the camera.
    when (pausedBallplay s) $ writeText w h (matchfont1 s) text coords
    writeText w h (matchfont2 s) (printf "%d min" (floor (snd (matchtime s)) `div` (60 :: Int))) (50, fromIntegral h - 50)
    glSwapBuffers

uniformScale :: GLfloat -> IO ()
uniformScale n = scale n n n

writeOnPitch :: Font -> String -> FRange -> IO ()
writeOnPitch f str (x, y) = do
  loadIdentity
  color $ Color3 1 1 (1 :: GLfloat)
  translate $ Vector3 (realToFrac x) (realToFrac y) (2 :: GLfloat)
  uniformScale (1 / camZoomLevel)
  textlen <- getFontAdvance f str
  translate $ Vector3 (-(realToFrac textlen / 2)) 0 (0 :: GLfloat)
  renderFont f str FTGL.Front

writeText :: Int -> Int -> Font -> String -> FRange -> IO ()
writeText w h f str (x, y) = do
  loadIdentity
  setCamera ((0, 0), (w, h))
  color $ Color3 1 1 (1 :: GLfloat)
  translate $ Vector3 (realToFrac x) (realToFrac y) (2 :: GLfloat)
  textlen <- getFontAdvance f str
  translate $ Vector3 (-(realToFrac textlen / 2)) 0 (0 :: GLfloat)
  renderFont f str FTGL.Front

playerOnHisSide :: MatchState -> Player -> Bool
playerOnHisSide m p =
  let (_, y) = absToRel m (plposition p)
      home   = playerHome p
  in if home then y <= 0.5 else y >= 0.5

updateBallPlay :: Match ()
updateBallPlay = do
  s <- State.get
  let (px, py) = pitchsize s
  case ballplay s of
    BeforeKickoff -> do
      when (all (playerOnHisSide s) (allPlayers s)) $
        sModBallplay (const $ WaitForKickoff 1000)
    WaitForKickoff timer -> do
      when (timer < 1000) $ sModBall $ modBallposition $ const $ to3D (px / 2, py / 2) 0
      sModBall $ modBallvelocity $ const nullFVector3
      if timer < 0
        then sModBallplay (const DoKickoff)
        else sModBallplay (const $ WaitForKickoff (timer - fromIntegral frameTime))
    DoKickoff -> do
      return () -- updated by handleMatchEvent BallKicked
    InPlay -> do
      let (bx, by) = to2D $ ballposition $ ball s
      when (bx < 0) $ do -- throwin from the left
        let restartpos = (0, by)
        sModBallplay $ const $ OutOfPlayWaiting 1000 (ThrowIn restartpos)
      when (bx > px) $ do -- throwin from the right
        let restartpos = (px, by)
        sModBallplay $ const $ OutOfPlayWaiting 1000 (ThrowIn restartpos)
      when (by < 0) $ do  -- corner kick or goal kick on bottom half
        if bx > px / 2 - 3.66 && bx < px / 2 + 3.66 -- goal
          then do
            let restartpos = (px / 2, py / 2)
            sModAwaygoals succ
            sModBallplay $ const BeforeKickoff
          else
            if not (homeRestarts s) -- corner kick
              then do
                let restartpos =
                      if bx < px / 2
                        then (0, 0)
                        else (px, 0)
                sModBallplay $ const $ OutOfPlayWaiting 1000 (CornerKick restartpos)
              else do -- goal kick
                let restartpos =
                      if bx < px / 2
                        then (px / 2 - 9.15, 5.5)
                        else (px / 2 + 9.15, 5.5)
                sModBallplay $ const $ OutOfPlayWaiting 1000 (GoalKick restartpos)
      when (by > py) $ do  -- corner kick of goal kick on lower half
        if bx > px / 2 - 3.66 && bx < px / 2 + 3.66 -- goal
          then do
            let restartpos = (px / 2, py / 2)
            sModHomegoals succ
            sModBallplay $ const BeforeKickoff
          else
            if not (homeRestarts s) -- goal kick
              then do
                let restartpos =
                      if bx < px / 2
                        then (px / 2 - 9.15, py - 5.5)
                        else (px / 2 + 9.15, py - 5.5)
                sModBallplay $ const $ OutOfPlayWaiting 1000 (GoalKick restartpos)
              else do -- corner kick
                let restartpos =
                      if bx < px / 2
                        then (0, py)
                        else (px, py)
                sModBallplay $ const $ OutOfPlayWaiting 1000 (CornerKick restartpos)
    OutOfPlayWaiting timer restart -> 
      if timer > 0
        then sModBallplay $ const $ OutOfPlayWaiting (timer - fromIntegral frameTime) restart
        else sModBallplay $ const $ OutOfPlay 1000 restart
    OutOfPlay timer restart ->
      if timer > 0
        then do
          when (timer < 1000) $ sModBall $ modBallposition $ const $ to3D (getRestartPoint restart) 0
          sModBall $ modBallvelocity $ const nullFVector3
          sModBallplay $ const $ OutOfPlay (timer - fromIntegral frameTime) restart
        else sModBallplay $ const $ RestartPlay restart
    RestartPlay _ -> do
      when ((to2D $ ballposition (ball s)) `inside2s` ((0, 0), (pitchsize s))) $
        sModBallplay $ const InPlay

handleMatchEvent :: MatchEvent -> Match ()
handleMatchEvent BallKicked = do
  s <- State.get
  case ballplay s of
    DoKickoff -> do
      sModBallplay $ const InPlay
    _ -> return ()

handleMatchEvents :: Match ()
handleMatchEvents = do
  s <- State.get
  mapM_ handleMatchEvent (pendingevents s)
  sModPendingevents $ const []

updateBallPosition :: Match ()
updateBallPosition = do
  s <- State.get
  let dt = fromIntegral frameTime / 1000
  sModBall $ modBallposition (*+* ((ballvelocity (ball s)) *** (fromIntegral frameTime / 1000)))
  sModBall $ collCheckBall
  sModBall $ gravitateBall dt
  sModBall $ slowDownBall dt

execAI :: Match ()
execAI = do
  s <- State.get
  let plactions = doAI s
  mapM_ plact plactions

modFst :: (a -> a) -> (a, b) -> (a, b)
modFst f (a, b) = (f a, b)

modSnd :: (b -> b) -> (a, b) -> (a, b)
modSnd f (a, b) = (a, f b)

updateTimers :: Match ()
updateTimers = do
  sModAllPlayers (modKicktimer (\t -> max 0 (t - fromIntegral frameTime)))
  s <- State.get
  when (inPlay (ballplay s)) $ sModMatchtime $ modSnd $ (+ ((fromIntegral frameTime / 1000) * 30))

controllable :: BallPlay -> Bool
controllable InPlay                     = True
controllable DoKickoff                  = False
controllable (RestartPlay (GoalKick _)) = False
controllable (RestartPlay _)            = False
controllable _                          = False

setControl :: Bool -> Match ()
setControl h = do
  s <- State.get
  let p = if h
            then nearestToBallHwoGK s
            else nearestToBallAwoGK s
  sModControlledpl $ const $ Just $ playerid p

setControlledPlayer :: Match ()
setControlledPlayer = do
  s <- State.get
  if (controllable (ballplay s))
    then
      if teamowner (hometeam s) == HumanOwner
        then setControl True
        else if teamowner (awayteam s) == HumanOwner
               then setControl False
               else sModControlledpl $ const Nothing
     else sModControlledpl $ const Nothing

runMatch :: Match ()
runMatch = do
  t1 <- liftIO $ getCPUTime
  quitting <- fetchKeyEvents
  if quitting
    then return ()
    else do
      handleControls
      drawMatch
      setControlledPlayer
      execAI
      handleMatchEvents
      updateBallPosition
      updateBallPlay
      updateTimers
      t2 <- liftIO $ getCPUTime
      let tdiff = floor $ fromIntegral (t2 - t1) * (1e-9 :: Float)
      when (tdiff < frameTime) $ liftIO $ SDL.delay (frameTime - tdiff)
      runMatch

