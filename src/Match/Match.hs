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

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import qualified Swos
import SDLUtils
import Drawing
import DrawPitch
import Ball
import FVector
import Player
import SWOSShell

import Match.Internal.MatchState
import Match.Internal.MatchBase
import Match.Internal.AI
import Match.Internal.Actions
import Match.Internal.Formation

data TeamOwner = HumanOwner | AIOwner

data MatchTextureSet = MatchTextureSet {
    pitchtexture      :: TextureObject
  , hometexture       :: TextureObject
  , awaytexture       :: TextureObject
  , ballimginfo       :: ImageInfo
  , humandrawsize     :: FRange
  }

playMatch :: MatchTextureSet -> Font -> Font -> (Swos.SWOSTeam, TeamOwner) -> (Swos.SWOSTeam, TeamOwner) -> IO ()
playMatch texs f f2 (ht, _) (at, _) = do
  let psize = (68, 105)
      contr = Nothing
  plist <- liftIO $ defineNewList Compile (drawPitch (pitchtexture texs) (16, 16) psize)
  evalStateT runMatch (initMatchState plist psize (20, 40) texs (ht, at) contr f f2 (Swos.teamname ht) (Swos.teamname at))
  putStrLn "Match played! Yay!"
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

initMatchState :: DisplayList 
               -> FRange -> FRange 
               -> MatchTextureSet 
               -> (Swos.SWOSTeam, Swos.SWOSTeam) 
               -> Maybe PlayerID 
               -> Font -> Font
               -> String -> String
               -> MatchState
initMatchState plist psize cpos pltexs (ht, at) c f1 f2 tname1 tname2 = 
  MatchState plist [] psize cpos hps aps hf af c BeforeKickoff 
             (initialBall onPitchZ psize (ballimginfo pltexs))
             [] Nothing 0 0 f1 f2 tname1 tname2
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
    Just c  -> do -- TODO: use goto instead
      when (SDLK_w `elem` ks) $ modify $ modPlayer c $ modPlposition (goUp plspeed)
      when (SDLK_s `elem` ks) $ modify $ modPlayer c $ modPlposition (goUp (-plspeed))
      when (SDLK_a `elem` ks) $ modify $ modPlayer c $ modPlposition (goRight (-plspeed))
      when (SDLK_d `elem` ks) $ modify $ modPlayer c $ modPlposition (goRight plspeed)
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
      coords = (0, 50)
  liftIO $ do
    clear [ColorBuffer, DepthBuffer]
    let cpos = cameraCenter (fromIntegral w) (fromIntegral h) s
    setCamera' (cpos, (fromIntegral (w `div` camZoomLevel), fromIntegral (h `div` camZoomLevel)))
    callList (pitchlist s)
    mapM_ drawSprite $ sortBy (compare `on` getDepth) (SB (ball s) : map SB (M.elems (homeplayers s) ++ M.elems (awayplayers s)))
    when (pausedBallplay s) $ writeText w h (matchfont1 s) text coords
    glSwapBuffers

writeText :: Int -> Int -> Font -> String -> FRange -> IO ()
writeText w h f str (x, y) = do
  loadIdentity
  setCamera ((0, 0), (w, h))
  color $ Color3 0 0 (0 :: GLfloat)
  translate $ Vector3 (realToFrac x) (realToFrac y) (2 :: GLfloat)
  textlen <- getFontAdvance f str
  translate $ Vector3 (fromIntegral w / 2 - realToFrac textlen / 2) 0 (0 :: GLfloat)
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
        sModBallplay (const $ WaitForKickoff 2000)
    WaitForKickoff timer -> do
      when (timer > 1000) $ sModBall $ modBallposition $ const $ to3D (px / 2, py / 2) 0
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
        sModBallplay $ const $ OutOfPlayWaiting 2000 (ThrowIn restartpos)
      when (bx > py) $ do -- throwin from the right
        let restartpos = (py, by)
        sModBallplay $ const $ OutOfPlayWaiting 2000 (ThrowIn restartpos)
      when (by < 0) $ do  -- corner kick or goal kick on bottom half
        if bx > px / 2 - 3.66 && bx < px / 2 + 3.66 -- goal
          then do
            let restartpos = (px / 2, py / 2)
            sModAwaygoals succ
            sModBallplay $ const $ BeforeKickoff
          else
            if not (homeRestarts s) -- corner kick
              then do
                let restartpos =
                      if bx < px / 2
                        then (0, 0)
                        else (px, 0)
                sModBallplay $ const $ OutOfPlayWaiting 2000 (CornerKick restartpos)
              else do -- goal kick
                let restartpos =
                      if bx < px / 2
                        then (px / 2 - 9.15, 5.5)
                        else (px / 2 + 9.15, 5.5)
                sModBallplay $ const $ OutOfPlayWaiting 2000 (GoalKick restartpos)
      when (by > py) $ do  -- corner kick of goal kick on lower half
        if bx > px / 2 - 3.66 && bx < px / 2 + 3.66 -- goal
          then do
            let restartpos = (px / 2, py / 2)
            sModHomegoals succ
            sModBallplay $ const $ BeforeKickoff
          else
            if not (homeRestarts s) -- goal kick
              then do
                let restartpos =
                      if bx < px / 2
                        then (px / 2 - 9.15, py - 5.5)
                        else (px / 2 + 9.15, py + 5.5)
                sModBallplay $ const $ OutOfPlayWaiting 2000 (GoalKick restartpos)
              else do -- corner kick
                let restartpos =
                      if bx < px / 2
                        then (0, py)
                        else (px, py)
                sModBallplay $ const $ OutOfPlayWaiting 2000 (CornerKick restartpos)
    OutOfPlayWaiting timer restart -> 
      if timer > 0
        then sModBallplay $ const $ OutOfPlayWaiting (timer - fromIntegral frameTime) restart
        else sModBallplay $ const $ OutOfPlay 2000 restart
    OutOfPlay timer restart ->
      if timer > 0
        then do
          when (timer > 1000) $ sModBall $ modBallposition $ const $ to3D (getRestartPoint restart) 0
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

updateTimers :: Match ()
updateTimers = do
  sModAllPlayers (modKicktimer (\t -> max 0 (t - fromIntegral frameTime)))

runMatch :: Match ()
runMatch = do
  t1 <- liftIO $ getCPUTime
  quitting <- handleKeyEvents
  if quitting
    then return ()
    else do
      drawMatch
      execAI
      handleMatchEvents
      updateBallPosition
      updateBallPlay
      updateTimers
      t2 <- liftIO $ getCPUTime
      let tdiff = floor $ fromIntegral (t2 - t1) * (1e-9 :: Float)
      when (tdiff < frameTime) $ liftIO $ SDL.delay (frameTime - tdiff)
      runMatch

