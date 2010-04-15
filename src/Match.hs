{-# LANGUAGE TemplateHaskell #-}
module Match(playMatch, TeamOwner(..),
  MatchTextureSet(..))
where

import Control.Monad
import Control.Monad.State as State
import Data.Maybe
import qualified Data.IntMap as M
import System.CPUTime
import Data.Word

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import qualified Swos
import SDLUtils
import Drawing
import DrawPitch
import DeriveMod

data TeamOwner = HumanOwner | AIOwner

data MatchTextureSet = MatchTextureSet {
    pitchtexture      :: TextureObject
  , hometexture       :: TextureObject
  , awaytexture       :: TextureObject
  , humandrawsize     :: (Int, Int)
  }

data Player = Player {
    plposition :: FRange
  , plimage    :: ImageInfo
  , plposz     :: Float
  , plnumber   :: Int
  }
$(deriveMods ''Player)

type PlayerID = (Int, Bool)
type PlayerMap = M.IntMap Player

data MatchState = MatchState {
    pitchlist    :: DisplayList
  , currkeys     :: [SDLKey]
  , pitchsize    :: (Float, Float)
  , campos       :: (Float, Float)
  , homeplayers  :: PlayerMap
  , awayplayers  :: PlayerMap
  , controlledpl :: Maybe PlayerID
  }
$(deriveMods ''MatchState)

modPlayer :: PlayerID -> (Player -> Player) -> MatchState -> MatchState
modPlayer (pln, True)  f = modHomeplayers (M.adjust f pln)
modPlayer (pln, False) f = modAwayplayers (M.adjust f pln)

type Match = StateT MatchState IO

playMatch :: MatchTextureSet -> Font -> (Swos.SWOSTeam, TeamOwner) -> (Swos.SWOSTeam, TeamOwner) -> IO ()
playMatch texs _ (ht, _) (at, _) = do
  let psize = (68, 105)
      contr = Just (5, True)
  plist <- liftIO $ defineNewList Compile (drawPitch (pitchtexture texs) (16, 16) psize)
  evalStateT runMatch (initMatchState plist psize (20, 40) texs (ht, at) contr)
  putStrLn "Match played! Yay!"
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

initMatchState :: DisplayList -> FRange -> FRange -> MatchTextureSet -> (Swos.SWOSTeam, Swos.SWOSTeam) -> Maybe PlayerID -> MatchState
initMatchState plist psize cpos pltexs (ht, at) c = MatchState plist [] psize cpos hps aps c
  where hps = createPlayers True pltexs psize ht
        aps = createPlayers False pltexs psize at

swosPlayerToPlayer :: Bool -> MatchTextureSet -> FRange -> Swos.SWOSPlayer -> Player
swosPlayerToPlayer home texs (px, py) p = 
  Player (px - 10, py / 2) (ImageInfo tex size) 1 (Swos.plnumber p)
    where tex = if home then hometexture texs else awaytexture texs
          size = humandrawsize texs

createPlayers :: Bool -> MatchTextureSet -> FRange -> Swos.SWOSTeam -> PlayerMap
createPlayers home texs psize t =
  let (d, m, f) = Swos.numPositions (Swos.teamtactics t)
      g  = take 1 $ filter (\p -> Swos.isGoalkeeper (Swos.plposition p)) (Swos.teamplayers t)
      ds = take d $ filter (\p -> Swos.isDefender (Swos.plposition p)) (Swos.teamplayers t)
      ms = take m $ filter (\p -> Swos.isMidfielder (Swos.plposition p)) (Swos.teamplayers t)
      fs = take f $ filter (\p -> Swos.isAttacker (Swos.plposition p)) (Swos.teamplayers t)
      pllist = g ++ ds ++ ms ++ fs
      plnums = map Swos.plnumber pllist
  in M.fromList (zip plnums (map (swosPlayerToPlayer home texs psize) pllist))

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

playerTexRectangle :: Player -> Rectangle
playerTexRectangle p =
  ((a, b), (c, d))
    where (a, b) = plposition p
          (c, d) = (fromIntegral e, fromIntegral f)
          (e, f) = imgsize $ plimage p

drawPlayer :: Player -> IO ()
drawPlayer p = drawSprite (imgtexture (plimage p)) (playerTexRectangle p) (plposz p)

frameTime :: Word32 -- milliseconds
frameTime = 10

runMatch :: Match ()
runMatch = do
  t1 <- liftIO $ getCPUTime
  liftIO $ clear [ColorBuffer, DepthBuffer]
  s <- State.get
  (w, h) <- liftIO $ getWindowSize
  liftIO $ setCamera' (campos s, (fromIntegral (w `div` 20), fromIntegral (h `div` 20)))
  liftIO $ callList (pitchlist s)
  liftIO $ mapM_ drawPlayer (M.elems $ homeplayers s)
  liftIO $ mapM_ drawPlayer (M.elems $ awayplayers s)
  liftIO $ glSwapBuffers
  quitting <- handleKeyEvents
  if quitting
    then return ()
    else do
      t2 <- liftIO $ getCPUTime
      let tdiff = floor $ fromIntegral (t2 - t1) * (1e-9 :: Float)
      when (tdiff < frameTime) $ liftIO $ SDL.delay (frameTime - tdiff)
      runMatch

