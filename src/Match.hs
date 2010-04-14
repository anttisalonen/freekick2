{-# Language TemplateHaskell #-}
module Match(playMatch, TeamOwner(..))
where

import Control.Monad
import Control.Monad.State as State
import Data.Maybe
import Control.Applicative

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import qualified Swos
import SDLUtils
import Drawing
import DrawPitch
import DeriveMod

data TeamOwner = HumanOwner | AIOwner

data Player = Player {
    plposition :: FRange
  }
$(deriveMods ''Player)

data MatchState = MatchState {
    grasstexture :: GrassTexture
  , currkeys     :: [SDLKey]
  , pitchsize    :: (Float, Float)
  , campos       :: (Float, Float)
  , player       :: Player
  }
$(deriveMods ''MatchState)

type Match = StateT MatchState IO

playMatch :: TextureObject -> Font -> (Swos.SWOSTeam, TeamOwner) -> (Swos.SWOSTeam, TeamOwner) -> IO ()
playMatch tex _ _ _ = do
  evalStateT runMatch (initMatchState tex (16, 16) (68, 105) (20, 40))
  putStrLn "Match played! Yay!"
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

initMatchState :: TextureObject -> FRange -> FRange -> FRange -> MatchState
initMatchState tex til psize cpos = MatchState (GrassTexture tex til) [] psize cpos (initPlayer psize)

initPlayer :: FRange -> Player
initPlayer (px, py) = Player (px + 10, py / 2)

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
  ks <- currkeys <$> State.get
  when (SDLK_UP `elem` ks) $ sModCampos (goUp 1)
  when (SDLK_DOWN `elem` ks) $ sModCampos (goUp (-1))
  when (SDLK_LEFT `elem` ks) $ sModCampos (goRight (-1))
  when (SDLK_RIGHT `elem` ks) $ sModCampos (goRight 1)
  when (SDLK_w `elem` ks) $ sModPlayer $ modPlposition (goUp plspeed)
  when (SDLK_a `elem` ks) $ sModPlayer $ modPlposition (goUp (-plspeed))
  when (SDLK_s `elem` ks) $ sModPlayer $ modPlposition (goRight (-plspeed))
  when (SDLK_d `elem` ks) $ sModPlayer $ modPlposition (goRight plspeed)
  return (SDLK_ESCAPE `elem` ks)

runMatch :: Match ()
runMatch = do
  liftIO $ clear [ColorBuffer, DepthBuffer]
  s <- State.get
  (w, h) <- liftIO $ getWindowSize
  liftIO $ setCamera' (campos s, (fromIntegral (w `div` 20), fromIntegral (h `div` 20)))
  liftIO $ drawPitch (grasstexture s) (pitchsize s)
  liftIO $ glSwapBuffers
  liftIO $ SDL.delay 10
  quitting <- handleKeyEvents
  if quitting
    then return ()
    else runMatch

