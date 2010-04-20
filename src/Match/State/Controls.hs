module Match.State.Controls(handleInput)
where

import Control.Monad
import Control.Monad.State as State
import Data.Maybe

import Graphics.UI.SDL as SDL

import FVector
import SDLUtils

import Match.Player

import Match.State.MatchState
import Match.State.MatchBase
import Match.State.Actions

keyChanges :: [SDL.Event] -> [(SDLKey, Bool)]
keyChanges = catMaybes . map f
  where f (KeyDown (Keysym n _ _)) = Just (n, True)
        f (KeyUp   (Keysym n _ _)) = Just (n, False)
        f _                        = Nothing

updateKeyMap :: [(SDLKey, Bool)] -> [SDLKey] -> [SDLKey]
updateKeyMap []              m = m
updateKeyMap ((k, True):ns)  m = updateKeyMap ns (k:m)
updateKeyMap ((k, False):ns) m = updateKeyMap ns (filter (/= k) m)

handleInput :: (Integral a) => a -> Match Bool
handleInput frametime = do
  evts <- liftIO $ pollAllSDLEvents
  sModCurrkeys $ updateKeyMap (keyChanges evts)
  s <- State.get
  let ks = currkeys s
  handleControls frametime evts
  return (SDLK_ESCAPE `elem` ks)

handleControls :: (Integral a) => a -> [SDL.Event] -> Match ()
handleControls frametime evts = do
  s <- State.get
  let ks = currkeys s
  when (keyWasPressed SDLK_p evts) $ do
    sModPaused not
  when (SDLK_UP `elem` ks) $ sModCampos (goUp 1)
  when (SDLK_DOWN `elem` ks) $ sModCampos (goUp (-1))
  when (SDLK_LEFT `elem` ks) $ sModCampos (goRight (-1))
  when (SDLK_RIGHT `elem` ks) $ sModCampos (goRight 1)
  when (not (paused s)) $ do
    case controlledpl s of
      Nothing -> return ()
      Just c  -> 
        case findPlayer c s of
          Nothing -> return ()
          Just p  -> do
            let xd = if (SDLK_d `elem` ks)
                       then 10
                       else if (SDLK_a `elem` ks)
                              then -10
                              else 0
                yd = if (SDLK_w `elem` ks)
                       then 10
                       else if (SDLK_s `elem` ks)
                              then -10
                              else 0
                tgt = (xd, yd) `add2` (plposition p)
            act p (Goto tgt)
            when ((xd, yd) /= (0, 0)) $ do
              when (SDLK_SPACE `elem` ks) $ do
                sModKickpower (+(fromIntegral frametime))
                -- act p (Kick (xd * 10, yd * 10, 30))
              when (SDLK_RETURN `elem` ks) $ do
                act p (Kick (xd * 4, yd * 4, 0))
              when (SDLK_SPACE `notElem` ks && (kickpower s > 0)) $ do
                act p (Kick (xd * fromIntegral (kickpower s) / 100, yd * fromIntegral (kickpower s) / 100, fromIntegral (kickpower s) / 1000 * 30))
                sModKickpower $ const 0

