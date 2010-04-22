module Match.State.Controls(handleInput)
where

import Control.Monad
import Control.Monad.State as State
import Data.Maybe

import Graphics.UI.SDL as SDL

import FVector
import SDLUtils

import Match.Player

import Match.State.MatchBase
import Match.State.MatchState
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
handleInput dt = do
  evts <- liftIO $ pollAllSDLEvents
  sModCurrkeys $ updateKeyMap (keyChanges evts)
  s <- State.get
  let ks = currkeys s
  mquits <- handleControls dt evts
  return (mquits || (SDLK_ESCAPE `elem` ks))

handleControls :: (Integral a) => a -> [SDL.Event] -> Match Bool
handleControls dt evts = do
  s <- State.get
  let ks = currkeys s
  when (keyWasPressed SDLK_p evts) $ do
    sModPaused not
  when (not (paused s)) $ do
    case controlledpl s of
      Nothing -> return ()
      Just c  -> 
        case findPlayer c s of
          Nothing -> return ()
          Just p  -> do
            let xd = if (SDLK_RIGHT `elem` ks)
                       then 10
                       else if (SDLK_LEFT `elem` ks)
                              then -10
                              else 0
                yd = if (SDLK_UP `elem` ks)
                       then 10
                       else if (SDLK_DOWN `elem` ks)
                              then -10
                              else 0
                tgt = (xd, yd) `add2` (plposition p)
            act p (Goto tgt)
            when ((xd, yd) /= (0, 0)) $ do
              when (SDLK_RCTRL `elem` ks) $ do
                sModKickpower (+(fromIntegral dt))
              when ((SDLK_RCTRL `notElem` ks && (kickpower s > 0)) || (kickpower s > 1000)) $ do
                if kickpower s < 100
                  then act p (Kick (xd * 4,
                                    yd * 4,
                                    0))
                  else act p (Kick (xd * (200 + fromIntegral (kickpower s)) / 100, 
                                    yd * (200 + fromIntegral (kickpower s)) / 100, 
                                   (200 + fromIntegral (kickpower s)) / 1000 * 30))
                sModKickpower $ const 0
  return (fromMaybe False (liftM (>2000) (finishedSince (ballplay s))) && SDLK_RCTRL `elem` ks)

