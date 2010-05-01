module Main
where

import System.Random
import System.Environment
import Control.Monad
import Data.Maybe

import Data.Binary

import Utils
import Gen

usage = do
  n <- getProgName
  putStrLn $ "Usage: " ++ n ++ " <teamname> <teamnation> <teamlevel> <first-names-file> <surnames-file> <random seed> <out-file>"
  putStrLn "Team nation is an integer (see showTeamNation in Listings.hs.)"
  putStrLn "Team level must be between 1 and 10."

main = do
  args <- getArgs
  if length args < 7
    then usage
    else do
      let mn = safeRead (args !! 1)
          ml = safeRead (args !! 2)
          ms = safeRead (args !! 5)
      if isNothing ml || isNothing mn || isNothing ms
        then usage
        else do
          let l = fromJust ml
              n = fromJust mn
              s = fromJust ms
          fnames <- lines `fmap` readFile (args !! 3)
          snames <- lines `fmap` readFile (args !! 4)
          create (head args) n l s fnames snames (args !! 6)

create :: FilePath -> Int -> Int -> Int -> [String] -> [String] -> FilePath -> IO ()
create tname tnation tlevel s firsts surs outfile = do
  setStdGen (mkStdGen s)
  d <- randomRIO (3, 5)
  m <- case d of
         3 -> return 5
         5 -> randomRIO (3, 4)
         _ -> randomRIO (2, 5)
  let f = 10 - d - m
  tkitcol1 <- chooseIO allColors
  tkitcol2 <- chooseIO allColors
  tkitcol3 <- chooseIO allColors
  tkitcol4 <- chooseIO allColors
  let tkit = Kit 0 tkitcol1 tkitcol2 tkitcol3 tkitcol4
  tpls <- createPlayers tlevel firsts surs
  encodeFile outfile [GenTeam tnation tname (d, m, f) 0 tkit tpls]

createPlayers :: Int -> [String] -> [String] -> IO [GenPlayer]
createPlayers tlevel firsts surs = do
  forM [1..16] $ \i -> do
    fn <- chooseIO firsts
    sn <- chooseIO surs
    createPlayer fn sn i tlevel

createPlayer :: String -> String -> Int -> Int -> IO GenPlayer
createPlayer fn sn pnum tl = do
  pos <- mkPos pnum
  sks <- mkSkills tl
  return (GenPlayer pnum (fn ++ " " ++ sn) pos sks)

mkSkills :: Int -> IO PlayerSkills
mkSkills tl = do
  var1 <- randomRIO (-0.4, 0.4)
  var2 <- randomRIO (-0.4, 0.4)
  var3 <- randomRIO (-0.4, 0.4)
  var4 <- randomRIO (-0.4, 0.4)
  let std = fromIntegral tl * 0.1
  let s1 = clamp 0 1 $ std + var1
  let s2 = clamp 0 1 $ std + var2
  let s3 = clamp 0 1 $ std + var3
  let s4 = clamp 0 1 $ std + var4
  return $ PlayerSkills s1 s2 s3 s4

mkPos 1  = return Goalkeeper -- make sure everyone has enough different types of players to fill the formations
mkPos 2  = return Defender
mkPos 3  = return Defender
mkPos 4  = return Defender
mkPos 5  = return Defender
mkPos 6  = return Defender
mkPos 7  = return Midfielder
mkPos 8  = return Midfielder
mkPos 9  = return Midfielder
mkPos 10 = return Midfielder
mkPos 11 = return Midfielder
mkPos 12 = return Goalkeeper
mkPos 13 = return Attacker
mkPos 14 = return Attacker
mkPos 15 = return Attacker
mkPos 16 = return Attacker
mkPos _  = do
  n <- randomRIO (1, 3 :: Int)
  case n of
    1 -> return Attacker
    2 -> return Midfielder
    _ -> return Defender

