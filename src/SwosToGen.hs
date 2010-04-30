module Main
where

import System.Environment
import System.Directory
import System.FilePath
import System.IO
import Control.Monad

import Data.Binary

import Swos
import SWOSShell

swostogen :: Bool -> FilePath -> FilePath -> IO ()
swostogen teams from to = do
  createDirectory to
  let rfunc = if teams then loadTeamsFromFile else decodeFile
  fs <- getDirectoryContents from
  forM_ fs $ \f -> do
    isfile <- doesFileExist (from </> f)
    if isfile
      then 
        if teams
          then do
            dt <- loadTeamsFromFile (from </> f)
            withFile (to </> f) WriteMode (\h -> hPutStrLn h (show (map swosTeamToGenTeam dt)))
          else do
            dt <- decodeFile (from </> f)
            withFile (to </> f) WriteMode (\h -> hPutStrLn h (show (swosTacticsToSimpleFormation dt)))
      else return ()

main = do
  args <- getArgs
  case length args of
    3 -> do
      let teams = head args == "0"
          fromdir = args !! 1
          todir = args !! 2
      fromexists <- doesDirectoryExist fromdir
      toexists <- doesDirectoryExist todir
      if fromexists && not toexists
        then swostogen teams fromdir todir
        else usage
    _ -> usage

usage = do
  n <- getProgName
  putStrLn $ n ++ " <mode> <fromdir> <todir>"
  putStrLn "Mode: 0 = teams, other = tactics"
  putStrLn "Converts a directory of SWOS tactics/teams to Freekick tactics/teams."
  putStrLn "Fromdir must exist. Todir mustn't exist."

