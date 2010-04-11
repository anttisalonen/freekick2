module Main
where

import System.Environment

import Data.Binary

import Swos

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: progname <inputfile> <outputfile>"
    else do
      let infile = args !! 0
          outfile = args !! 1
      n <- decodeFile infile :: IO SWOSTeamFile
      encodeFile outfile n

