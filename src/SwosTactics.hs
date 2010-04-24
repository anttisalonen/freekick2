module SwosTactics
where

import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Control.Monad
import Control.Applicative
import Data.Char
import System.Directory
import System.FilePath

import BinHelpers

data SWOSTactics = SWOSTactics {
    tacticname :: String
  , positions  :: [[Int]]
  }
  deriving (Show)

putPlPos = putWord8Int

getPlPos = fromIntegral <$> getWord8

putPlTac = mapM_ putPlPos

getPlTac =
  replicateM 35 getPlPos

instance Binary SWOSTactics where
  put s = do
    putName 9 (caps $ tacticname s)
    mapM_ putPlTac (positions s)
    mapM_ putWord8 [0x00, 0xFF, 0xFF, 0x00, 0x01, 0xFF, 0xFF, 0x01, 0xFF, 0xFF, 0x00]
  get = do
    n <- getBytes 9
    ms <- replicateM 10 getPlTac
    return $ SWOSTactics (getName n) ms

loadTacticsFromDirectory :: FilePath -> IO [SWOSTactics]
loadTacticsFromDirectory fp = do
  fs <- getDirectoryContents fp
  tss <- forM fs $ \f -> do
    isfile <- doesFileExist (fp </> f)
    if isfile
      then do
        t <- decodeFile (fp </> f)
        return $ Just t
      else return Nothing
  return $ catMaybes tss

organizeTacticsByName :: SWOSTactics -> ((Int, Int, Int), SWOSTactics)
organizeTacticsByName t =
  if length (tacticname t) > 2
    then ((d, m, f), t)
    else ((4, 4, 2), t)
   where d = ord (tacticname t !! 0) - 48
         m = ord (tacticname t !! 1) - 48
         f = ord (tacticname t !! 2) - 48

