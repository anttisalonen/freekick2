module SwosTactics
where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Applicative
import Data.Char
import System.Directory
import System.FilePath

import BinHelpers

data SwosTactics = SwosTactics {
    tacticname :: String
  , positions  :: [[Int]]
  }
  deriving (Show)

putPlPos = putWord8Int

getPlPos = fromIntegral <$> getWord8

putPlTac = mapM_ putPlPos

getPlTac =
  replicateM 35 getPlPos

instance Binary SwosTactics where
  put s = do
    putName 9 (caps $ tacticname s)
    mapM_ putPlTac (positions s)
    mapM_ putWord8 [0x00, 0xFF, 0xFF, 0x00, 0x01, 0xFF, 0xFF, 0x01, 0xFF, 0xFF, 0x00]
  get = do
    n <- getBytes 9
    ms <- replicateM 10 getPlTac
    return $ SwosTactics (getName n) ms

