module Swos(SWOSTeamFile(..),
  SWOSTeam(..),
  SWOSPlayer(..),
  SWOSKit(..),
  SWOSSkills(..),
  SWOSColor(..),
  SWOSPosition(..),
  numPositions,
  isGoalkeeper,
  isDefender,
  isMidfielder,
  isAttacker,
  loadTeamsFromFile,
  loadTeamsFromDirectory
  )
where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath

import BinHelpers

packTwo :: Int -> Int -> Put
packTwo s1 s2 = 
  let a = fromIntegral s1
      b = fromIntegral s2
  in putWord8 (a `shiftL` 4 .|. (b .&. 15))

data SWOSTeamFile = SWOSTeamFile {
    numteams  :: Int
  , fileteams :: [SWOSTeam]
  }
  deriving (Show, Read, Eq)

data SWOSTeam = SWOSTeam {
    teamnation     :: Int
  , teamnumber     :: Int
  , swosteamnumber :: Int
  , teamname       :: String
  , teamtactics    :: Int
  , teamdivision   :: Int
  , primarykit     :: SWOSKit
  , secondarykit   :: SWOSKit
  , teamcoachname  :: String
  , teamplpos      :: C.ByteString
  , teamplayers    :: [SWOSPlayer]
  }
  deriving (Show, Read, Eq)

data SWOSColor = Grey | White | Black | Orange | Red | Blue | Brown | LightBlue | Green | Yellow
  deriving (Show, Read, Eq, Enum)

numPositions :: Int -> (Int, Int, Int)
numPositions 0 = (4, 4, 2)
numPositions 1 = (5, 4, 1)
numPositions 2 = (4, 5, 1)
numPositions 3 = (5, 3, 2)
numPositions 4 = (3, 5, 2)
numPositions 5 = (4, 3, 3)
numPositions 6 = (4, 2, 4)
numPositions 7 = (3, 4, 3)
numPositions 8 = (4, 4, 2)
numPositions 9 = (5, 2, 3)
numPositions 10 = (3, 2, 5)
numPositions 11 = (6, 3, 1)
numPositions _ = (4, 4, 2)

data SWOSKit = SWOSKit {
    kittype         :: Int
  , kitfirstcolor   :: SWOSColor
  , kitsecondcolor  :: SWOSColor
  , kitshortcolor   :: SWOSColor
  , kitsockscolor   :: SWOSColor
  }
  deriving (Show, Read, Eq)

data SWOSPosition = Goalkeeper | RightBack | LeftBack | Defender | RightWing | LeftWing | Midfielder | Attacker
  deriving (Show, Read, Eq, Enum)

isGoalkeeper :: SWOSPosition -> Bool
isGoalkeeper n = n == Goalkeeper

isDefender :: SWOSPosition -> Bool
isDefender RightBack = True
isDefender LeftBack  = True
isDefender Defender  = True
isDefender _         = False

isMidfielder :: SWOSPosition -> Bool
isMidfielder RightWing  = True
isMidfielder LeftWing   = True
isMidfielder Midfielder = True
isMidfielder _          = False

isAttacker :: SWOSPosition -> Bool
isAttacker n = n == Attacker

data SWOSPlayer = SWOSPlayer {
    plnationality :: Int
  , plnumber      :: Int
  , plname        :: String
  , plposition    :: SWOSPosition
  , plheadtype    :: Int
  , plunknown     :: Int
  , plmidbyte     :: Word8
  , plskills      :: SWOSSkills
  , plvalue       :: Int
  , plcareerbytes :: C.ByteString
  }
  deriving (Show, Read, Eq)

data SWOSSkills = SWOSSkills {
    skpassing   :: Int
  , skshooting  :: Int
  , skheading   :: Int
  , sktackling  :: Int
  , skcontrol   :: Int
  , skspeed     :: Int
  , skfinishing :: Int
  }
  deriving (Show, Read, Eq)

nullPlayer :: SWOSPlayer
nullPlayer = SWOSPlayer 0 0 "" Goalkeeper 0 0 0 nullSkills 0 (C.replicate 5 '\0')

nullSkills :: SWOSSkills
nullSkills = SWOSSkills 0 0 0 0 0 0 0

instance Binary SWOSTeamFile where
  put s = do
    putWord8 0x00
    putWord8Int (numteams s)
    mapM_ put (take (numteams s) (fileteams s))
  get = do
    _ <- get :: Get Word8
    n <- fromIntegral <$> getWord8
    ts <- replicateM n get
    return $ SWOSTeamFile n ts

instance Binary SWOSTeam where
  put s = do
    putWord8Int (teamnation s)
    putWord8Int (teamnumber s)
    putWord16le $ fromIntegral (swosteamnumber s)
    putWord8 0x00
    putName 19 (caps $ teamname s)
    putWord8Int (teamtactics s)
    putWord8Int (teamdivision s)
    put (primarykit s)
    put (secondarykit s)
    putName 24 (caps $ teamcoachname s)
    putBytes 16 (teamplpos s)
    mapM_ put (take 16 $ teamplayers s ++ repeat nullPlayer)
  get = do
    b1 <- getWord8Int
    b2 <- getWord8Int
    b3 <- fromIntegral <$> getWord16le
    _  <- get :: Get Word8
    tn <- getBytes 19
    b5 <- getWord8Int
    b6 <- getWord8Int
    kit1 <- get
    kit2 <- get
    cn <- getBytes 24
    pp  <- getBytes 16
    pls <- replicateM 16 get
    return $ SWOSTeam b1 b2 b3 (getName tn) b5 b6 kit1 kit2 (getName cn) pp pls

instance Binary SWOSKit where
  put s = do
    putWord8Int (kittype s)
    putWord8Int (fromEnum $ kitfirstcolor s)
    putWord8Int (fromEnum $ kitsecondcolor s)
    putWord8Int (fromEnum $ kitshortcolor s)
    putWord8Int (fromEnum $ kitsockscolor s)
  get = do
    t1 <- getWord8Int
    t2 <- toEnum <$> getWord8Int
    t3 <- toEnum <$> getWord8Int
    t4 <- toEnum <$> getWord8Int
    t5 <- toEnum <$> getWord8Int
    return $ SWOSKit t1 t2 t3 t4 t5

instance Binary SWOSPlayer where
  put s = do
    putWord8Int (plnationality s)
    putWord8 0x00
    putWord8Int (plnumber s)
    putName 23 (caps $ plname s)
    let bn = (fromIntegral (fromEnum $ plposition s) `shiftL` 5 .|. fromIntegral (plheadtype s) `shiftL` 3 .|. fromIntegral (plunknown s))
    putWord8 bn
    putWord8 (plmidbyte s)
    put (plskills s)
    putWord8Int (plvalue s)
    putBytes 5 (plcareerbytes s)
  get = do
    b0 <- getWord8Int
    _  <- getWord8Int
    b1 <- getWord8Int
    ns <- getBytes 23
    b2 <- getWord8Int
    b3 <- getWord8
    sks <- get
    b4 <- getWord8Int
    cr <- getBytes 5
    return $ SWOSPlayer b0 b1 (getName ns) (toEnum $ b2 `shiftR` 5) (b2 `shiftR` 3 .&. 0x03) (b2 .&. 0x03) b3 sks b4 cr

instance Binary SWOSSkills where
  put s = do
    packTwo 0x00 (skpassing s)
    packTwo (skshooting s) (skheading s)
    packTwo (sktackling s) (skcontrol s)
    packTwo (skspeed s) (skfinishing s)
  get = do
    b1 <- getWord8Int
    b2 <- getWord8Int
    b3 <- getWord8Int
    b4 <- getWord8Int
    let p = b1 .&. 15
        v = b2 `shiftR` 4
        h = b2 .&. 15
        t = b3 `shiftR` 4
        c = b3 .&. 15
        s = b4 `shiftR` 4
        f = b4 .&. 15
    return $ SWOSSkills p v h t c s f

loadTeamsFromFile :: FilePath -> IO [SWOSTeam]
loadTeamsFromFile fp = liftM fileteams (decodeFile fp)

loadTeamsFromDirectory :: FilePath -> IO [SWOSTeam]
loadTeamsFromDirectory fp = do
  fs <- getDirectoryContents fp
  tss <- forM fs $ \f -> do
    isfile <- doesFileExist (fp </> f)
    if isfile
      then loadTeamsFromFile (fp </> f)
      else return []
  return $ concat tss


