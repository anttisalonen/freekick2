module Main
where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Applicative
import Data.Char

getWord8Int :: Get Int
getWord8Int = fromIntegral <$> getWord8

putWord8Int :: Int -> Put
putWord8Int = putWord8 . fromIntegral

getName :: C.ByteString -> String
getName = correctName . C.unpack . C.takeWhile (/= '\0')

putName :: Int -> String -> Put
putName n s = putBytes n (C.pack s)

putBytes :: Int -> C.ByteString -> Put
putBytes n s =
  let s'  = C.take n s
      pad = C.replicate (n - C.length s') '\0'
  in putByteString $ s' `C.append` pad

capitalize :: String -> String
capitalize []     = []
capitalize (h:hs) = toUpper h : hs

deCaps :: String -> String
deCaps = map toLower

correctName :: String -> String
correctName = unwords . map capitalize . words . deCaps

caps :: String -> String
caps = map toUpper

packTwo :: Int -> Int -> Put
packTwo s1 s2 = 
  let a = fromIntegral s1
      b = fromIntegral s2
  in putWord8 (a `shiftL` 4 .|. (b .&. 15))

data SWOSTeamFile = SWOSTeamFile {
    numteams :: Int
  , teams    :: [SWOSTeam]
  }
  deriving (Show)

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
  deriving (Show)

data SWOSKit = SWOSKit {
    kittype         :: Int
  , kitfirstcolor   :: Int
  , kitsecondcolor  :: Int
  , kitshortcolor   :: Int
  , kitsockscolor   :: Int
  }
  deriving (Show)

data SWOSPlayer = SWOSPlayer {
    plnationality :: Int
  , plnumber      :: Int
  , plname        :: String
  , plposition    :: Int
  , plheadtype    :: Int
  , plunknown     :: Int
  , plmidbyte     :: Word8
  , plskills      :: SWOSSkills
  , plvalue       :: Int
  , plcareerbytes :: C.ByteString
  }
  deriving (Show)

data SWOSSkills = SWOSSkills {
    skpassing   :: Int
  , skshooting  :: Int
  , skheading   :: Int
  , sktackling  :: Int
  , skcontrol   :: Int
  , skspeed     :: Int
  , skfinishing :: Int
  }
  deriving (Show)

nullPlayer :: SWOSPlayer
nullPlayer = SWOSPlayer 0 0 "" 0 0 0 0 nullSkills 0 (C.replicate 5 '\0')

nullSkills :: SWOSSkills
nullSkills = SWOSSkills 0 0 0 0 0 0 0

instance Binary SWOSTeamFile where
  put s = do
    putWord8 0x00
    putWord8Int (numteams s)
    mapM_ put (take (numteams s) (teams s))
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
    putName 25 (caps $ teamcoachname s)
    putBytes 15 (teamplpos s)
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
    cn <- getBytes 25
    pp  <- getBytes 15
    pls <- replicateM 16 get
    return $ SWOSTeam b1 b2 b3 (getName tn) b5 b6 kit1 kit2 (getName cn) pp pls

instance Binary SWOSKit where
  put s = do
    putWord8Int (kittype s)
    putWord8Int (kitfirstcolor s)
    putWord8Int (kitsecondcolor s)
    putWord8Int (kitshortcolor s)
    putWord8Int (kitsockscolor s)
  get = do
    t1 <- getWord8Int
    t2 <- getWord8Int
    t3 <- getWord8Int
    t4 <- getWord8Int
    t5 <- getWord8Int
    return $ SWOSKit t1 t2 t3 t4 t5

instance Binary SWOSPlayer where
  put s = do
    putWord8Int (plnationality s)
    putWord8 0x00
    putWord8Int (plnumber s)
    putName 23 (caps $ plname s)
    let bn = (fromIntegral (plposition s) `shiftL` 5 .|. fromIntegral (plheadtype s) `shiftL` 3 .|. fromIntegral (plunknown s))
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
    return $ SWOSPlayer b0 b1 (getName ns) (b2 `shiftR` 5) (b2 `shiftR` 3 .&. 0x03) (b2 .&. 0x03) b3 sks b4 cr

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

