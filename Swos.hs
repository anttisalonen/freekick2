module Swos(SWOSTeamFile(..),
  SWOSTeam(..),
  SWOSPlayer(..),
  SWOSKit(..),
  SWOSSkills(..),
  loadTeamsFromFile,
  loadTeamsFromDirectory,
  showNation,
  showDivision)
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

data SWOSKit = SWOSKit {
    kittype         :: Int
  , kitfirstcolor   :: Int
  , kitsecondcolor  :: Int
  , kitshortcolor   :: Int
  , kitsockscolor   :: Int
  }
  deriving (Show, Read, Eq)

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
nullPlayer = SWOSPlayer 0 0 "" 0 0 0 0 nullSkills 0 (C.replicate 5 '\0')

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

showDivision :: Int -> String
showDivision 0 = "Premier"
showDivision 1 = "First"
showDivision 2 = "Second"
showDivision 3 = "Third"
showDivision 4 = "Non-league"
showDivision _ = "Unknown"

showNation :: Int -> String
showNation 0 = "Albania"
showNation 1 = "Austria"
showNation 2 = "Belgium"
showNation 3 = "Bulgaria"
showNation 4 = "Croatia"
showNation 5 = "Cyprus"
showNation 6 = "Czech Republic"
showNation 7 = "Denmark"
showNation 8 = "England"
showNation 9 = "Estonia"
showNation 10 = "Faroe Islands"
showNation 11 = "Finland"
showNation 12 = "France"
showNation 13 = "Germany"
showNation 14 = "Greece"
showNation 15 = "Hungary"
showNation 16 = "Iceland"
showNation 17 = "Israel"
showNation 18 = "Italy"
showNation 19 = "Latvia"
showNation 20 = "Lithuania"
showNation 21 = "Luxembourg"
showNation 22 = "Malta"
showNation 23 = "The Netherlands"
showNation 24 = "Northern Ireland"
showNation 25 = "Norway"
showNation 26 = "Poland"
showNation 27 = "Portugal"
showNation 28 = "Romania"
showNation 29 = "Russia"
showNation 30 = "San Marino"
showNation 31 = "Scotland"
showNation 32 = "Slovenia"
showNation 33 = "Sweden"
showNation 34 = "Turkey"
showNation 35 = "Ukraine"
showNation 36 = "Wales"
showNation 37 = "Serbia"
showNation 38 = "Belarus"
showNation 39 = "Slovakia"
showNation 40 = "Spain"
showNation 41 = "Armenia"
showNation 42 = "Bosnia-Herzegovina"
showNation 43 = "Azerbaijan"
showNation 44 = "Georgia"
showNation 45 = "Switzerland"
showNation 46 = "Ireland"
showNation 47 = "FYR Macedonia"
showNation 48 = "Turkmenistan"
showNation 49 = "Liechtenstein"
showNation 50 = "Moldova"
showNation 51 = "Costa Rica"
showNation 52 = "El Salvador"
showNation 53 = "Guatemala"
showNation 54 = "Honduras"
showNation 55 = "Bahamas"
showNation 56 = "Mexico"
showNation 57 = "Panama"
showNation 58 = "U.S.A."
showNation 59 = "Bahrain"
showNation 60 = "Nicaragua"
showNation 61 = "Bermuda"
showNation 62 = "Jamaica"
showNation 63 = "Trinidad and Tobago"
showNation 64 = "Canada"
showNation 65 = "Barbados"
showNation 66 = "El Salvador"
showNation 67 = "Saint Vincent and the Grenadines"
showNation 68 = "Argentina"
showNation 69 = "Bolivia"
showNation 70 = "Brazil"
showNation 71 = "Chile"
showNation 72 = "Colombia"
showNation 73 = "Ecuador"
showNation 74 = "Paraguay"
showNation 75 = "Surinam"
showNation 76 = "Uruguay"
showNation 77 = "Venezuela"
showNation 78 = "Guyana"
showNation 79 = "Peru"
showNation 80 = "Algeria"
showNation 81 = "South Africa"
showNation 82 = "Botswana"
showNation 83 = "Burkina Faso"
showNation 84 = "Burundi"
showNation 85 = "Lesotho"
showNation 86 = "Congo"
showNation 87 = "Zambia"
showNation 88 = "Ghana"
showNation 89 = "Senegal"
showNation 90 = "Ivory Coast"
showNation 91 = "Tunisia"
showNation 92 = "Mali"
showNation 93 = "Madagascar"
showNation 94 = "Cameroon"
showNation 95 = "Chad"
showNation 96 = "Uganda"
showNation 97 = "Liberia"
showNation 98 = "Mozambique"
showNation 99 = "Kenia"
showNation 100 = "Sudan"
showNation 101 = "Swaziland"
showNation 102 = "Angola"
showNation 103 = "Togo"
showNation 104 = "Zimbabwe"
showNation 105 = "Egypt"
showNation 106 = "Tanzania"
showNation 107 = "Nigeria"
showNation 108 = "Ethiopia"
showNation 109 = "Gabon"
showNation 110 = "Sierra Leone"
showNation 111 = "Benin"
showNation 112 = "Congo"
showNation 113 = "Guinea"
showNation 114 = "Sri Lanka"
showNation 115 = "Morocco"
showNation 116 = "Gambia"
showNation 117 = "Malawi"
showNation 118 = "Japan"
showNation 119 = "Taiwan"
showNation 120 = "India"
showNation 121 = "Bangladesh"
showNation 122 = "Brunei"
showNation 123 = "Iraq"
showNation 124 = "Jordan"
showNation 125 = "Sri Lanka"
showNation 126 = "Syria"
showNation 127 = "South Korea"
showNation 128 = "Iran"
showNation 129 = "Vietnam"
showNation 130 = "Malaysia"
showNation 131 = "Saudi Arabia"
showNation 132 = "Yemen"
showNation 133 = "Kuwait"
showNation 134 = "Laos"
showNation 135 = "North Korea"
showNation 136 = "Oman"
showNation 137 = "Pakistan"
showNation 138 = "Philippines"
showNation 139 = "China"
showNation 140 = "Singapore"
showNation 141 = "Mauritius"
showNation 142 = "Burma"
showNation 143 = "Papua New Guinea"
showNation 144 = "Thailand"
showNation 145 = "Uzbekistan"
showNation 146 = "Qatar"
showNation 147 = "United Arab Emirates"
showNation 148 = "Australia"
showNation 149 = "New Zealand"
showNation 150 = "Fiji"
showNation 151 = "Solomon Islands"
showNation _   = "Unknown"

