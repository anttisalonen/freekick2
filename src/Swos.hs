module Swos(SWOSTeamFile(..),
  SWOSTeam(..),
  SWOSPlayer(..),
  SWOSKit(..),
  SWOSSkills(..),
  loadTeamsFromFile,
  loadTeamsFromDirectory,
  showPlayerNation,
  showTeamNation,
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
showDivision 0 = "Premier league"
showDivision 1 = "First league"
showDivision 2 = "Second league"
showDivision 3 = "Third league"
showDivision 4 = "Non-league"
showDivision _ = "Unknown"

showPlayerNation :: Int -> String
showPlayerNation 0 = "Albania"
showPlayerNation 1 = "Austria"
showPlayerNation 2 = "Belgium"
showPlayerNation 3 = "Bulgaria"
showPlayerNation 4 = "Croatia"
showPlayerNation 5 = "Cyprus"
showPlayerNation 6 = "Czech Republic"
showPlayerNation 7 = "Denmark"
showPlayerNation 8 = "England"
showPlayerNation 9 = "Estonia"
showPlayerNation 10 = "Faroe Islands"
showPlayerNation 11 = "Finland"
showPlayerNation 12 = "France"
showPlayerNation 13 = "Germany"
showPlayerNation 14 = "Greece"
showPlayerNation 15 = "Hungary"
showPlayerNation 16 = "Iceland"
showPlayerNation 17 = "Israel"
showPlayerNation 18 = "Italy"
showPlayerNation 19 = "Latvia"
showPlayerNation 20 = "Lithuania"
showPlayerNation 21 = "Luxembourg"
showPlayerNation 22 = "Malta"
showPlayerNation 23 = "The Netherlands"
showPlayerNation 24 = "Northern Ireland"
showPlayerNation 25 = "Norway"
showPlayerNation 26 = "Poland"
showPlayerNation 27 = "Portugal"
showPlayerNation 28 = "Romania"
showPlayerNation 29 = "Russia"
showPlayerNation 30 = "San Marino"
showPlayerNation 31 = "Scotland"
showPlayerNation 32 = "Slovenia"
showPlayerNation 33 = "Sweden"
showPlayerNation 34 = "Turkey"
showPlayerNation 35 = "Ukraine"
showPlayerNation 36 = "Wales"
showPlayerNation 37 = "Serbia"
showPlayerNation 38 = "Belarus"
showPlayerNation 39 = "Slovakia"
showPlayerNation 40 = "Spain"
showPlayerNation 41 = "Armenia"
showPlayerNation 42 = "Bosnia-Herzegovina"
showPlayerNation 43 = "Azerbaijan"
showPlayerNation 44 = "Georgia"
showPlayerNation 45 = "Switzerland"
showPlayerNation 46 = "Ireland"
showPlayerNation 47 = "FYR Macedonia"
showPlayerNation 48 = "Turkmenistan"
showPlayerNation 49 = "Liechtenstein"
showPlayerNation 50 = "Moldova"
showPlayerNation 51 = "Costa Rica"
showPlayerNation 52 = "El Salvador"
showPlayerNation 53 = "Guatemala"
showPlayerNation 54 = "Honduras"
showPlayerNation 55 = "Bahamas"
showPlayerNation 56 = "Mexico"
showPlayerNation 57 = "Panama"
showPlayerNation 58 = "U.S.A."
showPlayerNation 59 = "Bahrain"
showPlayerNation 60 = "Nicaragua"
showPlayerNation 61 = "Bermuda"
showPlayerNation 62 = "Jamaica"
showPlayerNation 63 = "Trinidad and Tobago"
showPlayerNation 64 = "Canada"
showPlayerNation 65 = "Barbados"
showPlayerNation 66 = "El Salvador"
showPlayerNation 67 = "Saint Vincent and the Grenadines"
showPlayerNation 68 = "Argentina"
showPlayerNation 69 = "Bolivia"
showPlayerNation 70 = "Brazil"
showPlayerNation 71 = "Chile"
showPlayerNation 72 = "Colombia"
showPlayerNation 73 = "Ecuador"
showPlayerNation 74 = "Paraguay"
showPlayerNation 75 = "Surinam"
showPlayerNation 76 = "Uruguay"
showPlayerNation 77 = "Venezuela"
showPlayerNation 78 = "Guyana"
showPlayerNation 79 = "Peru"
showPlayerNation 80 = "Algeria"
showPlayerNation 81 = "South Africa"
showPlayerNation 82 = "Botswana"
showPlayerNation 83 = "Burkina Faso"
showPlayerNation 84 = "Burundi"
showPlayerNation 85 = "Lesotho"
showPlayerNation 86 = "Congo"
showPlayerNation 87 = "Zambia"
showPlayerNation 88 = "Ghana"
showPlayerNation 89 = "Senegal"
showPlayerNation 90 = "Ivory Coast"
showPlayerNation 91 = "Tunisia"
showPlayerNation 92 = "Mali"
showPlayerNation 93 = "Madagascar"
showPlayerNation 94 = "Cameroon"
showPlayerNation 95 = "Chad"
showPlayerNation 96 = "Uganda"
showPlayerNation 97 = "Liberia"
showPlayerNation 98 = "Mozambique"
showPlayerNation 99 = "Kenia"
showPlayerNation 100 = "Sudan"
showPlayerNation 101 = "Swaziland"
showPlayerNation 102 = "Angola"
showPlayerNation 103 = "Togo"
showPlayerNation 104 = "Zimbabwe"
showPlayerNation 105 = "Egypt"
showPlayerNation 106 = "Tanzania"
showPlayerNation 107 = "Nigeria"
showPlayerNation 108 = "Ethiopia"
showPlayerNation 109 = "Gabon"
showPlayerNation 110 = "Sierra Leone"
showPlayerNation 111 = "Benin"
showPlayerNation 112 = "Congo"
showPlayerNation 113 = "Guinea"
showPlayerNation 114 = "Sri Lanka"
showPlayerNation 115 = "Morocco"
showPlayerNation 116 = "Gambia"
showPlayerNation 117 = "Malawi"
showPlayerNation 118 = "Japan"
showPlayerNation 119 = "Taiwan"
showPlayerNation 120 = "India"
showPlayerNation 121 = "Bangladesh"
showPlayerNation 122 = "Brunei"
showPlayerNation 123 = "Iraq"
showPlayerNation 124 = "Jordan"
showPlayerNation 125 = "Sri Lanka"
showPlayerNation 126 = "Syria"
showPlayerNation 127 = "South Korea"
showPlayerNation 128 = "Iran"
showPlayerNation 129 = "Vietnam"
showPlayerNation 130 = "Malaysia"
showPlayerNation 131 = "Saudi Arabia"
showPlayerNation 132 = "Yemen"
showPlayerNation 133 = "Kuwait"
showPlayerNation 134 = "Laos"
showPlayerNation 135 = "North Korea"
showPlayerNation 136 = "Oman"
showPlayerNation 137 = "Pakistan"
showPlayerNation 138 = "Philippines"
showPlayerNation 139 = "China"
showPlayerNation 140 = "Singapore"
showPlayerNation 141 = "Mauritius"
showPlayerNation 142 = "Burma"
showPlayerNation 143 = "Papua New Guinea"
showPlayerNation 144 = "Thailand"
showPlayerNation 145 = "Uzbekistan"
showPlayerNation 146 = "Qatar"
showPlayerNation 147 = "United Arab Emirates"
showPlayerNation 148 = "Australia"
showPlayerNation 149 = "New Zealand"
showPlayerNation 150 = "Fiji"
showPlayerNation 151 = "Solomon Islands"
showPlayerNation _   = "Unknown"

showTeamNation :: Int -> String
showTeamNation 0 = "Albania"
showTeamNation 1 = "Austria"
showTeamNation 2 = "Belgium"
showTeamNation 3 = "Bulgaria"
showTeamNation 4 = "Croatia"
showTeamNation 5 = "Cyprus"
showTeamNation 6 = "Czech Republic"
showTeamNation 7 = "Denmark"
showTeamNation 8 = "England"
showTeamNation 9 = "9 - Unknown"
showTeamNation 10 = "Estonia"
showTeamNation 11 = "Faroe Islands"
showTeamNation 12 = "Finland"
showTeamNation 13 = "France"
showTeamNation 14 = "Germany"
showTeamNation 15 = "Greece"
showTeamNation 16 = "Hungary"
showTeamNation 17 = "Iceland"
showTeamNation 18 = "Ireland"
showTeamNation 19 = "Israel"
showTeamNation 20 = "Italy"
showTeamNation 21 = "Latvia"
showTeamNation 22 = "Lithuania"
showTeamNation 23 = "Luxembourg"
showTeamNation 24 = "Malta"
showTeamNation 25 = "The Netherlands"
showTeamNation 26 = "Northern Ireland"
showTeamNation 27 = "Norway"
showTeamNation 28 = "Poland"
showTeamNation 29 = "Portugal"
showTeamNation 30 = "Romania"
showTeamNation 31 = "Russia"
showTeamNation 32 = "San Marino"
showTeamNation 33 = "Scotland"
showTeamNation 34 = "Slovenia"
showTeamNation 35 = "Spain"
showTeamNation 36 = "Sweden"
showTeamNation 37 = "Switzerland"
showTeamNation 38 = "Turkey"
showTeamNation 39 = "Ukraine"
showTeamNation 40 = "Wales"
showTeamNation 41 = "Yugoslavia"
showTeamNation 42 = "Algeria"
showTeamNation 43 = "Argentina"
showTeamNation 44 = "Australia"
showTeamNation 45 = "Bolivia"
showTeamNation 46 = "Brazil"
showTeamNation 47 = "47 - Unknown"
showTeamNation 48 = "Chile"
showTeamNation 49 = "Colombia"
showTeamNation 50 = "Ecuador"
showTeamNation 51 = "El Salvador"
showTeamNation 52 = "52 - Unknown"
showTeamNation 53 = "53 - Unknown"
showTeamNation 54 = "54 - Unknown"
showTeamNation 55 = "Japan"
showTeamNation 60 = "Mexico"
showTeamNation 62 = "New Zealand"
showTeamNation 64 = "Paraguay"
showTeamNation 65 = "Peru"
showTeamNation 66 = "Surinam"
showTeamNation 67 = "Taiwan"
showTeamNation 69 = "South Africa"
showTeamNation 71 = "Uruguay"
showTeamNation 73 = "U.S.A."
showTeamNation 75 = "India"
showTeamNation 76 = "Belarus"
showTeamNation 77 = "Venezuela"
showTeamNation 78 = "Slovakia"
showTeamNation 79 = "Ghana"
showTeamNation 80 = "European national"
showTeamNation 81 = "African national"
showTeamNation 82 = "South American national"
showTeamNation 83 = "North American national"
showTeamNation 84 = "Asian national"
showTeamNation 85 = "Oceanian national"
showTeamNation n = show n
