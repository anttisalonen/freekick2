{-# LANGUAGE TemplateHaskell #-}
module Gen
where

import Control.Monad.State(modify)
import Data.Word
import Data.Bits

import Data.Binary

import FVector
import DeriveMod
import Utils

type Skill = Float

type Tactic = FRange -> FRange

data PlayerSkills = PlayerSkills {
    shootingskill :: Skill
  , passingskill  :: Skill
  , speedskill    :: Skill
  , controlskill  :: Skill
  }
  deriving (Show, Read)
$(deriveMods ''PlayerSkills)

instance Binary Gen.PlayerSkills where
  put (PlayerSkills a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (PlayerSkills a b c d)

data PlPosition = Goalkeeper | Defender | Midfielder | Attacker
  deriving (Eq, Show, Read)

instance Binary Gen.PlPosition where
  put Goalkeeper = putWord8 0
  put Defender = putWord8 1
  put Midfielder = putWord8 2
  put Attacker = putWord8 3
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Goalkeeper
      1 -> return Defender
      2 -> return Midfielder
      3 -> return Attacker
      _ -> fail "no parse"

isGoalkeeper p = genplpos p == Goalkeeper
isDefender p = genplpos p == Defender
isMidfielder p = genplpos p == Midfielder
isAttacker p = genplpos p == Attacker

data GenPlayer = GenPlayer {
    plnumber      :: Int
  , plname        :: String
  , genplpos      :: PlPosition
  , plskills      :: PlayerSkills
  }
  deriving (Show, Read)

instance Binary Gen.GenPlayer where
  put (GenPlayer a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (GenPlayer a b c d)

type Color = (Word8, Word8, Word8)

data GenFormation = GenFormation {
    tactics     :: [Tactic]
  , playerorder :: (Int, Int, Int)
  }

plPosToTactic :: [Int] -> Gen.Tactic
plPosToTactic ps = \b -> plpoint (ballrectangle b) ps

ballrectangle :: (Float, Float) -> Int
ballrectangle (bx, by) = y * 5 + x
  where x = clamp 0 4 $ floor $ (1 - bx) * 5
        y = clamp 0 6 $ floor $ by * 7

plpoint :: Int -> [Int] -> FRange
plpoint br ts = 
  let pn = ts !! (min 34 br)
      xp = pn `shiftR` 4
      yp = pn .&. 0x0F
      x = 1 - fromIntegral xp * (1/15)
      y = fromIntegral yp * (1/16)
  in (x, y)

simpleFormationToGenFormation :: SimpleFormation -> GenFormation
simpleFormationToGenFormation st =
  let ds = simpleorder st
      ts = map plPosToTactic (simpletactics st)
  in GenFormation ts ds

data SimpleFormation = SimpleFormation {
    simpletactics  :: [[Int]]
  , simpleorder    :: (Int, Int, Int)
  }
  deriving (Show, Read)

instance Binary Gen.SimpleFormation where
  put (SimpleFormation a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (SimpleFormation a b)

data Kit = Kit {
    kittype         :: Int
  , kitfirstcolor   :: Color
  , kitsecondcolor  :: Color
  , kitshortcolor   :: Color
  , kitsockscolor   :: Color
  }
  deriving (Show, Read)

instance Binary Gen.Kit where
  put (Kit a b c d e) = put a >> put b >> put c >> put d >> put e
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Kit a b c d e)

data GenTeam = GenTeam {
    teamnation     :: Int
  , genteamname    :: String
  , teamtactics    :: (Int, Int, Int)
  , teamdivision   :: Int
  , primarykit     :: Kit
  , teamplayers    :: [GenPlayer]
  }
  deriving (Show, Read)

instance Binary Gen.GenTeam where
  put (GenTeam a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> return (GenTeam a b c d e f)

grey, white, black, orange, red, blue, brown, lightblue,
  green, yellow :: Color
grey = (128, 128, 128)
white = (255, 255, 255)
black = (0, 0, 0)
orange = (255, 127, 0)
red = (255, 0, 0)
blue = (0, 0, 255)
brown = (150, 75, 0)
lightblue = (173, 216, 230)
green = (0, 255, 0)
yellow = (255, 255, 0)

allColors = [grey, white, black, orange, red, blue, brown, lightblue, green, yellow]


