{-# LANGUAGE TemplateHaskell #-}
module Gen
where

import Control.Monad.State
import Data.Word

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

data PlPosition = Goalkeeper | Defender | Midfielder | Attacker
  deriving (Eq, Show, Read)

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

type Color = (Word8, Word8, Word8)

data GenFormation = GenFormation {
    tactics     :: [Tactic]
  , playerorder :: (Int, Int, Int)
  }

plPosToTactic :: [Int] -> Gen.Tactic
plPosToTactic ps = \b -> plpoint (ballrectangle b) ps

ballrectangle :: (Float, Float) -> Int
ballrectangle (bx, by) = v
  where v = y * 5 + x
        x = clamp 0 4 $ floor $ (1 - bx) * 5
        y = clamp 0 6 $ floor $ by * 7

plpoint :: Int -> [Int] -> FRange
plpoint br ts = 
  let pn = ts !! (min 34 br)
      xp = pn `mod` 15
      yp = pn `div` 16
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

data Kit = Kit {
    kittype         :: Int
  , kitfirstcolor   :: Color
  , kitsecondcolor  :: Color
  , kitshortcolor   :: Color
  , kitsockscolor   :: Color
  }
  deriving (Show, Read)

data GenTeam = GenTeam {
    teamnation     :: Int
  , genteamname    :: String
  , teamtactics    :: (Int, Int, Int)
  , teamdivision   :: Int
  , primarykit     :: Kit
  , teamplayers    :: [GenPlayer]
  }
  deriving (Show, Read)

