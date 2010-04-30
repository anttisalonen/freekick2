{-# LANGUAGE TemplateHaskell #-}
module Gen
where

import Control.Monad.State
import Data.Word

import FVector
import DeriveMod

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

