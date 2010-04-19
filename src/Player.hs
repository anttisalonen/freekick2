{-# LANGUAGE TemplateHaskell #-}
module Player
where

import Control.Monad.State

import Drawing
import FVector
import DeriveMod

data PlPosition = Goalkeeper | Defender | Midfielder | Attacker
  deriving (Eq)

type PlayerID = (Int, Bool)

type Skill = Float

data PlayerSkills = PlayerSkills {
    shootingskill :: Skill
  , passingskill  :: Skill
  , speedskill    :: Skill
  }
$(deriveMods ''PlayerSkills)

data Player = Player {
    plposition  :: FRange
  , plimage     :: ImageInfo
  , plshadow    :: ImageInfo
  , plposz      :: Float
  , playerid    :: PlayerID
  , plpos       :: PlPosition
  , kicktimer   :: Int
  , plskills    :: PlayerSkills
  }
$(deriveMods ''Player)

instance Eq Player where
  p1 == p2 = playerid p1 == playerid p2

instance Sprite Player where
  getTexture   = imgtexture . plimage
  getRectangle = playerTexRectangle
  getDepth     = playerHeight

playerHome :: Player -> Bool
playerHome = snd . playerid

playerNumber :: Player -> Int
playerNumber = fst . playerid

playerTexRectangle :: Player -> Rectangle
playerTexRectangle p =
  ((a - c / 2, b), (c, d))
    where (a, b) = plposition p
          (c, d) = imgsize $ plimage p

playerHeight :: Player -> Float
playerHeight p = topDownDepth (plposition p) (plposz p)

drawPlayer :: Player -> IO ()
drawPlayer = drawSprite

drawPlayerShadow :: Player -> IO ()
drawPlayerShadow p = drawSprite' (imgtexture (plshadow p)) (playerShadowRectangle p) (plposz p)

playerShadowRectangle :: Player -> Rectangle
playerShadowRectangle p = ((x, y), (w, h))
  where (x, y)   = (bx, by - bh / 2)
        (bx, by) = fst $ playerTexRectangle p
        (w, h) = imgsize $ plshadow p
        (_, bh) = imgsize $ plshadow p

