{-# LANGUAGE TemplateHaskell #-}
module Match.Player
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
  , controlskill  :: Skill
  }
$(deriveMods ''PlayerSkills)

data PlayerTextureSet = PlayerTextureSet {
    pltextures  :: TextureObject
  , pltexturen  :: TextureObject
  , pltexturew  :: TextureObject
  , pltexturee  :: TextureObject
  }

data Player = Player {
    plposition   :: FRange
  , plimgsize    :: FRange
  , pltextureset :: PlayerTextureSet
  , plshadow     :: ImageInfo
  , plposz       :: Float
  , playerid     :: PlayerID
  , plpos        :: PlPosition
  , kicktimer    :: Int
  , plskills     :: PlayerSkills
  , plrotation   :: Float
  }
$(deriveMods ''Player)

instance Eq Player where
  p1 == p2 = playerid p1 == playerid p2

instance Sprite Player where
  getTexture   = getPlayerTexture
  getRectangle = playerTexRectangle
  getDepth     = playerHeight

-- reduce jitter on diagonal movement with an offset of one degree.
getPlayerTexture :: Player -> TextureObject
getPlayerTexture p 
  | plrotation p >= 134 && plrotation p < 226 = pltextures . pltextureset $ p
  | plrotation p >= 226 && plrotation p < 316 = pltexturew . pltextureset $ p
  | plrotation p >= 44  && plrotation p < 134 = pltexturee . pltextureset $ p
  | otherwise                                 = pltexturen . pltextureset $ p

playerHome :: Player -> Bool
playerHome = snd . playerid

playerNumber :: Player -> Int
playerNumber = fst . playerid

playerTexRectangle :: Player -> Rectangle
playerTexRectangle p =
  ((a - c / 2, b), (c, d))
    where (a, b) = plposition p
          (c, d) = plimgsize p

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

