{-# LANGUAGE TemplateHaskell #-}
module Match.Player(TeamOwner(..),
  PlayerID,
  PlayerTextureSet(..),
  Player(..),
  modPlrotation,
  modPlposition,
  modKicktimer,
  playerHome,
  playerNumber,
  genPlayerToPlayer,
  drawPlayer,
  drawPlayerShadow)
where

import Control.Monad.State

import Drawing
import FVector
import qualified Gen
import DeriveMod

data TeamOwner = HumanOwner | AIOwner
  deriving (Eq)

type PlayerID = (Int, Bool)

data PlayerTextureSet = PlayerTextureSet {
    pltextures  :: TextureObject
  , pltexturen  :: TextureObject
  , pltexturew  :: TextureObject
  , pltexturee  :: TextureObject
  }

data Player = Player {
    plposition   :: FRange
  , plname       :: String
  , plimgsize    :: FRange
  , pltextureset :: PlayerTextureSet
  , plshadow     :: ImageInfo
  , plposz       :: Float
  , playerid     :: PlayerID
  , plpos        :: Gen.PlPosition
  , kicktimer    :: Int
  , plskills     :: Gen.PlayerSkills
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

genPlayerToPlayer :: Float 
                  -> Bool
                  -> PlayerTextureSet
                  -> ImageInfo 
                  -> FRange 
                  -> FRange 
                  -> Gen.GenPlayer
                  -> Player
genPlayerToPlayer inz home pltexset plshimg hsize (px, py) p = 
  Player (px - 2, py / 2) (Gen.plname p) hsize pltexset plshimg inz ((Gen.plnumber p), home) npos 0 psk 180
    where npos = Gen.genplpos p
          psk = Gen.plskills p


