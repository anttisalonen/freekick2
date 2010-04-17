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

data Player = Player {
    plposition :: FRange
  , plimage    :: ImageInfo
  , plposz     :: Float
  , playerid   :: PlayerID
  , plpos      :: PlPosition
  }
$(deriveMods ''Player)

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
playerHeight p = 
  let (_, yv) = plposition p
  in plposz p - yv * 0.002

drawPlayer :: Player -> IO ()
drawPlayer = drawSprite


