{-# LANGUAGE TemplateHaskell #-}
module Match.Ball
where

import Control.Monad.State

import Drawing
import FVector
import DeriveMod

data Ball = Ball {
    ballposition :: FVector3
  , ballvelocity :: FVector3
  , ballimage    :: ImageInfo
  , ballshadow   :: ImageInfo
  , ballposz     :: Float
  }
$(deriveMods ''Ball)

instance Sprite Ball where
  getTexture     = imgtexture . ballimage
  getRectangle b = ballTexRectangle True b
  getDepth b     = topDownDepth (to2D $ ballposition b) (ballposz b)

initialBall :: Float -> FRange -> ImageInfo -> ImageInfo -> Ball
initialBall zv (px, py) img shimg = Ball (px / 2, py / 2, 0) nullFVector3 img shimg zv

ballTexRectangle :: Bool -> Ball -> Rectangle
ballTexRectangle sh b = ((x - s / 2, y - t / 2 + sz), (s, t))
      where (x, y, z) = ballposition b
            (s, t) = imgsize $ ballimage b
            sz = if sh
                   then z * 1.66
                   else 0

collCheckBall :: Float -> Ball -> Ball
collCheckBall bounc b =
  let zv = getZ (ballposition b)
      zvel = getZ $ ballvelocity b
  in if zv < 0  -- bounciness
       then modBallposition (addZ (2 * (-zv))) (modBallvelocity (addZ (bounc * (-zvel))) b)
     else
       b

gravitateBall :: Float -> Float -> Ball -> Ball
gravitateBall g dt b = 
  let zv = getZ $ ballposition $ b
      zvel = getZ $ ballvelocity $ b
  in if (zv > 0.01)
       then 
         modBallvelocity (addZ (g * dt)) b
       else
         if (abs zvel < 0.1) 
           then modBallvelocity (setZ 0) b
           else b

slowDownBall :: Float -> Float -> Float -> Ball -> Ball
slowDownBall vis rol dt b =
  let zv = getZ $ ballposition $ b
      zvel = getZ $ ballvelocity $ b
  in if zv > 0.01
       then  -- air viscosity
         modBallvelocity (*** (1 - (vis * dt))) b
       else  -- rolling friction
         modBallvelocity (*** (1 - (rol * dt))) b

drawBall :: Ball -> IO ()
drawBall = drawSprite 

drawBallShadow :: Ball -> IO ()
drawBallShadow b = drawSprite' (imgtexture (ballshadow b)) (ballShadowRectangle b) (ballposz b)

ballShadowRectangle :: Ball -> Rectangle
ballShadowRectangle b = ((x, y), (w, h))
  where (x, y)   = (bx, by - bh / 2)
        (bx, by) = fst $ ballTexRectangle False b
        (w, h) = imgsize $ ballshadow b
        (_, bh) = imgsize $ ballimage b

