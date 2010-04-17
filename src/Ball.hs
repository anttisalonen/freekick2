{-# LANGUAGE TemplateHaskell #-}
module Ball
where

import Control.Monad.State

import Drawing
import FVector
import DeriveMod

data Ball = Ball {
    ballposition :: FVector3
  , ballvelocity :: FVector3
  , ballimage    :: ImageInfo
  , ballposz     :: Float
  }
$(deriveMods ''Ball)

instance Sprite Ball where
  getTexture     = imgtexture . ballimage
  getRectangle b = ballTexRectangle b
  getDepth b     = topDownDepth (to2D $ ballposition b) (ballposz b)

initialBall :: Float -> FRange -> ImageInfo -> Ball
initialBall zv (px, py) img = Ball (px / 2, py / 2, 0) nullFVector3 img zv

ballTexRectangle :: Ball -> Rectangle
ballTexRectangle b = ((x - s / 2, y - t / 2), (s, t))
      where (x, y) = to2D (ballposition b)
            (s, t) = imgsize $ ballimage b

collCheckBall :: Ball -> Ball
collCheckBall b =
  let zv = getZ (ballposition b)
      zvel = getZ $ ballvelocity b
  in if zv < 0
       then modBallposition (addZ (2 * (-zv))) (modBallvelocity (addZ (2 * (-zvel))) b)
     else
       b

gravitateBall :: Float -> Ball -> Ball
gravitateBall dt b = 
  let zv = getZ $ ballposition $ b
      zvel = getZ $ ballvelocity $ b
  in if (zv > 0.01)
       then 
         modBallvelocity (addZ (-9.81 * dt)) b
       else
         if (abs zvel < 0.1) 
           then modBallvelocity (setZ 0) b
           else b

slowDownBall :: Float -> Ball -> Ball
slowDownBall dt b =
  let zv = getZ $ ballposition $ b
      zvel = getZ $ ballvelocity $ b
  in if zv > 0.01
       then  -- air viscosity
         modBallvelocity (*** (1 - (1 * dt))) b
       else  -- rolling friction
         modBallvelocity (*** (1 - (1 * dt))) b

drawBall :: Ball -> IO ()
drawBall = drawSprite 


