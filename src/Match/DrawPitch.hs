module Match.DrawPitch(drawPitch)
where

import Graphics.Rendering.OpenGL as OpenGL

import Drawing
import FVector

drawRect :: Rectangle -> Float -> IO ()
drawRect r d' = preservingMatrix $ do
  let ((a, b), (c, d)) = rectToNum r
      e                = realToFrac d'
  loadIdentity
  translate $ Vector3 a b e
  renderPrimitive Quads $ do
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    vertex $ Vertex3 c 0 (0 :: GLfloat)
    vertex $ Vertex3 c d 0
    vertex $ Vertex3 0 d 0

lw :: Float -- linewidth
lw = 0.15

drawRectBox :: Rectangle -> Float -> Float -> IO ()
drawRectBox ((p, q), (r, s)) w d = do
  drawRect ((p,     q),         (r, w)) d
  drawRect ((p,     q),         (w, s)) d
  drawRect ((p,     q + s - w), (r, w)) d
  drawRect ((p + r - w, q),     (w, s)) d

draw2DArc :: (Float, Float) -> Float -> Float -> Float -> IO ()
draw2DArc p r w d = draw2DArcAngled' p r w d Nothing

draw2DArcAngled :: (Float, Float) -> Float -> Float -> Float -> (Float, Float) -> IO ()
draw2DArcAngled p r w d as = draw2DArcAngled' p r w d (Just as)

draw2DArcAngled' :: (Float, Float) -> Float -> Float -> Float -> (Maybe (Float, Float)) -> IO ()
draw2DArcAngled' (xp', yp') r' w' d' an = preservingMatrix $ do
  let (xp, yp) = (realToFrac xp', realToFrac yp')
      r = realToFrac r'
      w = realToFrac w'
      d = realToFrac d'
  translate $ Vector3 xp yp (d :: GLfloat)
  case an of
    Nothing       -> renderQuadric (QuadricStyle Nothing NoTextureCoordinates Inside FillStyle) (Disk r (r + w) 64 1)
    Just (a1, a2) -> renderQuadric (QuadricStyle Nothing NoTextureCoordinates Inside FillStyle) (PartialDisk r (r + w) 64 1 (realToFrac a1) (realToFrac a2))

drawSpot :: (Float, Float) -> Float -> Float -> IO ()
drawSpot p = draw2DArc p 0

drawPitch :: TextureObject -> FRange -> FRange -> IO ()
drawPitch grtexobj grtiling (px, py) = do
  loadIdentity
  color $ Color3 1.0 1.0 (1.0 :: GLfloat)
  drawTiling grtexobj (return ()) ((-px, -py), (px * 3, py * 3)) (-1) grtiling
  color $ Color3 0.8 0.8 (0.8 :: GLfloat)
  drawRectBox ((0, 0), (px, py)) lw 0 -- pitch boundaries
  drawRect ((0, (py - lw) / 2), (px, lw)) 0  -- middle line
  draw2DArc ((px / 2), (py / 2)) 9.15 lw 0 -- centre ring
  drawSpot ((px / 2), (py / 2)) (lw * 2) 0 -- centre spot
  drawRectBox ((px / 2 - 20.16, 0), (40.32, 16.5)) lw 0 -- penalty area 1
  drawRectBox ((px / 2 - 9.15,  0), (18.3,  5.5))  lw 0 -- goal area 1
  drawSpot (px / 2, 11) (lw * 2) 0 -- penalty spot 1
  draw2DArcAngled (px / 2, 11.1) 9.15 lw 0 (-54.63298, 109.26596) -- penalty arc 1
  drawRectBox ((px / 2 - 20.16, py - 16.5), (40.32, 16.5)) lw 0 -- penalty area 2
  drawRectBox ((px / 2 - 9.15,  py - 5.5), (18.3,  5.5))  lw 0 -- goal area 2
  drawSpot (px / 2, py - 11) (lw * 2) 0 -- penalty spot 2
  draw2DArcAngled (px / 2, py - 11.2) 9.15 lw 0 (125.36702, 109.26596) -- penalty arc 2
  draw2DArcAngled (0, 0) 1 lw 0 (0, 90) -- corner line 1
  draw2DArcAngled (px, 0) 1 lw 0 (0, -90) -- corner line 2
  draw2DArcAngled (0, py) 1 lw 0 (90, 90) -- corner line 3
  draw2DArcAngled (px, py) 1 lw 0 (-90, -90) -- corner line 4


