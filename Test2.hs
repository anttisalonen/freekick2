module Main where
-- copied from nehe-tuts, Jeff Molofee '99 (ported to Haskell GHC 2005)

import Control.Monad
import Data.Array.Storable

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import Codec.Image.PNG

width, height :: (Num a) => a
width = 800
height = 600

initGL :: IO TextureObject
initGL = do
  tex <- loadGLTextures
  texture Texture2D $= Enabled
  return tex

loadGLTextures :: IO TextureObject
loadGLTextures = do
  eimg <- loadPNGFile "bg.png"
  case eimg of
    Left err  -> error err
    Right img -> do
      let (imgw, imgh) = dimensions img
      print $ dimensions img
      texName <- liftM head (genObjectNames 1)
      textureBinding Texture2D $= Just texName
      textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
      withStorableArray (imageData img) $ \imgdata -> 
        texImage2D Nothing NoProxy 0 RGB' 
        (TextureSize2D (fromIntegral imgw) (fromIntegral imgh)) 0 
        (PixelData RGB UnsignedByte imgdata)
      return texName

drawScene :: TextureObject -> IO ()
drawScene tex = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity

  color $ Color3 0.4 0.4 (0.4 :: GLfloat)
  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $ do
    texCoord (TexCoord2 0 (1 :: GLfloat))
    vertex (Vertex3 0 0          (-1::GLfloat))
    texCoord (TexCoord2 1 (1 :: GLfloat))
    vertex (Vertex3 width 0      (-1::GLfloat))
    texCoord (TexCoord2 1 (0 :: GLfloat))
    vertex (Vertex3 width height (-1::GLfloat))
    texCoord (TexCoord2 0 (0 :: GLfloat))
    vertex (Vertex3 0 height     (-1::GLfloat))

  textureBinding Texture2D $= Nothing
  preservingMatrix $ do
    translate $ Vector3 100 200 (0 :: GLfloat)
    renderPrimitive Quads $ do
      color  $ Color3  1   0.62352 (0 ::GLfloat)
      vertex $ Vertex3 0   0       (0 ::GLfloat)
      color  $ Color3  0.8 0.33333 (0 ::GLfloat)
      vertex $ Vertex3 100 0       (0 ::GLfloat)
      color  $ Color3  0.8 0.33333 (0 ::GLfloat)
      vertex $ Vertex3 100 30      (0 ::GLfloat)
      color  $ Color3  1   0.62352 (0 ::GLfloat)
      vertex $ Vertex3 0   30      (0 ::GLfloat)

  preservingMatrix $ do
    translate $ Vector3 500 200 (0 :: GLfloat)
    renderPrimitive Quads $ do
      color  $ Color3  0   0       (0.8 ::GLfloat)
      vertex $ Vertex3 0   0       (0 ::GLfloat)
      color  $ Color3  0   0       (0.545 ::GLfloat)
      vertex $ Vertex3 100 0       (0 ::GLfloat)
      color  $ Color3  0   0       (0.545 ::GLfloat)
      vertex $ Vertex3 100 30      (0 ::GLfloat)
      color  $ Color3  0   0       (0.8 ::GLfloat)
      vertex $ Vertex3 0   30      (0 ::GLfloat)

  glSwapBuffers

type Camera = ((GLdouble, GLdouble), (GLdouble, GLdouble))

setCamera :: Camera -> IO ()
setCamera ((minx, miny), (diffx, diffy)) = do
  matrixMode $= Projection
  loadIdentity
  ortho minx (minx + diffx) miny (miny + diffy) (-10) 10
  matrixMode $= Modelview 0

main :: IO ()
main = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  -- perspective 45 (width / height) 0.1 100
  -- ortho ((-1) * width) (1 * width) ((-1) * height) (1 * height) (-10) 10
  setCamera ((0, 0), (width, height))
  matrixMode $= Modelview 0
  tex <- initGL
  forever (drawScene tex)

