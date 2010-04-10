module Main where
-- copied from nehe-tuts, Jeff Molofee '99 (ported to Haskell GHC 2005)

import Control.Monad
import Data.Array.Storable
import System.IO.Error
import Control.Exception
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import Codec.Image.PNG

width, height :: (Num a) => a
width = 800
height = 600

loadTexture :: FilePath -> IO TextureObject
loadTexture fp = do
  eimg <- loadPNGFile fp
  case eimg of
    Left err  -> throwIO $ mkIOError doesNotExistErrorType ("loading texture:" ++ err) Nothing (Just fp)
    Right img -> do
      let (imgw, imgh) = dimensions img
      texName <- liftM head (genObjectNames 1)
      textureBinding Texture2D $= Just texName
      textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
      let isAlpha = hasAlphaChannel img
          intform = if isAlpha then RGBA' else RGB'
          pformat = if isAlpha then RGBA  else RGB
      withStorableArray (imageData img) $ \imgdata -> 
        texImage2D Nothing NoProxy 0 intform
          (TextureSize2D (fromIntegral imgw) (fromIntegral imgh)) 0 
          (PixelData pformat UnsignedByte imgdata)
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
  setCamera ((0, 0), (width, height))
  matrixMode $= Modelview 0
  texture Texture2D $= Enabled
  tex <- loadTexture "bg.png"
  forever (drawScene tex)

