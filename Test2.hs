module Main where

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

data SColor = SBlue | SOrange

getSColor1 :: (Fractional a) => SColor -> Color3 a
getSColor1 SBlue   = Color3 0 0       0.8
getSColor1 SOrange = Color3 1 0.62352 0

getSColor2 :: (Fractional a) => SColor -> Color3 a
getSColor2 SBlue   = Color3 0   0     0.545
getSColor2 SOrange = Color3 0.8 0.333 0

drawBox :: Either SColor TextureObject -> IO () -> ((Int, Int), (Int, Int)) -> Int -> IO ()
drawBox mat prep ((x', y'), (w', h')) d' = preservingMatrix $ do
  let ((x, y), (w, h)) = ((fromIntegral x', fromIntegral y'), (fromIntegral w', fromIntegral h'))
      d :: GLint
      d = fromIntegral d'
  loadIdentity
  prep
  case mat of
    Right tex -> do
      textureBinding Texture2D $= Just tex
      renderPrimitive Quads $ do
        texCoord (TexCoord2 0 (1 :: GLfloat))
        vertex $ Vertex3 x       y       d
        texCoord (TexCoord2 1 (1 :: GLfloat))
        vertex $ Vertex3 (x + w) y       d
        texCoord (TexCoord2 1 (0 :: GLfloat))
        vertex $ Vertex3 (x + w) (y + h) d
        texCoord (TexCoord2 0 (0 :: GLfloat))
        vertex $ Vertex3 x       (y + h) d
    Left scol -> do
      textureBinding Texture2D $= Nothing
      renderPrimitive Quads $ do
        color $ (getSColor1 scol :: Color3 GLfloat)
        vertex $ Vertex3 x       y       d
        color $ (getSColor2 scol :: Color3 GLfloat)
        vertex $ Vertex3 (x + w) y       d
        color $ (getSColor2 scol :: Color3 GLfloat)
        vertex $ Vertex3 (x + w) (y + h) d
        color $ (getSColor1 scol :: Color3 GLfloat)
        vertex $ Vertex3 x       (y + h) d

drawScene :: TextureObject -> Int -> Int -> IO ()
drawScene tex w h = do
  clear [ColorBuffer, DepthBuffer]

  drawBox (Right tex)    (color $ Color3 0.4 0.4 (0.4 :: GLfloat)) ((0, 0), (w, h)) (-1)
  drawBox (Left SOrange) (return ())                               ((100, 200), (100, 30)) 0
  drawBox (Left SBlue)   (return ())                               ((500, 200), (100, 30)) 0

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
  forever (drawScene tex width height)

