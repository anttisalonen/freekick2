module Main where

import Control.Monad
import Data.Maybe
import Data.Array.Storable
import System.IO (hPutStrLn, stderr)
import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL
import Codec.Image.PNG

import SDLUtils

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

drawBox :: Either SColor TextureObject -> IO () -> ((Int, Int), (Int, Int)) -> Int -> Maybe (String, Font) -> IO ()
drawBox mat prep ((x', y'), (w', h')) d' stf = preservingMatrix $ do
  let ((x, y), (w, h)) = ((fromIntegral x', fromIntegral y'), (fromIntegral w', fromIntegral h'))
      d :: GLfloat
      d = fromIntegral d'
  loadIdentity
  prep
  translate $ Vector3 x y d
  case mat of
    Right tex -> do
      textureBinding Texture2D $= Just tex
      renderPrimitive Quads $ do
        texCoord (TexCoord2 0 (1 :: GLfloat))
        vertex $ Vertex3 0 0 (0 :: GLfloat)
        texCoord (TexCoord2 1 (1 :: GLfloat))
        vertex $ Vertex3 w 0 0
        texCoord (TexCoord2 1 (0 :: GLfloat))
        vertex $ Vertex3 w h (0 :: GLfloat)
        texCoord (TexCoord2 0 (0 :: GLfloat))
        vertex $ Vertex3 0 h 0
    Left scol -> do
      textureBinding Texture2D $= Nothing
      renderPrimitive Quads $ do
        color $ (getSColor1 scol :: Color3 GLfloat)
        vertex $ Vertex3 0 0 (0 :: GLfloat)
        color $ (getSColor2 scol :: Color3 GLfloat)
        vertex $ Vertex3 w 0 0
        color $ (getSColor2 scol :: Color3 GLfloat)
        vertex $ Vertex3 w h 0
        color $ (getSColor1 scol :: Color3 GLfloat)
        vertex $ Vertex3 0 h 0
  case stf of
    Nothing       -> return ()
    Just (str, f) -> do
      color $ Color3 0 0 (0 :: GLint)
      translate $ Vector3 6 6 (1 :: GLfloat)
      renderFont f str FTGL.Front

drawScene :: Font -> TextureObject -> Int -> Int -> IO ()
drawScene f tex w h = do
  clear [ColorBuffer, DepthBuffer]

  drawBox (Right tex)    (color $ Color3 0.4 0.4 (0.4 :: GLfloat)) ((0, 0), (w, h))        (-1) Nothing
  drawBox (Left SOrange) (return ())                               ((100, 200), (200, 30)) 0    (Just ("orange button", f))
  drawBox (Left SBlue)   (return ())                               ((500, 200), (200, 30)) 0    (Just ("blue button", f))
  glSwapBuffers

loop :: Font -> TextureObject -> Int -> Int -> IO ()
loop f tex w h = do
  drawScene f tex w h
  evts <- pollAllSDLEvents
  if isJust $ specificKeyPressed [SDLK_ESCAPE] evts
    then return ()
    else loop f tex w h

type Camera = ((Int, Int), (Int, Int))

setCamera :: Camera -> IO ()
setCamera ((minx', miny'), (diffx', diffy')) = do
  let ((minx, miny), (diffx, diffy)) = ((fromIntegral minx', fromIntegral miny'), (fromIntegral diffx', fromIntegral diffy'))
  matrixMode $= Projection
  loadIdentity
  ortho minx (minx + diffx) miny (miny + diffy) (-10) (10 :: GLdouble)
  matrixMode $= Modelview 0

main :: IO ()
main = catch run (\e -> hPutStrLn stderr $ "Exception: " ++ show (e :: IOException))

loadDataFont :: FilePath -> IO Font
loadDataFont fp = do
  -- fn <- getDataFileName fp
  -- exists <- doesFileExist fn
  -- when (not exists) $ do
    -- throwIO $ mkIOError doesNotExistErrorType "loading data font during initialization" Nothing (Just fn)
  f <- createTextureFont fp
  _ <- setFontFaceSize f 24 48
  return f

run :: IO ()
run = do
  let width, height :: (Num a) => a
      width = 800
      height = 600
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
  f <- loadDataFont "DejaVuSans.ttf"
  loop f tex width height

