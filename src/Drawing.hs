module Drawing
where

import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)
import Data.Array.Storable
import Control.Monad
import Data.Maybe
import Foreign.Ptr

import Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL as OpenGL
import Codec.Image.PNG

type FRange = (Float, Float)

type Rectangle = ((Float, Float), (Float, Float))

type Camera = ((Int, Int), (Int, Int))

data SColor = SBlue | SOrange | SRed

getSColor1 :: (Fractional a) => SColor -> Color3 a
getSColor1 SBlue   = Color3 0.6784 0.8470 0.9019 -- light blue
getSColor1 SOrange = Color3 1 0.62352 0 -- orange peel
getSColor1 SRed    = Color3 1 0.6509 0.7882 -- carnation pink

getSColor2 :: (Fractional a) => SColor -> Color3 a
getSColor2 SBlue   = Color3 0.0 0.0   0.803 -- medium blue
getSColor2 SOrange = Color3 0.8 0.333 0 -- burnt orange
getSColor2 SRed    = Color3 0.9843 0.3764 0.5 -- brink pink

rectToNum :: (Fractional a) => Rectangle -> ((a, a), (a, a))
rectToNum ((x, y), (w, h)) = ((realToFrac x, realToFrac y), (realToFrac w, realToFrac h))

camToNum :: (Num a) => Camera -> ((a, a), (a, a))
camToNum ((x, y), (w, h)) = ((fromIntegral x, fromIntegral y), (fromIntegral w, fromIntegral h))

drawTiling :: TextureObject -> IO () -> Rectangle -> Float -> (Float, Float) -> IO ()
drawTiling tex prep c d' (s', t') = preservingMatrix $ do
  let ((x, y), (w, h)) = rectToNum c
      d :: GLfloat
      d = realToFrac d'
      (s, t) = (realToFrac s', realToFrac t')
  loadIdentity
  color $ Color3 1 1 (1 :: GLfloat)
  prep
  translate $ Vector3 x y d
  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $ do
    texCoord (TexCoord2 0       (h / t :: GLfloat))
    vertex $ Vertex3 0 0 (0 :: GLfloat)
    texCoord (TexCoord2 (w / s) (h / t :: GLfloat))
    vertex $ Vertex3 w 0 0
    texCoord (TexCoord2 (w / s) (0 :: GLfloat))
    vertex $ Vertex3 w h (0 :: GLfloat)
    texCoord (TexCoord2 0       (0 :: GLfloat))
    vertex $ Vertex3 0 h 0
  textureBinding Texture2D $= Nothing

loadTexture :: Maybe Int -> Maybe Int -> FilePath -> IO TextureObject
loadTexture mcuttop mcutbottom fp = do
  eimg <- loadPNGFile fp
  case eimg of
    Left err  -> throwIO $ mkIOError doesNotExistErrorType ("could not load texture: " ++ err) Nothing (Just fp)
    Right img -> do
      let (imgw, imgh) = dimensions img
      texName <- liftM head (genObjectNames 1)
      textureBinding Texture2D $= Just texName
      textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
      let isAlpha = hasAlphaChannel img
          intform = if isAlpha then RGBA' else RGB'
          pformat = if isAlpha then RGBA  else RGB
      withStorableArray (imageData img) $ \imgdata -> do
        let movedimg = case mcuttop of
                         Nothing     -> imgdata
                         Just cuttop -> 
                           let numbytes = if isAlpha then 4 else 3
                           in plusPtr imgdata (fromIntegral imgw * cuttop * numbytes)
        let totheight = (fromMaybe (fromIntegral imgh) mcutbottom) - (fromMaybe 0 mcuttop)
        texImage2D Nothing NoProxy 0 intform
          (TextureSize2D (fromIntegral imgw) (fromIntegral totheight)) 0 
          (PixelData pformat UnsignedByte movedimg)
      return texName

setCamera :: Camera -> IO ()
setCamera = setCamera' . camToNum

setCamera' :: Rectangle -> IO ()
setCamera' c = do
  let ((minx, miny), (diffx, diffy)) = rectToNum c
  matrixMode $= Projection
  loadIdentity
  ortho minx (minx + diffx) miny (miny + diffy) (-10) (10 :: GLdouble)
  matrixMode $= Modelview 0

drawSprite :: TextureObject -> Rectangle -> Float -> IO ()
drawSprite tex r f = drawBoxF (Right tex) (return ()) r f Nothing

drawBox :: Either SColor TextureObject -> IO () -> Camera -> Int -> Maybe (String, Font) -> IO ()
drawBox mat prep c d stf =
  drawBoxF mat prep (camToNum c) (fromIntegral d) stf

drawBoxF :: Either SColor TextureObject -> IO () -> Rectangle -> Float -> Maybe (String, Font) -> IO ()
drawBoxF mat prep c d' stf = preservingMatrix $ do
  let ((x, y), (w, h)) = rectToNum c
      d :: GLfloat
      d = realToFrac d'
  loadIdentity
  color $ Color3 1 1 (1 :: GLfloat)
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
      pts <- getFontFaceSize f
      translate $ Vector3 (fromIntegral $ pts `div` 4) (fromIntegral $ pts `div` 4) (1 :: GLfloat)
      textlen <- getFontAdvance f str
      translate $ Vector3 (w / 2 - realToFrac textlen / 2) 0 (0 :: GLfloat)
      renderFont f str FTGL.Front


