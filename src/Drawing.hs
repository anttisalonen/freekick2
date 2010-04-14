module Drawing
where

import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)
import Data.Array.Storable
import Control.Monad
import Data.Maybe
import Foreign.Ptr


import Graphics.Rendering.OpenGL as OpenGL
import Codec.Image.PNG

type FRange = (Float, Float)

type Rectangle = ((Float, Float), (Float, Float))

type Camera = ((Int, Int), (Int, Int))

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
loadTexture mlimmin mlimmax fp = do
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
        let movedimg = case mlimmin of
                         Nothing     -> imgdata
                         Just limmin -> 
                           let numbytes = if isAlpha then 4 else 3
                           in plusPtr imgdata (fromIntegral imgw * limmin * numbytes)
        let totheight = (fromMaybe (fromIntegral imgh) mlimmax) - (fromMaybe 0 mlimmin)
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


