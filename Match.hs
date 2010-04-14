module Match
where

import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)
import Data.Array.Storable
import Control.Monad
import Control.Monad.State as State
import Data.Maybe
import Control.Applicative

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL
import Codec.Image.PNG

import Swos
import Tree
import SDLUtils

data RenderContext = RenderContext {
    renderfont  :: Font
  , smallerfont :: Font
  , bgtexture   :: TextureObject
  }

data WorldContext = WorldContext {
    rendercontext :: RenderContext
  , worldteams    :: TeamStructure
  , hometeam      :: Maybe (SWOSTeam, TeamOwner)
  , awayteam      :: Maybe (SWOSTeam, TeamOwner)
  }

data TeamOwner = HumanOwner | AIOwner

type TeamStructure = Tree String (String, [SWOSTeam])

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

type Rectangle = ((Float, Float), (Float, Float))

type Camera = ((Int, Int), (Int, Int))

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

drawBox :: Either SColor TextureObject -> IO () -> ((Int, Int), (Int, Int)) -> Int -> Maybe (String, Font) -> IO ()
drawBox mat prep c d' stf = preservingMatrix $ do
  let ((x, y), (w, h)) = camToNum c
      d :: GLfloat
      d = fromIntegral d'
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

loadTexture :: FilePath -> IO TextureObject
loadTexture fp = do
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
      withStorableArray (imageData img) $ \imgdata -> 
        texImage2D Nothing NoProxy 0 intform
          (TextureSize2D (fromIntegral imgw) (fromIntegral imgh)) 0 
          (PixelData pformat UnsignedByte imgdata)
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

playMatch :: Font -> (SWOSTeam, TeamOwner) -> (SWOSTeam, TeamOwner) -> IO ()
playMatch f _ _ = do
  tex <- loadTexture "grass1.png"
  evalStateT runMatch (initMatchState tex (16, 16) (68, 105) (20, 40))
  putStrLn "Match played! Yay!"
  (w, h) <- liftIO $ getWindowSize
  setCamera ((0, 0), (w, h))

data MatchState = MatchState {
    grasstexture :: GrassTexture
  , currkeys     :: [SDLKey]
  , pitchsize    :: (Float, Float)
  , campos       :: (Float, Float)
  }

modCurrkeys :: ([SDLKey] -> [SDLKey]) -> MatchState -> MatchState
modCurrkeys f c = c{currkeys = f (currkeys c)}

modCampos :: (FRange -> FRange) -> MatchState -> MatchState
modCampos f c = c{campos = f (campos c)}

goUp :: Float -> FRange -> FRange
goUp n (x, y) = (x, y + n)

goRight :: Float -> FRange -> FRange
goRight n (x, y) = (x + n, y)

data GrassTexture = GrassTexture {
    grasstexobj  :: TextureObject
  , grasstiling  :: (Float, Float)
  }

type FRange = (Float, Float)

initMatchState :: TextureObject -> FRange -> FRange -> FRange -> MatchState
initMatchState tex til = MatchState (GrassTexture tex til) []

type Match = StateT MatchState IO

keyChanges :: [SDL.Event] -> [(SDLKey, Bool)]
keyChanges = catMaybes . map f
  where f (KeyDown (Keysym n _ _)) = Just (n, True)
        f (KeyUp   (Keysym n _ _)) = Just (n, False)
        f _                        = Nothing

updateKeyMap :: [(SDLKey, Bool)] -> [SDLKey] -> [SDLKey]
updateKeyMap []              m = m
updateKeyMap ((k, True):ns)  m = updateKeyMap ns (k:m)
updateKeyMap ((k, False):ns) m = updateKeyMap ns (filter (/= k) m)

handleKeyEvents :: Match Bool
handleKeyEvents = do
  evts <- liftIO $ pollAllSDLEvents
  modify $ modCurrkeys $ updateKeyMap (keyChanges evts)
  ks <- currkeys <$> State.get
  when (SDLK_UP `elem` ks) $ modify $ modCampos (goUp 1)
  when (SDLK_DOWN `elem` ks) $ modify $ modCampos (goUp (-1))
  when (SDLK_LEFT `elem` ks) $ modify $ modCampos (goRight (-1))
  when (SDLK_RIGHT `elem` ks) $ modify $ modCampos (goRight 1)
  return (SDLK_ESCAPE `elem` ks)

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

drawPitch :: GrassTexture -> FRange -> IO ()
drawPitch grtexture psize@(px, py) = do
  loadIdentity
  drawTiling (grasstexobj grtexture) (return ()) ((0, 0), psize) (-1) (grasstiling grtexture)
  color $ Color3 0.8 0.8 (0.8 :: GLfloat)
  drawRectBox ((0, 0), (px, py)) lw 0 -- pitch boundaries
  drawRect ((0, (py - lw) / 2), (px, lw)) 0  -- middle line
  draw2DArc ((px / 2), (py / 2)) 9.15 lw 0 -- centre ring
  drawSpot ((px / 2), (py / 2)) (lw * 2) 0 -- centre spot
  drawRectBox ((px / 2 - 20.16, 0), (40.32, 16.5)) lw 0 -- penalty area 1
  drawRectBox ((px / 2 - 9.15,  0), (18.3,  5.5))  lw 0 -- goal area 1
  drawSpot (px / 2, 11) (lw * 2) 0 -- penalty spot 1
  -- draw2DArcAngled (px / 2, 11) 9.15 lw 0 (-33.367012, 66.7340259) -- penalty arc 1
  draw2DArcAngled (px / 2, 11.1) 9.15 lw 0 (-54.63298, 109.26596) -- penalty arc 1
  drawRectBox ((px / 2 - 20.16, py), (40.32, -16.5)) lw 0 -- penalty area 2
  drawRectBox ((px / 2 - 9.15,  py), (18.3,  -5.5))  lw 0 -- goal area 2
  drawSpot (px / 2, py - 11) (lw * 2) 0 -- penalty spot 2
  draw2DArcAngled (px / 2, py - 11.2) 9.15 lw 0 (125.36702, 109.26596) -- penalty arc 2
  draw2DArcAngled (0, 0) 1 lw 0 (0, 90) -- corner line 1
  draw2DArcAngled (px, 0) 1 lw 0 (0, -90) -- corner line 2
  draw2DArcAngled (0, py) 1 lw 0 (90, 90) -- corner line 3
  draw2DArcAngled (px, py) 1 lw 0 (-90, -90) -- corner line 4
  draw2DArcAngled (0, 0) 5 lw 0 (0, 0)
  draw2DArcAngled (10, 10) 5 lw 0 (0, 30)
  draw2DArcAngled (20, 20) 5 lw 0 (0, 60)
  draw2DArcAngled (30, 30) 5 lw 0 (0, 120)
  draw2DArcAngled (40, 40) 5 lw 0 (45, 30)
  draw2DArcAngled (50, 50) 5 lw 0 (90, 60)
  draw2DArcAngled (60, 60) 5 lw 0 (-90, -30)
  draw2DArcAngled (70, 70) 5 lw 0 (-120, -60)

runMatch :: Match ()
runMatch = do
  liftIO $ clear [ColorBuffer, DepthBuffer]
  s <- State.get
  (w, h) <- liftIO $ getWindowSize
  liftIO $ setCamera' (campos s, (fromIntegral (w `div` 20), fromIntegral (h `div` 20)))
  liftIO $ drawPitch (grasstexture s) (pitchsize s)
  liftIO $ glSwapBuffers
  liftIO $ SDL.delay 10
  quitting <- handleKeyEvents
  if quitting
    then return ()
    else runMatch

