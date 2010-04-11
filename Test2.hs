module Main where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Array.Storable
import System.IO (hPutStrLn, stderr)
import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)
import Data.Ord
import Data.Function
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable (toList)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL
import Codec.Image.PNG

import SDLUtils
import Swos
import Tree

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
      textlen <- getFontAdvance f str
      translate $ Vector3 (w / 2 - realToFrac textlen / 2) 0 (0 :: GLfloat)
      renderFont f str FTGL.Front

drawGenScene :: Font -> TextureObject -> [Button] -> IO ()
drawGenScene f tex btns = do
  clear [ColorBuffer, DepthBuffer]
  (w, h) <- getWindowSize
  drawBox (Right tex) (color $ Color3 0.4 0.4 (0.4 :: GLfloat)) ((0, 0), (w, h)) (-1) Nothing
  mapM_ (drawButton f) btns
  glSwapBuffers

type Material = Either SColor TextureObject

data Button = Button { buttonMaterial :: Material
                     , buttonBox      :: Camera
                     , buttonLabel    :: String
                     }

drawButton :: Font -> Button -> IO ()
drawButton f b = drawBox (buttonMaterial b) (return ()) (buttonBox b) 0 (Just (buttonLabel b, f))

data RenderContext = RenderContext {
    renderfont :: Font
  , bgtexture  :: TextureObject
  }

data WorldContext = WorldContext {
    rendercontext :: RenderContext
  , worldteams    :: TeamStructure
  }

type TeamStructure = Tree String (String, [SWOSTeam])

type MenuBlock = ReaderT WorldContext IO

structureTeams :: [SWOSTeam] -> TeamStructure
structureTeams ts = Leaf ("All", ts)

getFontAndTexture :: MenuBlock (Font, TextureObject)
getFontAndTexture = do
  c <- rendercontext <$> ask
  return (renderfont c, bgtexture c)

browseTeams :: ButtonHandler
browseTeams _ = do
  allteams <- concatMap snd <$> toList <$> worldteams <$> ask
  liftIO $ print $ length $ allteams
  (_, h) <- liftIO $ getWindowSize
  let quitlabel = "Quit"
      quitbutton = Button (Left SOrange) ((10, 10), (200, 30)) quitlabel
      teambuttons = map (\(n, t) -> Button (Left SOrange) ((20 + 250 * (n `mod` 3), h - (n `div` 3) * 40), (240, 30)) (teamname t)) (zip [1..] allteams)
      allbuttons = quitbutton : teambuttons
      qhandler = \_ -> return True
  genLoop allbuttons [(quitlabel, qhandler)]
  return False

data Nation = Nation {
    natnumber :: Int
  , divisions :: [Division]
  }

data Division = Division {
    divnumber :: Int
  , divnation :: Int
  , divteams :: [SWOSTeam]
  }

splitBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
splitBy f = groupBy ((==) `on` f) . sortBy (comparing f)

splitToNations :: [Division] -> [[Division]]
splitToNations = splitBy divnation

splitToDivisions :: [SWOSTeam] -> [[SWOSTeam]]
splitToDivisions = splitBy teamdivision

arrangeTeams :: [SWOSTeam] -> [Nation]
arrangeTeams ts = map mkNation (splitToNations (map mkDivision (splitToDivisions ts)))

mkDivision :: [SWOSTeam] -> Division
mkDivision ts = Division (teamdivision (head ts)) (teamnation (head ts)) ts

mkNation :: [Division] -> Nation
mkNation ds = Nation (teamnation $ head $ divteams $ head ds) ds

checkGenButtonClicks :: [(String, ButtonHandler)] -> [Button] -> [SDL.Event] -> MenuBlock Bool
checkGenButtonClicks btnhandlers btns evts = do
  btnsclicked <- liftIO $ mouseClickInAnyM [ButtonLeft] (map buttonBox btns) evts
  let mlbl = liftM buttonLabel $ 
               btnsclicked >>= \b ->
               find (\bt -> b == buttonBox bt) btns
  case mlbl of
    Nothing  -> return False
    Just lbl -> do
      let mact = lookup lbl btnhandlers
      case mact of
        Nothing  -> return False
        Just act -> act lbl 

type ButtonHandler = String -> MenuBlock Bool

genLoop :: [Button] -> [(String, ButtonHandler)] -> MenuBlock ()
genLoop btns btnhandlers = do
  (f, tex) <- getFontAndTexture
  liftIO $ drawGenScene f tex btns
  evts <- liftIO $ pollAllSDLEvents
  back <- checkGenButtonClicks btnhandlers btns evts
  let escpressed = isJust $ specificKeyPressed [SDLK_ESCAPE] evts
  if back || escpressed
    then return ()
    else genLoop btns btnhandlers

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
  allteams <- liftIO $ loadTeamsFromDirectory "teams"
  let button1 = Button (Left SOrange) ((300, 200), (200, 30)) quitLabel
      button2 = Button (Left SBlue)   ((300, 400), (200, 30)) browseLabel
      browseLabel = "Browse"
      quitLabel = "Quit"
      buttons = [button1, button2]
      rc = RenderContext f tex
  runReaderT (genLoop buttons [(browseLabel, browseTeams), (quitLabel, \_ -> return True)]) (WorldContext rc (structureTeams allteams))

