module Main() where

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
      pts <- getFontFaceSize f
      translate $ Vector3 (fromIntegral $ pts `div` 4) (fromIntegral $ pts `div` 4) (1 :: GLfloat)
      textlen <- getFontAdvance f str
      translate $ Vector3 (w / 2 - realToFrac textlen / 2) 0 (0 :: GLfloat)
      renderFont f str FTGL.Front

drawGenScene :: TextureObject -> [Button a] -> IO ()
drawGenScene tex btns = do
  clear [ColorBuffer, DepthBuffer]
  (w, h) <- getWindowSize
  drawBox (Right tex) (color $ Color3 0.4 0.4 (0.4 :: GLfloat)) ((0, 0), (w, h)) (-1) Nothing
  mapM_ drawButton btns
  glSwapBuffers

type Material = Either SColor TextureObject

data Button a = Button { buttonMaterial :: Material
                       , buttonBox      :: Camera
                       , buttonLabel    :: String
                       , buttonFont     :: Font
                       , buttonAction   :: String -> a
                       }

drawButton :: Button a -> IO ()
drawButton b = drawBox (buttonMaterial b) (return ()) (buttonBox b) 0 (Just (buttonLabel b, buttonFont b))

data RenderContext = RenderContext {
    renderfont  :: Font
  , smallerfont :: Font
  , bgtexture   :: TextureObject
  }

data WorldContext = WorldContext {
    rendercontext :: RenderContext
  , worldteams    :: TeamStructure
  }

type TeamStructure = Tree String (String, [SWOSTeam])

type MenuBlock = ReaderT WorldContext IO

structureTeams :: [SWOSTeam] -> TeamStructure
structureTeams ts = f "World" ts (countryContinent . nationToString, continentToString) `g` (teamnation, nationToString) `g` (teamdivision, divisionToString)
  where f :: (Ord a) => String -> [SWOSTeam] -> (SWOSTeam -> a, SWOSTeam -> String) -> TeamStructure
        f n teams (func, nfunc) = 
          let ts' = splitBy func teams
          in Node n (map (\tp -> Leaf (nfunc (head tp), tp)) ts')
        g :: (Ord a) => TeamStructure -> (SWOSTeam -> a, SWOSTeam -> String) -> TeamStructure
        g tr (func, nfunc) =
          go tr
            where go (Node i ts')    = Node i (map go ts')
                  go (Leaf (i, ts')) = f i ts' (func, nfunc)
        nationToString    = showTeamNation . teamnation
        divisionToString  = showDivision . teamdivision
        continentToString = show . countryContinent . nationToString

data Continent = NorthAmerica
               | SouthAmerica
               | Oceania
               | Africa
               | Asia
               | Europe
               | OtherContinent
  deriving (Ord, Eq)

instance Show Continent where
  show NorthAmerica   = "North America"
  show SouthAmerica   = "South America"
  show Oceania        = "Oceania"
  show Africa         = "Africa"
  show Asia           = "Asia"
  show Europe         = "Europe"
  show OtherContinent = "Other"

countryContinent :: String -> Continent
countryContinent "El Salvador" = NorthAmerica
countryContinent "Mexico" = NorthAmerica
countryContinent "U.S.A." = NorthAmerica
countryContinent "Argentina" = SouthAmerica
countryContinent "Bolivia" = SouthAmerica
countryContinent "Brazil" = SouthAmerica
countryContinent "Chile" = SouthAmerica
countryContinent "Colombia" = SouthAmerica
countryContinent "Ecuador" = SouthAmerica
countryContinent "Paraguay" = SouthAmerica
countryContinent "Peru" = SouthAmerica
countryContinent "Surinam" = SouthAmerica
countryContinent "Uruguay" = SouthAmerica
countryContinent "Venezuela" = SouthAmerica
countryContinent "Australia" = Oceania
countryContinent "New Zealand" = Oceania
countryContinent "India" = Asia
countryContinent "Japan" = Asia
countryContinent "Taiwan" = Asia
countryContinent "Algeria" = Africa
countryContinent "Ghana" = Africa
countryContinent "South Africa" = Africa
countryContinent "Albania" = Europe
countryContinent "Austria" = Europe
countryContinent "Belarus" = Europe
countryContinent "Belgium" = Europe
countryContinent "Bulgaria" = Europe
countryContinent "Croatia" = Europe
countryContinent "Cyprus" = Europe
countryContinent "Czech Republic" = Europe
countryContinent "Denmark" = Europe
countryContinent "England" = Europe
countryContinent "Estonia" = Europe
countryContinent "Faroe Islands" = Europe
countryContinent "Finland" = Europe
countryContinent "France" = Europe
countryContinent "Germany" = Europe
countryContinent "Greece" = Europe
countryContinent "Holland" = Europe
countryContinent "The Netherlands" = Europe
countryContinent "Hungary" = Europe
countryContinent "Iceland" = Europe
countryContinent "Israel" = Europe
countryContinent "Italy" = Europe
countryContinent "Latvia" = Europe
countryContinent "Lithuania" = Europe
countryContinent "Luxembourg" = Europe
countryContinent "Malta" = Europe
countryContinent "Ireland" = Europe
countryContinent "Northern Ireland" = Europe
countryContinent "Norway" = Europe
countryContinent "Poland" = Europe
countryContinent "Portugal" = Europe
countryContinent "Republic Ireland" = Europe
countryContinent "Romania" = Europe
countryContinent "Russia" = Europe
countryContinent "San Marino" = Europe
countryContinent "Scotland" = Europe
countryContinent "Slovakia" = Europe
countryContinent "Slovenia" = Europe
countryContinent "Spain" = Europe
countryContinent "Sweden" = Europe
countryContinent "Switzerland" = Europe
countryContinent "Turkey" = Europe
countryContinent "Ukraine" = Europe
countryContinent "Wales" = Europe
countryContinent "Yugoslavia" = Europe
countryContinent "Bosnia-Herzegovina" = Europe
countryContinent "Serbia" = Europe
countryContinent "Montenegro" = Europe
countryContinent "Kosovo" = Europe
countryContinent "FYR Macedonia" = Europe
countryContinent "Macedonia" = Europe
countryContinent "Azerbaijan" = Europe
countryContinent "Armenia" = Europe
countryContinent "Georgia" = Europe
countryContinent _ = OtherContinent

getFontAndTexture :: MenuBlock (Font, TextureObject)
getFontAndTexture = do
  c <- rendercontext <$> ask
  return (renderfont c, bgtexture c)

getTwoFonts :: MenuBlock (Font, Font)
getTwoFonts = do
  c <- rendercontext <$> ask
  return (renderfont c, smallerfont c)

getTSLabel :: TeamStructure -> String
getTSLabel (Node i _)    = i
getTSLabel (Leaf (i, _)) = i

getTSChildrenTitles :: TeamStructure -> [String]
getTSChildrenTitles = (either (map getTSLabel) (map teamname)) . getTSChildren

getTSTitles :: TeamStructure -> (String, [String])
getTSTitles t = (getTSLabel t, getTSChildrenTitles t)

getTSChildren :: TeamStructure -> Either [TeamStructure] [SWOSTeam]
getTSChildren (Node _ ts)    = Left ts
getTSChildren (Leaf (_, ts)) = Right ts

getTSChildrenByTitle :: TeamStructure -> String -> Maybe (Either TeamStructure SWOSTeam)
getTSChildrenByTitle ts n =
  case getTSChildren ts of
    Left ts'  -> liftM Left  $ find (\t -> getTSLabel t == n) ts'
    Right tms -> liftM Right $ find (\t -> teamname t == n) tms

browseTeams :: TeamStructure -> ButtonHandler
browseTeams toplevel _ = do
  let (title, labels) = getTSTitles toplevel
  (f1, f2) <- getTwoFonts
  (w, h) <- liftIO $ getWindowSize
  let quitlabel = "Quit"
      quitbutton = Button (Left SOrange) ((10, 10), (200, 30)) quitlabel f1 (\_ -> return True)
      teambuttons = map 
        (\(n, t) -> 
           Button (Left SOrange) 
                  ((20 + 250 * (n `mod` 3), h - 100 - (n `div` 3) * 25), (240, 20)) 
                  t f2 lhandler) 
        (zip [0..] labels)
      titlebutton = Button (Left SOrange) ((w `div` 2 - 100, h - 50), (200, 30)) title f1 (\_ -> return False)
      allbuttons = quitbutton : titlebutton : teambuttons
      lhandler lbl = case getTSChildrenByTitle toplevel lbl of
                       Nothing        -> return False
                       Just (Left t)  -> browseTeams t (getTSLabel t)
                       Just (Right t) -> liftIO (putStrLn $ "chose team: " ++ (teamname t)) >> return False
  genLoop allbuttons
  return False

splitBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
splitBy f = groupBy ((==) `on` f) . sortBy (comparing f)

checkGenButtonClicks :: (MonadIO m) => [Button (m Bool)] -> [SDL.Event] -> m Bool
checkGenButtonClicks btns evts = do
  btnsclicked <- liftIO $ mouseClickInAnyM [ButtonLeft] (map buttonBox btns) evts
  let mlbl = liftM buttonLabel $ 
               btnsclicked >>= \b ->
               find (\bt -> b == buttonBox bt) btns
  case mlbl of
    Nothing  -> return False
    Just lbl -> do
      let mbt = find (\b -> buttonLabel b == lbl) btns
      case mbt of
        Nothing -> return False
        Just bt -> (buttonAction bt) lbl

type ButtonHandler = String -> MenuBlock Bool

genLoop :: [Button (MenuBlock Bool)] -> MenuBlock ()
genLoop btns = do
  (_, tex) <- getFontAndTexture
  liftIO $ drawGenScene tex btns
  evts <- liftIO $ pollAllSDLEvents
  back <- checkGenButtonClicks btns evts
  let escpressed = isJust $ specificKeyPressed [SDLK_ESCAPE] evts
  if back || escpressed
    then return ()
    else genLoop btns

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

loadDataFont :: Int -> Int -> FilePath -> IO Font
loadDataFont sz pt fp = do
  -- fn <- getDataFileName fp
  -- exists <- doesFileExist fn
  -- when (not exists) $ do
    -- throwIO $ mkIOError doesNotExistErrorType "loading data font during initialization" Nothing (Just fn)
  f <- createTextureFont fp
  _ <- setFontFaceSize f sz pt
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
  f <- loadDataFont 24 48 "DejaVuSans.ttf"
  f2 <- loadDataFont 16 48 "DejaVuSans.ttf"
  allteams <- structureTeams `fmap` loadTeamsFromDirectory "teams"
  let button1 = Button (Left SOrange) ((300, 200), (200, 30)) quitLabel f (\_ -> return True)
      button2 = Button (Left SBlue)   ((300, 400), (200, 30)) browseLabel f (browseTeams allteams)
      browseLabel = "Browse"
      quitLabel = "Quit"
      buttons = [button1, button2]
      rc = RenderContext f f2 tex
  runReaderT (genLoop buttons) (WorldContext rc allteams)

