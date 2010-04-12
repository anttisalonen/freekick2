module Main() where

import Control.Monad
import Data.Maybe
import Data.List
import System.IO (hPutStrLn, stderr)
import Control.Exception
import Prelude hiding (catch)
import Data.Ord
import Data.Function
import Control.Monad.State as State
import Control.Applicative

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import SDLUtils
import Swos
import Tree
import Match

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

modHometeam :: (Maybe (SWOSTeam, TeamOwner) -> Maybe (SWOSTeam, TeamOwner)) -> WorldContext -> WorldContext
modHometeam f c = c{hometeam = f (hometeam c)}

modAwayteam :: (Maybe (SWOSTeam, TeamOwner) -> Maybe (SWOSTeam, TeamOwner)) -> WorldContext -> WorldContext
modAwayteam f c = c{awayteam = f (awayteam c)}

type MenuBlock = StateT WorldContext IO

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
  c <- rendercontext <$> State.get
  return (renderfont c, bgtexture c)

getTwoFonts :: MenuBlock (Font, Font)
getTwoFonts = do
  c <- rendercontext <$> State.get
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

hasJust :: (Eq a) => a -> Maybe a -> Bool
hasJust _ Nothing  = False
hasJust n (Just m) = n == m

rotateTeam :: SWOSTeam -> Maybe (SWOSTeam, TeamOwner) -> Maybe (SWOSTeam, TeamOwner)
rotateTeam t Nothing                = Just (t, HumanOwner)
rotateTeam _ (Just (t, HumanOwner)) = Just (t, AIOwner)
rotateTeam _ (Just (_, AIOwner))    = Nothing

getOwner :: String -> WorldContext -> Maybe TeamOwner
getOwner t c =
  let t1 = case hometeam c of
             Nothing       -> Nothing
             Just (ht, ho) -> if teamname ht == t then Just ho else Nothing
      t2 = case awayteam c of
             Nothing       -> Nothing
             Just (ht, ho) -> if teamname ht == t then Just ho else Nothing
  in t1 `mplus` t2

clickedOnTeam :: SWOSTeam -> MenuBlock ()
clickedOnTeam t = do
  c <- State.get
  if hasJust (teamname t) (liftM (teamname . fst) (hometeam c))
    then modify $ modHometeam $ rotateTeam t
    else if hasJust (teamname t) (liftM (teamname . fst) (awayteam c))
           then modify $ modAwayteam $ rotateTeam t
           else if isNothing (hometeam c)
                  then modify $ modHometeam $ rotateTeam t
                  else if isNothing (awayteam c)
                         then modify $ modAwayteam $ rotateTeam t
                         else return ()

browseTeams :: TeamStructure -> ButtonHandler
browseTeams toplevel _ = do
  let (_, labels) = getTSTitles toplevel
  if length labels == 1
    then browserButtonHandler toplevel (head labels)
    else do
      mutLoop (browseTeams' toplevel)
      return False

browserButtonHandler :: TeamStructure -> String -> MenuBlock Bool
browserButtonHandler toplevel lbl =
  case getTSChildrenByTitle toplevel lbl of
    Nothing        -> return False
    Just (Left t)  -> browseTeams t (getTSLabel t)
    Just (Right t) -> clickedOnTeam t >> return False

continueToMatch :: MenuBlock ()
continueToMatch = do
  c <- State.get
  (f1, f2) <- getTwoFonts
  (w, h) <- liftIO $ getWindowSize
  case hometeam c of
    Nothing       -> return ()
    Just (ht, ho) -> do
      case awayteam c of
        Nothing       -> return ()
        Just (at, ao) -> do
          let quitlabel = "Back"
              title = "Match"
              quitbutton = Button (Left SOrange) ((10, 10), (200, 30)) quitlabel f1 (\_ -> return True)
              team1buttons = map 
                (\(n, t) -> 
                   Button (Left SOrange)
                          ((20, h - 100 - n * 25), (240, 20)) 
                          t f2 (\_ -> return False))
                (zip [0..] t1labels)
              t1labels = map plname (teamplayers ht)
              team2buttons = map 
                (\(n, t) -> 
                   Button (Left SOrange)
                          ((520, h - 100 - n * 25), (240, 20)) 
                          t f2 (\_ -> return False))
                (zip [0..] t2labels)
              t2labels = map plname (teamplayers at)
              titlebutton = Button (Left SOrange) ((w `div` 2 - 100, h - 50), (200, 30)) title f1 (\_ -> return False)
              contlabel = "Play"
              contbutton = Button (Left SOrange) ((w - 210, 10), (200, 30)) contlabel f1 (\_ -> liftIO (playMatch f2 (ht, ho) (at, ao)) >> return False)
              allbuttons = contbutton : quitbutton : titlebutton : team1buttons ++ team2buttons
          genLoop allbuttons

ownerToColor :: String -> WorldContext -> SColor
ownerToColor t c = 
  case getOwner t c of
    Nothing         -> SOrange
    Just AIOwner    -> SRed
    Just HumanOwner -> SBlue

browseTeams' :: TeamStructure -> MenuBlock [Button (MenuBlock Bool)]
browseTeams' toplevel = do
  let (title, labels) = getTSTitles toplevel
  c <- State.get
  (f1, f2) <- getTwoFonts
  (w, h) <- liftIO $ getWindowSize
  let quitlabel = "Back"
      quitbutton = Button (Left SOrange) ((10, 10), (200, 30)) quitlabel f1 (\_ -> return True)
      teambuttons = map 
        (\(n, t) -> 
           Button (Left (ownerToColor t c))
                  ((20 + 250 * (n `mod` 3), h - 100 - (n `div` 3) * 25), (240, 20)) 
                  t f2 (browserButtonHandler toplevel)) 
        (zip [0..] labels)
      titlebutton = Button (Left SOrange) ((w `div` 2 - 100, h - 50), (200, 30)) title f1 (\_ -> return False)
      contlabel = "Play"
      mcont = if isJust (hometeam c) && isJust (awayteam c)
                then Just $ Button (Left SOrange) ((w - 210, 10), (200, 30)) contlabel f1 (\_ -> continueToMatch >> return False)
                else Nothing
      allbuttons = case mcont of
                     Nothing -> quitbutton : titlebutton : teambuttons
                     Just cn -> cn : quitbutton : titlebutton : teambuttons
  return allbuttons

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

mutLoop :: MenuBlock [Button (MenuBlock Bool)] -> MenuBlock ()
mutLoop f = do
  liftIO $ SDL.delay 40
  btns <- f
  (_, tex) <- getFontAndTexture
  liftIO $ drawGenScene tex btns
  evts <- liftIO $ pollAllSDLEvents
  back <- checkGenButtonClicks btns evts
  let escpressed = isJust $ specificKeyPressed [SDLK_ESCAPE] evts
  if back || escpressed
    then return ()
    else mutLoop f

genLoop :: [Button (MenuBlock Bool)] -> MenuBlock ()
genLoop btns = mutLoop (return btns)

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
  evalStateT (genLoop buttons) (WorldContext rc allteams Nothing Nothing)

