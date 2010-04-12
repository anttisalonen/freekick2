module Match
where

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Swos
import Tree

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

playMatch :: Font -> (SWOSTeam, TeamOwner) -> (SWOSTeam, TeamOwner) -> IO ()
playMatch _ _ _ = putStrLn "Match played! Yay!"

