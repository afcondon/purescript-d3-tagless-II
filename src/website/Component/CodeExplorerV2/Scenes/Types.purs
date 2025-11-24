-- | Scene types and configuration for CodeExplorerV2
module Component.CodeExplorerV2.Scenes.Types where

import Prelude

import D3.Viz.Spago.Files (SpagoLink)
import D3.Viz.Spago.Model (SpagoSimNode)
import Data.Maybe (Maybe)
import PSD3.Internal.Simulation.Types (Force)

-- | Scene enumeration
data Scene
  = Orbit        -- Packages outer ring, modules clustered
  | TreeReveal   -- Staggered transition to radial tree
  | ForceGraph   -- Force simulation with graph links

derive instance Eq Scene

instance Show Scene where
  show Orbit = "Orbit"
  show TreeReveal = "TreeReveal"
  show ForceGraph = "ForceGraph"

-- | Link rendering style
data LinkStyle
  = NoLinks
  | TreeBezier
  | GraphStraight

-- | Scene configuration - single source of truth for each scene
type SceneConfig =
  { nodeFilter :: SpagoSimNode -> Boolean
  , linkFilter :: Maybe (SpagoLink -> Boolean)
  , nodeInitializers :: Array (Array SpagoSimNode -> Array SpagoSimNode)
  , forces :: Array (Force SpagoSimNode)
  , linkStyle :: LinkStyle
  , domSync :: Maybe String  -- Selector to sync DOM positions to data before scene
  }
