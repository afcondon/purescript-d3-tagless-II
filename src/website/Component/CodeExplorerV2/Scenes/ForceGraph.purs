-- | ForceGraph scene - force simulation with graph links
module Component.CodeExplorerV2.Scenes.ForceGraph where

import Prelude

import Component.CodeExplorerV2.Forces (charge, collision, links, center)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (isUsedModule, unpinAllNodes)
import Data.Maybe (Maybe(..))

-- | ForceGraph scene configuration
config :: SceneConfig
config =
  { nodeFilter: isUsedModule  -- Only modules in graph
  , linkFilter: Just \l -> l.linktype == M2M_Graph
  , nodeInitializers: [ unpinAllNodes ]  -- Let forces take over
  , forces: [ charge, collision, links, center ]
  , linkStyle: GraphStraight
  , domSync: Just "g.nodes > g"  -- Sync tree positions to data before force layout
  }
