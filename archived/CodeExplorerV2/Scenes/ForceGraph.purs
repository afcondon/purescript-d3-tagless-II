-- | ForceGraph scene - force simulation with graph links
module Component.CodeExplorerV2.Scenes.ForceGraph where

import Prelude

import Component.CodeExplorerV2.Forces (centerStrong, collide2, chargeTree, links)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (isUsedModule, unpinAllNodes)
import Data.Maybe (Maybe(..))

-- | ForceGraph scene configuration
-- | Uses tree links (spanning tree) not graph links (all dependencies)
-- | Forces tuned for balanced layout: centerStrong, chargeTree, collide2, links
config :: SceneConfig
config =
  { nodeFilter: isUsedModule  -- Only modules in graph
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers: [ unpinAllNodes ]  -- Let forces take over
  , forces: [ centerStrong, chargeTree, collide2, links ]
  , linkStyle: GraphStraight
  , domSync: Just "g.nodes > g"  -- Sync tree positions to data before force layout
  }
