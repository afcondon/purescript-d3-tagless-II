-- | BubblePack scene - modules as bubble packs with nested declarations
module Component.CodeExplorerV2.Scenes.BubblePack where

import Prelude

import Component.CodeExplorerV2.Forces (centerStrong, collidePack, chargePack, links)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (isMyProjectModule, unpinAllNodes)
import Data.Maybe (Maybe(..))

-- | BubblePack scene configuration
-- | Uses tree links between modules, force simulation for layout
-- | Each module node will be rendered as a bubble pack of its declarations
config :: SceneConfig
config =
  { nodeFilter: isMyProjectModule  -- Only my-project modules
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers: [ unpinAllNodes ]
  , forces: [ centerStrong, collidePack, chargePack, links ]
  , linkStyle: GraphStraight
  , domSync: Just "g.nodes > g"  -- Sync positions from ForceGraph scene
  }

-- | Hybrid BubblePack scene configuration
-- | Shows ALL nodes, but only packs my-project modules
-- | Spago package nodes remain as regular force-directed nodes
configHybrid :: SceneConfig
configHybrid =
  { nodeFilter: \_ -> true  -- Keep all nodes
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers: [ unpinAllNodes ]
  , forces: [ centerStrong, collidePack, chargePack, links ]
  , linkStyle: GraphStraight
  , domSync: Just "g.nodes > g"  -- Sync positions from ForceGraph scene
  }
