-- | BubblePack scene - modules as bubble packs with nested declarations
module Component.CodeExplorerV2.Scenes.BubblePack where

import Prelude

import Component.CodeExplorerV2.ForceConfigs as FC
import Component.CodeExplorerV2.Forces (centerStrong, collidePack, chargePack, links)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (isMyProjectModule, unpinAllNodes)
import Data.Maybe (Maybe(..))
import PSD3.Config.Scene as CFG

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

-- | Hybrid BubblePack scene configuration (OLD SYSTEM)
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

-- | NEW: BubblePack scene using immutable configuration
-- | This uses the new PSD3.Config system
sceneConfig :: CFG.SceneConfig
sceneConfig = CFG.scene "BubblePack" [
    FC.centerStrong  -- Strong pull toward origin (strength 0.5)
  , FC.collidePack   -- Collision for packed bubbles (radius fn(d.r + 5), strength 1.0, iterations 3)
  , FC.chargePack    -- Strong charge for separation (strength -200, theta 0.9, distMax 600)
  , FC.links         -- Link force (distance 40)
  ]
  # CFG.withDescription "Bubble pack layout with nested declarations"
  # CFG.withAlpha 0.3  -- Start with moderate energy

-- | Expected parameter values for verification
-- | centerStrong: { x: 0, y: 0, strength: 0.5 }
-- | collidePack:  { radius: fn(d.r + 5), strength: 1.0, iterations: 3 }
-- | chargePack:   { strength: -200, theta: 0.9, distanceMin: 1, distanceMax: 600 }
-- | links:        { distance: 40 }
