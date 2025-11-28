-- | ForceGraph scene - NEW VERSION using PSD3.Config
-- |
-- | This is a parallel implementation using the new immutable force configuration system.
-- | It can be tested alongside the existing ForceGraph scene to verify the new approach works.
module Component.CodeExplorerV2.Scenes.ForceGraphV2 where

import Prelude

import Component.CodeExplorerV2.ForceConfigs as FC
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (isUsedModule, unpinAllNodes)
import Data.Maybe (Maybe(..))
import PSD3.Config.Scene as CFG

-- | ForceGraph scene configuration (OLD SYSTEM - using Force handles)
-- | Kept for compatibility during transition
config :: SceneConfig
config =
  { nodeFilter: isUsedModule  -- Only modules in graph
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers: [ unpinAllNodes ]  -- Let forces take over
  -- NOTE: This still uses the old Force type from Forces.purs
  -- We can't use ForceConfigs directly here yet because SceneConfig expects Force not ForceConfig
  , forces: []  -- Will be populated by old system
  , linkStyle: GraphStraight
  , domSync: Just "g.nodes > g"  -- Sync tree positions to data before force layout
  }

-- | NEW: Force Graph scene using immutable configuration
-- | This uses the new PSD3.Config system
sceneConfig :: CFG.SceneConfig
sceneConfig = CFG.scene "ForceGraph" [
    FC.centerStrong  -- Pull toward origin (strength 0.5)
  , FC.chargeTree    -- Repel tree parents (strength -290, theta 0.8, distMin 9, distMax 300)
  , FC.collide2      -- Collision with large radius (radius +19, strength 0.6)
  , FC.links         -- Link force (distance 40)
  ]
  # CFG.withDescription "Force-directed layout of module graph using tree links"
  # CFG.withAlpha 0.3  -- Start with moderate energy

-- | Expected parameter values for verification
-- | centerStrong: { x: 0, y: 0, strength: 0.5 }
-- | chargeTree:   { strength: -290, theta: 0.8, distanceMin: 9, distanceMax: 300 }
-- | collide2:     { radius: fn(d.r + 19), strength: 0.6 }
-- | links:        { distance: 40 }
