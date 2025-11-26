-- | Orbit scene - initial view with packages in orbit, modules clustered
module Component.CodeExplorerV2.Scenes.Orbit where

import Prelude

import Component.CodeExplorerV2.ForceConfigs as FC
import Component.CodeExplorerV2.Forces (collision, clusterX, clusterY)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Model (isPackage, nodesToCircle, pinMainAtXY)
import Data.Maybe (Maybe(..))
import PSD3.Config.Scene as CFG

-- | Orbit scene configuration (OLD SYSTEM - using Force handles)
-- | Kept for compatibility during transition
config :: SceneConfig
config =
  { nodeFilter: const true  -- All nodes visible
  , linkFilter: Nothing     -- No links in orbit view
  , nodeInitializers:
      [ nodesToCircle isPackage 1600.0  -- Packages spread on circle, modules get gridXY from packages
      , pinMainAtXY 0.0 0.0              -- Main at center
      ]
  , forces: [ collision, clusterX, clusterY ]
  , linkStyle: NoLinks
  , domSync: Nothing  -- Initial scene, no DOM to sync
  }

-- | NEW: Orbit scene using immutable configuration
-- | This uses the new PSD3.Config system
sceneConfig :: CFG.SceneConfig
sceneConfig = CFG.scene "Orbit" [
    FC.collision  -- Prevent node overlap (radius fn(d.r + 2), strength 1.0)
  , FC.clusterX   -- Pull modules toward package X (strength 0.2, modulesOnly)
  , FC.clusterY   -- Pull modules toward package Y (strength 0.2, modulesOnly)
  ]
  # CFG.withDescription "Initial orbit view with packages spread in circle, modules clustered"
  # CFG.withAlpha 0.3  -- Start with moderate energy

-- | Expected parameter values for verification
-- | collision: { radius: fn(d.r + 2), strength: 1.0, iterations: 1 }
-- | clusterX:  { x: fn(gridPointX(d)), strength: 0.2, modulesOnly filter }
-- | clusterY:  { y: fn(gridPointY(d)), strength: 0.2, modulesOnly filter }
