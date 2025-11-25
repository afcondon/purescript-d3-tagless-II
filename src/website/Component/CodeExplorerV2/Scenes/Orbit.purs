-- | Orbit scene - initial view with packages in orbit, modules clustered
module Component.CodeExplorerV2.Scenes.Orbit where

import Prelude

import Component.CodeExplorerV2.Forces (collision, clusterX, clusterY)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Model (isPackage, nodesToCircle, pinMainAtXY)
import Data.Maybe (Maybe(..))

-- | Orbit scene configuration
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
