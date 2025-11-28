-- | Tree scene - radial tree with pinned positions
module Component.CodeExplorerV2.Scenes.Tree where

import Prelude

import Component.CodeExplorerV2.ForceConfigs as FC
import Component.CodeExplorerV2.Forces (collision, charge2)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (SpagoSimNode, isUsedModule, treeDepthMultiplier)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (notNull, null, toMaybe)
import Data.Number (cos, sin)
import PSD3.Config.Scene as CFG

-- | Node initializer: Clear gridXY to disable cluster forces
clearGridXY :: Array SpagoSimNode -> Array SpagoSimNode
clearGridXY = map \node -> node { gridXY = null }

-- | Node initializer: Pin nodes at their treeXY positions (fx/fy)
pinAtTree :: Array SpagoSimNode -> Array SpagoSimNode
pinAtTree = map \node ->
  case toMaybe node.treeXY of
    Just treeXY ->
      let angle = treeXY.x
          radius = treeXY.y * treeDepthMultiplier
          x = radius * cos angle
          y = radius * sin angle
      in node { x = x, y = y, fx = notNull x, fy = notNull y }
    Nothing ->
      node

-- | Tree scene configuration (OLD SYSTEM - using Force handles)
-- | Kept for compatibility during transition
-- | Nodes pinned at radial tree positions, colored by package
config :: SceneConfig
config =
  { nodeFilter: isUsedModule  -- Only modules in tree
  , linkFilter: Just \l -> l.linktype == M2M_Tree
  , nodeInitializers:
      [ clearGridXY   -- Remove gridXY so cluster forces don't pull
      , pinAtTree     -- Pin at treeXY positions
      ]
  , forces: [ collision, charge2 ]  -- Collision + weak charge for stability
  , linkStyle: TreeBezier
  , domSync: Just ".nodes"  -- Sync DOM positions before applying initializers
  }

-- | NEW: Tree scene using immutable configuration
-- | This uses the new PSD3.Config system
sceneConfig :: CFG.SceneConfig
sceneConfig = CFG.scene "Tree" [
    FC.collision  -- Prevent node overlap (radius fn(d.r + 2), strength 1.0)
  , FC.charge2    -- Weak charge for stability (strength -100, theta 0.9, distMax 400)
  ]
  # CFG.withDescription "Radial tree layout with pinned positions"
  # CFG.withAlpha 0.3  -- Start with moderate energy

-- | Expected parameter values for verification
-- | collision: { radius: fn(d.r + 2), strength: 1.0, iterations: 1 }
-- | charge2:   { strength: -100, theta: 0.9, distanceMin: 1, distanceMax: 400 }
