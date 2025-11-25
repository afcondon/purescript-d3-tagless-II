-- | Tree scene - radial tree with pinned positions
module Component.CodeExplorerV2.Scenes.Tree where

import Prelude

import Component.CodeExplorerV2.Forces (collision, charge2)
import Component.CodeExplorerV2.Scenes.Types (LinkStyle(..), SceneConfig)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (SpagoSimNode, isUsedModule, treeDepthMultiplier)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (notNull, null, toMaybe)
import Data.Number (cos, sin)

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

-- | Tree scene configuration
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
