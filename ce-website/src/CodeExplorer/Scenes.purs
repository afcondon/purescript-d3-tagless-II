-- | Scene Configurations
-- |
-- | Declarative definitions for each scene using NodeRule system.
-- |
-- | Each scene has three phases:
-- | 1. initRules: Applied before transition starts
-- | 2. layout: Where DumbEngine moves nodes
-- | 3. finalRules: Applied after transition completes
module CodeExplorer.Scenes
  ( -- Form scenes (transition to positions, then Static)
    treemapFormScene
  , treeFormScene
  , radialTreeFormScene
  , topoFormScene
  -- Run scenes (enable physics/forces)
  , treeRunScene
  -- Rule helpers
  , isPackage
  , isModule
  , isTreeModule
  , isNonTreeModule
  , pinAtCurrent
  , unpinNode
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import CodeExplorer.Scene (SceneConfig, EngineMode(..), NodeRule, PositionMap)
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Node Predicates (Selectors)
-- =============================================================================

isPackage :: SimNode -> Boolean
isPackage n = n.nodeType == PackageNode

isModule :: SimNode -> Boolean
isModule n = n.nodeType == ModuleNode

-- | Is this module part of the spanning tree (reachable from root)?
isTreeModule :: SimNode -> Boolean
isTreeModule n = n.nodeType == ModuleNode && n.isInTree

-- | Is this module NOT part of the spanning tree?
isNonTreeModule :: SimNode -> Boolean
isNonTreeModule n = n.nodeType == ModuleNode && not n.isInTree

-- =============================================================================
-- Node Transforms
-- =============================================================================

-- | Pin a node at its current position
pinAtCurrent :: SimNode -> SimNode
pinAtCurrent n = n
  { fx = Nullable.notNull n.x
  , fy = Nullable.notNull n.y
  }

-- | Unpin a node (clear fx/fy)
unpinNode :: SimNode -> SimNode
unpinNode n = n
  { fx = Nullable.null
  , fy = Nullable.null
  }

-- =============================================================================
-- Common Rule Sets
-- =============================================================================

-- | Standard init: pin all nodes at current positions
pinAllRule :: NodeRule SimNode
pinAllRule =
  { name: "pinAll"
  , select: const true
  , apply: pinAtCurrent
  }

-- =============================================================================
-- Tree Root Position (for "grow from root" animation)
-- =============================================================================

-- | Root position for vertical tree layout
-- | This matches the calculation in Data/Loader.purs:
-- | Root at treeY=0 maps to: (0 / 1000 - 0.5) * 1400 = -700
treeRootX :: Number
treeRootX = 0.0

treeRootY :: Number
treeRootY = -700.0

-- | Move a tree module to the root position (for "grow from root" animation)
moveToTreeRoot :: SimNode -> SimNode
moveToTreeRoot n = n
  { x = treeRootX
  , y = treeRootY
  , fx = Nullable.notNull treeRootX
  , fy = Nullable.notNull treeRootY
  }

-- =============================================================================
-- Treemap Scene
-- =============================================================================

-- | Form Treemap: Transition back to grid positions, then run physics
-- | All nodes return to their treemap/grid positions (gridX, gridY)
-- | Uses Physics mode so collide forces can spread modules apart
treemapFormScene :: SceneConfig SimNode
treemapFormScene =
  { name: "TreemapForm"
  , initRules: [ pinAllRule ]
  , layout: treemapFormLayout
  , finalRules: \_ ->
      [ { name: "pinPackagesAtGrid"
        , select: isPackage
        , apply: \n -> n
            { fx = Nullable.notNull n.gridX
            , fy = Nullable.notNull n.gridY
            }
        }
      , { name: "unpinModules"
        , select: isModule
        , apply: unpinNode
        }
      ]
  , stableMode: Physics
  }

-- | Layout for Form Treemap: all nodes to their grid positions
treemapFormLayout :: Array SimNode -> PositionMap
treemapFormLayout nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.gridX, y: n.gridY }) nodes

-- =============================================================================
-- Tree Scenes (Form + Run)
-- =============================================================================

-- | Form Tree: Transition to tree positions, then stop (Static)
-- | Tree modules start at root and animate outward ("grow from root")
-- | Packages/non-tree stay where they are
treeFormScene :: SceneConfig SimNode
treeFormScene =
  { name: "TreeForm"
  , initRules:
      [ { name: "moveTreeModulesToRoot"
        , select: isTreeModule
        , apply: moveToTreeRoot
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , layout: treeFormLayout
  , finalRules: \_ ->
      [ { name: "pinTreeModulesAtTree"
        , select: isTreeModule
        , apply: \n -> n
            { fx = Nullable.notNull n.treeX
            , fy = Nullable.notNull n.treeY
            }
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Static
  }

-- | Tree Run: Force-directed tree with link forces
-- | Tree modules are unpinned and connected by link forces
treeRunScene :: SceneConfig SimNode
treeRunScene =
  { name: "TreeRun"
  , initRules: [ pinAllRule ]
  , layout: treeRunLayout
  , finalRules: \_ ->
      [ { name: "unpinTreeModules"
        , select: isTreeModule
        , apply: unpinNode
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Physics
  }

-- | Layout for Form Tree: tree modules to tree positions, others stay put
treeFormLayout :: Array SimNode -> PositionMap
treeFormLayout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isTreeModule node then Tuple (show node.id) { x: node.treeX, y: node.treeY }
    else Tuple (show node.id) { x: node.x, y: node.y }

-- | Layout for Tree Run: keep current positions (forces will arrange)
treeRunLayout :: Array SimNode -> PositionMap
treeRunLayout nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes

-- =============================================================================
-- Radial Tree Scene (waypoint before Force)
-- =============================================================================

-- | Form Radial Tree: Transition to radial tree positions, then stop (Static)
-- | Used as a waypoint before entering Force view to prevent chaotic initial movement.
-- | Tree modules go to radial positions, packages/non-tree stay where they are.
radialTreeFormScene :: SceneConfig SimNode
radialTreeFormScene =
  { name: "RadialTreeForm"
  , initRules: [ pinAllRule ]
  , layout: radialTreeFormLayout
  , finalRules: \_ ->
      [ { name: "pinTreeModulesAtRadial"
        , select: isTreeModule
        , apply: \n -> n
            { fx = Nullable.notNull n.radialX
            , fy = Nullable.notNull n.radialY
            }
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Static
  }

-- | Layout for Radial Tree Form: tree modules to radial positions, others stay put
radialTreeFormLayout :: Array SimNode -> PositionMap
radialTreeFormLayout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isTreeModule node then Tuple (show node.id) { x: node.radialX, y: node.radialY }
    else Tuple (show node.id) { x: node.x, y: node.y }

-- =============================================================================
-- Topo Scene (Package DAG)
-- =============================================================================

-- | Form Topo: Transition packages to topological positions
-- | Packages go to topo positions (layer-based DAG), modules stay where they are
-- | Uses Static mode since packages are pinned in final positions
topoFormScene :: SceneConfig SimNode
topoFormScene =
  { name: "TopoForm"
  , initRules: [ pinAllRule ]
  , layout: topoFormLayout
  , finalRules: \_ ->
      [ { name: "pinPackagesAtTopo"
        , select: isPackage
        , apply: \n -> n
            { fx = Nullable.notNull n.topoX
            , fy = Nullable.notNull n.topoY
            }
        }
      , { name: "pinModulesAtCurrent"
        , select: isModule
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Static
  }

-- | Layout for Form Topo: packages to topo positions, modules stay put
topoFormLayout :: Array SimNode -> PositionMap
topoFormLayout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isPackage node then Tuple (show node.id) { x: node.topoX, y: node.topoY }
    else Tuple (show node.id) { x: node.x, y: node.y }
