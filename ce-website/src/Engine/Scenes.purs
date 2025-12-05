-- | Scene Configurations
-- |
-- | Declarative definitions for each scene using NodeRule system.
-- |
-- | Each scene has three phases:
-- | 1. initRules: Applied before transition starts
-- | 2. layout: Where DumbEngine moves nodes
-- | 3. finalRules: Applied after transition completes
module Engine.Scenes
  ( -- Form scenes (transition to positions, then Static)
    treeFormScene
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
import Engine.Scene (SceneConfig, EngineMode(..), NodeRule, PositionMap)
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
-- Tree Scenes (Form + Run)
-- =============================================================================

-- | Form Tree: Transition to tree positions, then stop (Static)
-- | Tree modules go to tree positions, packages/non-tree stay where they are
treeFormScene :: SceneConfig SimNode
treeFormScene =
  { name: "TreeForm"
  , initRules: [ pinAllRule ]
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
  , cssTransition: Nothing
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
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- | Layout for Form Tree: tree modules to tree positions, others stay put
treeFormLayout :: Array SimNode -> PositionMap
treeFormLayout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isTreeModule node
    then Tuple (show node.id) { x: node.treeX, y: node.treeY }
    else Tuple (show node.id) { x: node.x, y: node.y }

-- | Layout for Tree Run: keep current positions (forces will arrange)
treeRunLayout :: Array SimNode -> PositionMap
treeRunLayout nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes
