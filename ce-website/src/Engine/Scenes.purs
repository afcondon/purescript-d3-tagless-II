-- | Scene Configurations
-- |
-- | Declarative definitions for each scene using NodeRule system.
-- |
-- | Each scene has three phases:
-- | 1. initRules: Applied before transition starts
-- | 2. layout: Where DumbEngine moves nodes
-- | 3. finalRules: Applied after transition completes
module Engine.Scenes
  ( gridScene
  , tree1Scene
  , tree2Scene
  , tree3Scene
  , tree4Scene
  , tree5Scene
  -- Rule helpers
  , isPackage
  , isModule
  , isTreeModule
  , isNonTreeModule
  , pinAtCurrent
  , pinAtOrbit
  , unpinNode
  , setGridXYToPackageOrbit
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Engine.Scene (SceneConfig, EngineMode(..), NodeRule, PositionMap)
import Types (SimNode, NodeType(..))
import Viz.SpagoGridTest.GridLayout as GridLayout
import Viz.SpagoGridTest.OrbitLayout as OrbitLayout
import Viz.SpagoGridTest.TreeLayout as TreeLayout

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

-- | Pin a node at its orbit position
pinAtOrbit :: SimNode -> SimNode
pinAtOrbit n =
  let px = cos n.orbitAngle * OrbitLayout.orbitRadius
      py = sin n.orbitAngle * OrbitLayout.orbitRadius
  in n
    { fx = Nullable.notNull px
    , fy = Nullable.notNull py
    , gridX = px
    , gridY = py
    }

-- | Unpin a node (clear fx/fy)
unpinNode :: SimNode -> SimNode
unpinNode n = n
  { fx = Nullable.null
  , fy = Nullable.null
  }

-- | Set gridX/gridY to parent package's orbit position
-- | (Requires packageOrbitMap to be built externally)
setGridXYToPackageOrbit :: Object.Object { x :: Number, y :: Number } -> SimNode -> SimNode
setGridXYToPackageOrbit pkgMap n =
  case Object.lookup (show n.cluster) pkgMap of
    Just { x: px, y: py } -> n { gridX = px, gridY = py }
    Nothing -> n

-- =============================================================================
-- Common Rule Sets
-- =============================================================================

-- | Standard init: pin all nodes at current positions
pinAllRule :: NodeRule
pinAllRule =
  { name: "pinAll"
  , select: const true
  , apply: pinAtCurrent
  }

-- =============================================================================
-- Grid Scene
-- =============================================================================

gridScene :: SceneConfig
gridScene =
  { name: "Grid"
  , initRules: [ pinAllRule ]
  , layout: GridLayout.calculateGridPositions
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
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- =============================================================================
-- Tree1 Scene (Transition to Orbit)
-- =============================================================================

-- | Tree1: Packages to orbit ring, modules cluster around their package
tree1Scene :: SceneConfig
tree1Scene =
  { name: "Tree1"
  , initRules: [ pinAllRule ]
  , layout: tree1Layout
  , finalRules: tree1FinalRules
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- | Layout: All nodes to their package's orbit position
-- | (Modules and packages at same spot - forces will spread modules)
tree1Layout :: Array SimNode -> PositionMap
tree1Layout nodes =
  let
    -- Build package orbit positions
    packages = Array.filter isPackage nodes
    packagePositions = Object.fromFoldable $ map getPackageOrbitPos packages

    getPackageOrbitPos pkg =
      let px = cos pkg.orbitAngle * OrbitLayout.orbitRadius
          py = sin pkg.orbitAngle * OrbitLayout.orbitRadius
      in Tuple (show pkg.id) { x: px, y: py }

    -- All nodes go to their package's orbit position
    allPositions = map (getNodeOrbitPos packagePositions) nodes

    getNodeOrbitPos pkgMap node = case node.nodeType of
      PackageNode ->
        let px = cos node.orbitAngle * OrbitLayout.orbitRadius
            py = sin node.orbitAngle * OrbitLayout.orbitRadius
        in Tuple (show node.id) { x: px, y: py }
      ModuleNode ->
        case Object.lookup (show node.cluster) pkgMap of
          Just pos -> Tuple (show node.id) pos
          Nothing -> Tuple (show node.id) { x: node.x, y: node.y }
  in
    Object.fromFoldable allPositions

-- | Final rules for Tree1: packages pinned, modules unpinned with gridXY at package orbit
-- | Takes all nodes to build package orbit map for context
tree1FinalRules :: Array SimNode -> Array NodeRule
tree1FinalRules nodes =
  let
    -- Build package ID -> orbit position map
    packages = Array.filter isPackage nodes
    pkgOrbitMap = Object.fromFoldable $ map getPackageOrbitPos packages

    getPackageOrbitPos pkg =
      let px = cos pkg.orbitAngle * OrbitLayout.orbitRadius
          py = sin pkg.orbitAngle * OrbitLayout.orbitRadius
      in Tuple (show pkg.id) { x: px, y: py }
  in
    [ { name: "pinPackagesAtOrbit"
      , select: isPackage
      , apply: pinAtOrbit
      }
    , { name: "unpinModulesSetGridToOrbit"
      , select: isModule
      , apply: \n ->
          -- Look up package's orbit position by cluster ID
          case Object.lookup (show n.cluster) pkgOrbitMap of
            Just { x: px, y: py } -> n
              { fx = Nullable.null
              , fy = Nullable.null
              , gridX = px
              , gridY = py
              }
            Nothing -> unpinNode n
      }
    ]

-- =============================================================================
-- Tree2 Scene (Tree Setup - move tree modules to tree positions)
-- =============================================================================

-- | Tree2: Tree modules transition to their tree positions
-- | Packages and non-tree modules stay where they are (will fade via CSS)
-- | Final state: tree modules pinned at tree positions, static mode
tree2Scene :: SceneConfig
tree2Scene =
  { name: "Tree2"
  , initRules: [ pinAllRule ]
  , layout: tree2Layout
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
        , apply: pinAtCurrent  -- Keep packages/non-tree where they are
        }
      ]
  , stableMode: Static
  , cssTransition: Nothing  -- TODO: CSS fade for packages/non-tree
  }

-- | Layout for Tree2: tree modules go to tree positions, others stay put
tree2Layout :: Array SimNode -> PositionMap
tree2Layout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isTreeModule node
    then Tuple (show node.id) { x: node.treeX, y: node.treeY }
    else Tuple (show node.id) { x: node.x, y: node.y }  -- Stay at current position

-- =============================================================================
-- Tree3 Scene (Transition to Tree Layout)
-- =============================================================================

tree3Scene :: SceneConfig
tree3Scene =
  { name: "Tree3"
  , initRules: [ pinAllRule ]
  , layout: TreeLayout.calculateTreePositions
  , finalRules: \_ ->
      [ { name: "pinModulesAtTree"
        , select: isModule
        , apply: \n -> n
            { fx = Nullable.notNull n.treeX
            , fy = Nullable.notNull n.treeY
            }
        }
      ]
  , stableMode: Static
  , cssTransition: Nothing
  }

-- =============================================================================
-- Tree4 Scene (Force-Directed Tree)
-- =============================================================================

-- | Tree4: Force-directed tree layout
-- | Tree modules are unpinned and connected by link forces
-- | Packages and non-tree modules stay faded (via CSS)
tree4Scene :: SceneConfig
tree4Scene =
  { name: "Tree4"
  , initRules: [ pinAllRule ]
  , layout: tree4Layout  -- Keep current positions, forces will move them
  , finalRules: \_ ->
      [ { name: "unpinTreeModules"
        , select: isTreeModule
        , apply: unpinNode  -- Let link forces move them
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent  -- Keep packages/non-tree where they are
        }
      ]
  , stableMode: Physics {}  -- Forces will run
  , cssTransition: Nothing
  }

-- | Layout for Tree4: keep current positions (forces will arrange)
tree4Layout :: Array SimNode -> PositionMap
tree4Layout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node = Tuple (show node.id) { x: node.x, y: node.y }

-- =============================================================================
-- Tree5 Scene (Force-Directed Tree)
-- =============================================================================

tree5Scene :: SceneConfig
tree5Scene =
  { name: "Tree5"
  , initRules: [ pinAllRule ]
  , layout: TreeLayout.calculateTreePositions
  , finalRules: \_ ->
      [ { name: "unpinAll"
        , select: const true
        , apply: unpinNode
        }
      ]
  , stableMode: Physics {}
  , cssTransition: Nothing
  }
